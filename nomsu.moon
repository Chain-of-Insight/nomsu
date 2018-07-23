#!/usr/bin/env moon
-- This file contains the command-line Nomsu runner.
if NOMSU_VERSION and NOMSU_PREFIX
    ver_bits = [ver_bit for ver_bit in NOMSU_VERSION\gmatch("[0-9]+")]
    partial_vers = [table.concat(ver_bits,'.',1,i) for i=#ver_bits,1,-1]
    package.path = table.concat(["#{NOMSU_PREFIX}/share/nomsu/#{v}/?.lua" for v in *partial_vers],";")..";"..package.path
    package.cpath = table.concat(["#{NOMSU_PREFIX}/lib/nomsu/#{v}/?.so" for v in *partial_vers],";")..";"..package.cpath
    package.nomsupath = table.concat(["#{NOMSU_PREFIX}/share/nomsu/#{v}" for v in *partial_vers],";")

EXIT_SUCCESS, EXIT_FAILURE = 0, 1
usage = [=[
Nomsu Compiler

Usage: (nomsu | lua nomsu.lua | moon nomsu.moon) [-V version] [-O] [-v] [-c] [-s] [-t] [-I file] [--help | -h] [--version] [--no-core] [file [nomsu args...]]

OPTIONS
    -O Run the compiler in optimized mode (use precompiled .lua versions of Nomsu files, when available).
    -v Verbose: print compiled lua code.
    -c Compile the input files into a .lua files.
    -e Execute the specified string.
    -s Check the input files for syntax errors.
    -I <file> Add an additional input file or directory.
    -d <debugger> Attempt to use the specified debugger to wrap the main body of execution.
    -h/--help Print this message.
    --version Print the version number and exit.
    --no-core Skip loading the Nomsu core by default.
    -V specify which Nomsu version is desired.
    <file> The Nomsu file to run (can be "-" to use stdin).
]=]

ok, _ = pcall ->
    export lpeg, re
    lpeg = require 'lpeg'
    re = require 're'
if not ok
    print("Error: unable to find the 'lpeg' Lua module. Please install LPEG either from http://www.inf.puc-rio.br/~roberto/lpeg/re.html or, if you use luarocks: `luarocks install lpeg`")
    os.exit(EXIT_FAILURE)
Files = require "files"
Errhand = require "error_handling"
NomsuCompiler = require "nomsu_compiler"
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
{:repr} = require "utils"

-- If this file was reached via require(), then just return the Nomsu compiler
if not arg or debug.getinfo(2).func == require
    return NomsuCompiler

file_queue = {}
sep = "\0"
parser = re.compile([[
    args <- {| (flag %sep)* (({~ file ~} -> add_file) %sep)? {:nomsu_args: {| ({(!%sep .)*} %sep)* |} :} %sep? |} !.
    flag <-
        {:optimized: ("-O" -> true) :}
      / ("-I" %sep? ({~ file ~} -> add_file))
      / ("-e" %sep? (({} {~ file ~}) -> add_exec_string))
      / ({:check_syntax: ("-s" -> true):})
      / ({:compile: ("-c" -> true):})
      / ({:compile: ("-c" -> true):})
      / {:verbose: ("-v" -> true) :}
      / {:help: (("-h" / "--help") -> true) :}
      / {:version: ("--version" -> true) :}
      / {:no_core: ("--no-core" -> true) :}
      / {:debugger: ("-d" %sep? {(!%sep .)*}) :}
      / {:requested_version: "-V" (%sep? {([0-9.])+})? :}
    file <- ("-" -> "stdin") / {(!%sep .)+}
]], {
    true: -> true
    sep: lpeg.P(sep)
    add_file: (f)-> table.insert(file_queue, f)
    add_exec_string: (pos, s)->
        name = "command line arg @#{pos}.nom"
        Files.spoof(name, s)
        table.insert(file_queue, name)
})
arg_string = table.concat(arg, sep)..sep
args = parser\match(arg_string)
if not args or args.help
    print usage
    os.exit(EXIT_FAILURE)

nomsu = NomsuCompiler
nomsu.arg = NomsuCompiler.list(args.nomsu_args)

if args.version
    nomsu\run [[
use "core"
say (Nomsu version)]]
    os.exit(EXIT_SUCCESS)

export FILE_CACHE
-- FILE_CACHE is a map from filename (string) -> string of file contents
FILE_CACHE = setmetatable {}, {
    __index: (filename)=>
        file = io.open(filename)
        return nil unless file
        contents = file\read("*a")
        file\close!
        self[filename] = contents
        return contents
}

run = ->
    input_files = {}
    for f in *file_queue
        unless Files.exists(f)
            error("Could not find: '#{f}'")
        for _,filename in Files.walk(f)
            input_files[filename] = true

    nomsu.can_optimize = (f)->
        return false unless args.optimized
        return false if args.compile and input_files[f]
        return true

    unless args.no_core
        for _,filename in Files.walk('core')
            if filename\match "%.nom$"
                nomsu\run_file filename

    get_file_and_source = (filename)->
        local file, source
        if filename == 'stdin'
            file = io.read("*a")
            Files.spoof('stdin', file)
            source = Source('stdin', 1, #file)
        elseif filename\match("%.nom$")
            file = Files.read(filename)
            if not file
                error("File does not exist: #{filename}", 0)
            source = Source(filename, 1, #file)
        else return nil
        source = Source(filename,1,#file)
        return file, source

    run_file = (filename, lua_handler=nil)->
        file, source = get_file_and_source(filename)
        return unless file
        tree = nomsu\parse(file, source)
        if tree
            if tree.type != "FileChunks"
                tree = {tree}
            -- Each chunk's compilation is affected by the code in the previous chunks
            -- (typically), so each chunk needs to compile and run before the next one
            -- compiles.
            for chunk in *tree
                lua = nomsu\compile(chunk)\as_statements("return ")
                lua\declare_locals!
                lua\prepend "-- File: #{source.filename\gsub("\n.*", "...")}\n"
                if lua_handler and input_files[filename] then lua_handler(tostring(lua))
                nomsu\run_lua(lua)

    parse_errs = {}
    for f in *file_queue
        for _,filename in Files.walk(f)
            continue unless filename == "stdin" or filename\match("%.nom$")
            if args.check_syntax
                -- Check syntax
                file, source = get_file_and_source(filename)
                continue unless file
                nomsu\parse(file, source)
                print("Parse succeeded: #{filename}")

            if args.compile
                -- Compile .nom files into .lua
                output = if filename == 'stdin' then io.output()
                else io.open(filename\gsub("%.nom$", ".lua"), "w")
                run_file filename, (lua)->
                    output\write(tostring(lua), "\n")
                    if args.verbose then print(tostring(lua))
                print ("Compiled %-25s -> %s")\format(filename, filename\gsub("%.nom$", ".lua"))
                output\close!

            if not args.check_syntax and not args.compile
                -- Just run the file
                run_file filename, (args.verbose and print or nil)

    if #file_queue == 0
        -- Run in interactive mode (REPL)
        nomsu\run [[
#!/usr/bin/env nomsu -V2
use "lib/consolecolor.nom"
action [quit, exit]: lua> "os.exit(0)"
action [help]:
    say ".."
        This is the Nomsu v\(Nomsu version) interactive console.
        You can type in Nomsu code here and hit 'enter' twice to run it.
        To exit, type 'exit' or 'quit' and hit enter twice.

say ".."

    \(bright)\(underscore)Welcome to the Nomsu v\(Nomsu version) interactive console!\(reset color)
        press 'enter' twice to run a command
    \("")]]
        for repl_line=1,math.huge
            io.write(colored.bright colored.yellow ">> ")
            buff = {}
            while true
                io.write(colors.bright)
                line = io.read("*L")
                io.write(colors.reset)
                if line == "\n" or not line
                    if #buff > 0
                        io.write("\027[1A\027[2K")
                    break -- Run buffer
                line = line\gsub("\t", "    ")
                table.insert buff, line
                io.write(colored.dim colored.yellow ".. ")
            if #buff == 0
                break -- Exit
            
            buff = table.concat(buff)
            pseudo_filename = "user input #"..repl_line
            Files.spoof(pseudo_filename, buff)
            err_hand = (error_message)->
                Errhand.print_error error_message
            ok, ret = xpcall(nomsu.run, err_hand, nomsu, buff, Source(pseudo_filename, 1, #buff))
            if ok and ret != nil
                print "= "..repr(ret)
            elseif not ok
                Errhand.print_error ret

debugger = if args.debugger == "nil" then {}
else require(args.debugger or 'error_handling')
guard = if type(debugger) == 'function' then debugger
else debugger.guard or debugger.call or debugger.wrap or debugger.run or ((fn)->fn())
guard(run)
