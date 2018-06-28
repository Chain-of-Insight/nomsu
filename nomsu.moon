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

Usage: (lua nomsu.lua | moon nomsu.moon) [-V version] [-i] [-O] [-v] [-c] [-f] [-s] [--help] [--version] [-p print_file] file1 file2... [-- nomsu args...]

OPTIONS
    -i Run the compiler in interactive mode (REPL)
    -O Run the compiler in optimized mode (use precompiled .lua versions of Nomsu files, when available)
    -v Verbose: print compiled lua code
    -c Compile .nom files into .lua files
    -f Auto-format the given Nomsu file and print the result.
    -s Check the program for syntax errors.
    -h/--help Print this message.
    --version Print the version number and exit.
    -V specify which Nomsu version is desired
    -p <file> Print to the specified file instead of stdout.
    <input> Input file can be "-" to use stdin.
]=]

ok, _ = pcall ->
    export lpeg, re
    lpeg = require 'lpeg'
    re = require 're'
if not ok
    print("Error: unable to find the 'lpeg' Lua module. Please install LPEG either from http://www.inf.puc-rio.br/~roberto/lpeg/re.html or, if you use luarocks: `luarocks install lpeg`")
    os.exit(EXIT_FAILURE)
Errhand = require "error_handling"
NomsuCompiler = require "nomsu_compiler"
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
{:repr} = require "utils"

-- If this file was reached via require(), then just return the Nomsu compiler
if not arg or debug.getinfo(2).func == require
    return NomsuCompiler

parser = re.compile([[
    args <- {| (flag ";")* {:inputs: {| ({file} ";")* |} :} {:nomsu_args: {| ("--;" ({[^;]*} ";")*)? |} :} ";"? |} !.
    flag <-
        {:interactive: ("-i" -> true) :}
      / {:optimized: ("-O" -> true) :}
      / {:format: ("-f" -> true) :}
      / {:syntax: ("-s" -> true) :}
      / {:print_file: "-p" ";" {file} :}
      / {:compile: ("-c" -> true) :}
      / {:verbose: ("-v" -> true) :}
      / {:help: (("-h" / "--help") -> true) :}
      / {:version: ("--version" -> true) :}
      / {:requested_version: "-V" ((";")? {([0-9.])+})? :}
    file <- "-" / [^;]+
]], {true: -> true})
arg_string = table.concat(arg, ";")..";"
args = parser\match(arg_string)
if not args or args.help
    print usage
    os.exit(EXIT_FAILURE)

files = require "files"
nomsu = NomsuCompiler
nomsu.arg = args.nomsu_args

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
    for i,input in ipairs args.inputs
        if input == "-" then args.inputs[i] = 'stdin'

    if #args.inputs == 0 and not args.interactive
        args.inputs = {"core"}
        args.interactive = true

    print_file = if args.print_file == "-" then io.stdout
    elseif args.print_file then io.open(args.print_file, 'w')
    else io.stdout

    if print_file == nil
        nomsu.print = ->
    elseif print_file != io.stdout
        nomsu.print = (...)->
            N = select("#",...)
            if N > 0
                print_file\write(tostring(select(1,...)))
                for i=2,N
                    print_file\write('\t',tostring(select(1,...)))
            print_file\write('\n')
            print_file\flush!

    input_files = {}
    to_run = {}
    for input in *args.inputs
        if input == 'stdin'
            input_files[#input_files+1] = 'stdin'
            to_run['stdin'] = true
            continue
        found = false
        for f in files.walk(input)
            input_files[#input_files+1] = f
            to_run[f] = true
            found = true
        if not found
            error("Could not find: #{input}")

    nomsu.can_optimize = (f)->
        return false unless args.optimized
        return false if to_run[f]
        return true

    parse_errs = {}
    for arg in *args.inputs
        for filename in files.walk(arg)
            local file, source
            if filename == 'stdin'
                file = io.read("*a")
                files.spoof('stdin', file)
                source = Source('stdin', 1, #file)
            elseif filename\match("%.nom$")
                file = files.read(filename)
                if not file
                    error("File does not exist: #{filename}", 0)
                source = Source(filename, 1, #file)
            else continue
            source = Source(filename,1,#file)

            output = if args.compile then io.open(filename\gsub("%.nom$", ".lua"), "w") else nil

            if args.syntax
                -- Check syntax:
                ok,err = pcall nomsu.parse, nomsu, file, source
                if not ok
                    table.insert parse_errs, err
                elseif print_file
                    print_file\write("Parse succeeded: #{filename}\n")
                    print_file\flush!
                continue

            tree = nomsu\parse(file, source)

            if args.format
                -- Auto-format
                formatted = tree and tostring(nomsu\tree_to_nomsu(tree)) or ""
                if print_file
                    print_file\write(formatted, "\n")
                    print_file\flush!
                continue

            if tree
                if tree.type == "FileChunks"
                    -- Each chunk's compilation is affected by the code in the previous chunks
                    -- (typically), so each chunk needs to compile and run before the next one
                    -- compiles.
                    for chunk in *tree
                        lua = nomsu\compile(chunk)\as_statements("return ")
                        lua\declare_locals!
                        lua\prepend "-- File: #{source.filename\gsub("\n.*", "...")}\n"
                        if args.compile
                            output\write(tostring(lua), "\n")
                        if args.verbose
                            print(tostring(lua))
                        nomsu\run_lua(lua)
                else
                    lua = nomsu\compile(tree)\as_statements("return ")
                    lua\declare_locals!
                    lua\prepend "-- File: #{source.filename\gsub("\n.*", "...")}\n"
                    if args.compile
                        output\write(tostring(lua), "\n")
                    if args.verbose
                        print(tostring(lua))
                    nomsu\run_lua(lua)
            if args.compile
                print ("Compiled %-25s -> %s")\format(filename, filename\gsub("%.nom$", ".lua"))
                output\close!

    if #parse_errs > 0
        io.stderr\write table.concat(parse_errs, "\n\n")
        io.stderr\flush!
        os.exit(EXIT_FAILURE)
    elseif args.syntax
        os.exit(EXIT_SUCCESS)

    if args.interactive
        -- REPL
        nomsu\run [[
use "core"
use "lib/consolecolor.nom"
action [quit, exit]: lua> "os.exit(0)"
action [help]
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
            files.spoof(pseudo_filename, buff)
            err_hand = (error_message)->
                Errhand.print_error error_message
            ok, ret = xpcall(nomsu.run, err_hand, nomsu, buff, Source(pseudo_filename, 1, #buff))
            if ok and ret != nil
                print "= "..repr(ret)
            elseif not ok
                Errhand.print_error ret

has_ldt, ldt = pcall(require,'ldt')
if has_ldt
    ldt.guard(run)
else
    Errhand.run_safely(run)
