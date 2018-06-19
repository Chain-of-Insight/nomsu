#!/usr/bin/env moon
-- This file contains the command-line Nomsu runner.
usage = [=[
Nomsu Compiler

Usage: (lua nomsu.lua | moon nomsu.moon) [-i] [-O] [-v] [-c] [-f] [-s] [--help] [-p print_file] file1 file2... [-- nomsu args...]

OPTIONS
    -i Run the compiler in interactive mode (REPL)
    -O Run the compiler in optimized mode (use precompiled .lua versions of Nomsu files, when available)
    -v Verbose: print compiled lua code
    -c Compile .nom files into .lua files
    -f Auto-format the given Nomsu file and print the result.
    -s Check the program for syntax errors.
    -h/--help Print this message.
    -p <file> Print to the specified file instead of stdout.
    <input> Input file can be "-" to use stdin.
]=]

lpeg = require 'lpeg'
re = require 're'
Errhand = require "error_handling"
NomsuCompiler = require "nomsu_compiler"
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
STDIN, STDOUT, STDERR = "/dev/fd/0", "/dev/fd/1", "/dev/fd/2"

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
    file <- "-" / [^;]+
]], {true: -> true})
args = table.concat(arg, ";")..";"
args = parser\match(args)
if not args or args.help
    print usage
    os.exit!

nomsu = NomsuCompiler
nomsu.arg = args.nomsu_args

run = ->
    for i,input in ipairs args.inputs
        if input == "-" then args.inputs[i] = STDIN

    if #args.inputs == 0 and not args.interactive
        args.inputs = {"core"}
        args.interactive = true

    print_file = if args.print_file == "-" then io.stdout
    elseif args.print_file then io.open(args.print_file, 'w')
    else io.stdout

    nomsu.skip_precompiled = not args.optimized
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
        for f in all_files(input)
            input_files[#input_files+1] = f
            to_run[f] = true

    if args.compile or args.verbose
        nomsu.on_compile = (code, from_file)->
            return unless to_run[from_file]
            if args.verbose
                io.write(tostring(code), "\n")
            if args.compile and from_file\match("%.nom$")
                output_filename = from_file\gsub("%.nom$", ".lua")
                output_file = io.open(output_filename, 'w')
                output_file\write(tostring(code))
                output_file\flush!
                print ("Compiled %-25s -> %s")\format(from_file, output_filename)
                output_file\close!

    parse_errs = {}
    for filename in *input_files
        if args.syntax
            -- Check syntax:
            file_contents = io.open(filename)\read('*a')
            ok,err = pcall nomsu.parse, nomsu, file_contents, Source(filename, 1, #file_contents)
            if not ok
                table.insert parse_errs, err
            elseif print_file
                print_file\write("Parse succeeded: #{filename}\n")
                print_file\flush!
        elseif args.format
            -- Auto-format
            file = FILE_CACHE[filename]
            if not file
                error("File does not exist: #{filename}", 0)
            tree = nomsu\parse(file, Source(filename,1,#file))
            formatted = tostring(nomsu\tree_to_nomsu(tree))
            if print_file
                print_file\write(formatted, "\n")
                print_file\flush!
        elseif filename == STDIN
            file = io.input!\read("*a")
            FILE_CACHE.stdin = file
            nomsu\run(file, Source('stdin',1,#file))
        else
            nomsu\run_file(filename)

    if #parse_errs > 0
        io.stderr\write table.concat(parse_errs, "\n\n")
        io.stderr\flush!
        os.exit(false, true)
    elseif args.syntax
        os.exit(true, true)

    if args.interactive
        -- REPL
        for repl_line=1,math.huge
            io.write(colored.bright colored.yellow ">> ")
            buff = {}
            while true
                line = io.read("*L")
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
            FILE_CACHE[pseudo_filename] = buff
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
