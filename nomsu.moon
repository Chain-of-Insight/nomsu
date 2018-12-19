#!/usr/bin/env moon
-- This file contains the command-line Nomsu runner.

EXIT_SUCCESS, EXIT_FAILURE = 0, 1

if _VERSION == "Lua 5.1" and not jit
    -- Cannot run on Lua5.1 because it doesn't have gotos.
    print("Sorry, Nomsu does not run on Lua 5.1. Please use LuaJIT 2+ or Lua 5.2+")
    os.exit(EXIT_FAILURE)

if NOMSU_VERSION and NOMSU_PREFIX
    ver_bits = [ver_bit for ver_bit in NOMSU_VERSION\gmatch("[0-9]+")]
    partial_vers = [table.concat(ver_bits,'.',1,i) for i=#ver_bits,1,-1]
    package.path = table.concat(["#{NOMSU_PREFIX}/share/nomsu/#{v}/?.lua" for v in *partial_vers],";")..";"..package.path
    package.cpath = table.concat(["#{NOMSU_PREFIX}/lib/nomsu/#{v}/?.so" for v in *partial_vers],";")..";"..package.cpath
    package.nomsupath = table.concat(["#{NOMSU_PREFIX}/share/nomsu/#{v}" for v in *partial_vers],";")..";."
else
    package.nomsupath = "."

usage = [=[
Nomsu Compiler

Usage: (nomsu | lua nomsu.lua | moon nomsu.moon) [-V version] [--help | -h] [--version] [-O optimization level] [-v] [-c] [-s] [-d debugger] [--no-core] [(file | -t tool | -e "nomsu code..." | -m files... [--]) [nomsu args...]]

OPTIONS
    -t <tool> Run a tool.
    -e Execute the specified string.
    -m Run multiple files (all extra arguments).
    -O <level> Run the compiler with the given optimization level (>0: use precompiled .lua versions of Nomsu files, when available).
    -v Verbose: print compiled lua code.
    -c Compile the input files into a .lua files.
    -s Check the input files for syntax errors.
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
--NomsuCompiler = require "nomsu_compiler"
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
--{:Importer, :import_to_1_from, :_1_forked} = require 'importer'
{:List, :Dict, :Text} = require 'containers'
--SyntaxTree = require "syntax_tree"
nomsu_environment = require('nomsu_environment')

-- If this file was reached via require(), then just return the Nomsu compiler
if not arg or debug.getinfo(2).func == require
    return nomsu_environment

sep = "\3"
parser = re.compile([[
    args <- {| (flag %sep)*
        (("-e" %sep {:execute: {[^%sep]+} :} %sep)
         / {:files: {|
            ("-t" %sep {~ {[^%sep]+} -> "nomsu://tools/%1.nom" ~} %sep
             / "-m" %sep (!("--" %sep) {[^%sep]+} %sep)* ("--" %sep)?
             / {[^%sep]+} %sep
             / {~ '' %sep? -> 'nomsu://tools/repl.nom' ~}) |} :})
      {:nomsu_args: {| (nomsu_flag %sep)* {:extras: {| ({[^%sep]+} %sep)* |} :} |} :}
      |} !.
    flag <-
        {:optimization: "-O" (%sep? %number)? :}
      / ({:check_syntax: "-s" %true:})
      / ({:compile: "-c" %true:})
      / {:verbose: "-v" %true :}
      / {:help: ("-h" / "--help") %true :}
      / {:version: "--version" %true :}
      / {:no_core: "--no-core" %true :}
      / {:debugger: ("-d" %sep? {[^%sep]*}) :}
      / {:requested_version: "-V" (%sep? {([0-9.])+})? :}
    nomsu_flag <- {| ({:key: ('-' [a-z]) :} {:value: %true :}) / ({:key: ('--' [^%sep=]+) :} {:value: ('=' {[^%sep]+}) / %true :}) |}
]], {
    true:lpeg.Cc(true), number:lpeg.R("09")^1/tonumber, sep:lpeg.P(sep)
})
arg_string = table.concat(arg, sep)..sep
args = parser\match(arg_string)
if not args or args.help
    print usage
    os.exit(EXIT_FAILURE)
nomsu_args = Dict{}
for argpair in *args.nomsu_args
    nomsu_args[argpair.key] = argpair.value
nomsu_args.extras = List(args.nomsu_args.extras or {})
nomsu_environment.COMMAND_LINE_ARGS = nomsu_args
nomsu_environment.OPTIMIZATION = tonumber(args.optimization or 1)

run = ->
    unless args.no_core
        for nomsupath in package.nomsupath\gmatch("[^;]+")
            files = Files.list(nomsupath.."/core")
            continue unless files
            for f in *files
                continue unless f\match("%.nom$")
                nomsu_environment.run_file_1_in f, nomsu_environment, nomsu_environment.OPTIMIZATION

    if args.version
        nomsu_environment.run_1_in("say (Nomsu version)", nomsu_environment)
        os.exit(EXIT_SUCCESS)

    input_files = {}
    if args.execute
        table.insert input_files, Files.spoof("<input command>", args.execute)
    if args.files
        for f in *args.files
            local files
            if nomsu_name = f\match("^nomsu://(.*)")
                for nomsupath in package.nomsupath\gmatch("[^;]+")
                    files = Files.list(nomsupath.."/"..nomsu_name)
                    continue unless files
            else
                files = Files.list(f)
            unless files and #files > 0
                error("Could not find: '#{f}'")
            for filename in *files
                table.insert input_files, filename
    
    for filename in *input_files
        if args.check_syntax
            -- Check syntax
            code = Files.read(filename)
            source = Source(filename, 1, #code)
            nomsu_environment._1_parsed(NomsuCode\from(source, code))
            print("Parse succeeded: #{filename}")
        elseif args.compile
            -- Compile .nom files into .lua
            output = if filename == 'stdin' then io.output()
            else io.open(filename\gsub("%.nom$", ".lua"), "w")
            code = Files.read(filename)
            source = Source(filename, 1, #code)
            code = NomsuCode\from(source, code)
            tree = nomsu_environment._1_parsed(code)
            tree = {tree} unless tree.type == 'FileChunks'
            for chunk_no, chunk in ipairs tree
                lua = nomsu_environment.compile(chunk)
                lua\declare_locals!
                lua\prepend((chunk_no > 1) and '\n' or '', "-- File #{filename} chunk ##{chunk_no}\n")
                if args.verbose then print(lua\text!)
                nomsu_environment.run_1_in(chunk, nomsu_environment)
                output\write(lua\text!, "\n")
            print ("Compiled %-25s -> %s")\format(filename, filename\gsub("%.nom$", ".lua"))
            output\close!
        elseif args.verbose
            code = Files.read(filename)
            source = Source(filename, 1, #code)
            code = NomsuCode\from(source, code)
            tree = nomsu_environment._1_parsed(code)
            tree = {tree} unless tree.type == 'FileChunks'
            for chunk_no, chunk in ipairs tree
                lua = nomsu_environment.compile(chunk)
                lua\declare_locals!
                lua\prepend((chunk_no > 1) and '\n' or '', "-- File #{filename} chunk ##{chunk_no}\n")
                print(lua\text!)
                nomsu_environment.run_1_in(lua, nomsu_environment)
        else
            -- Just run the file
            nomsu_environment.run_file_1_in(filename, nomsu_environment, 0)

debugger = if args.debugger == "nil" then {}
else require(args.debugger or 'error_handling')
guard = if type(debugger) == 'function' then debugger
else debugger.guard or debugger.call or debugger.wrap or debugger.run or ((fn)->fn())
guard(run)
