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

Usage: (nomsu | lua nomsu.lua | moon nomsu.moon) [-V version] [-O optimization level] [-v] [-c] [-s] [-I file] [--help | -h] [--version] [--no-core] [file [nomsu args...]]

OPTIONS
    -O <level> Run the compiler with the given optimization level (>0: use precompiled .lua versions of Nomsu files, when available).
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
--NomsuCompiler = require "nomsu_compiler"
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
--{:Importer, :import_to_1_from, :_1_forked} = require 'importer'
{:List, :Dict, :Text} = require 'containers'
--SyntaxTree = require "syntax_tree"
nomsu_environment = require('nomsu_environment')

-- If this file was reached via require(), then just return the Nomsu compiler
if not arg or debug.getinfo(2).func == require
    return nomsu_environment

file_queue = List{}
sep = "\3"
parser = re.compile([[
    args <- {| (flag %sep)* (({~ file ~} -> add_file) {:primary_file: %true :} %sep)?
        {:nomsu_args: {| ({(!%sep .)*} %sep)* |} :} %sep? |} !.
    flag <-
        {:optimization: "-O" (%sep? %number)? :}
      / ("-I" %sep? ({~ file ~} -> add_file))
      / ("-e" %sep? (({} {~ file ~}) -> add_exec_string) {:exec_strings: %true :})
      / ({:check_syntax: "-s" %true:})
      / ({:compile: "-c" %true:})
      / {:verbose: "-v" %true :}
      / {:help: ("-h" / "--help") %true :}
      / {:version: "--version" %true :}
      / {:no_core: "--no-core" %true :}
      / {:debugger: ("-d" %sep? {(!%sep .)*}) :}
      / {:requested_version: "-V" (%sep? {([0-9.])+})? :}
    file <- ("-" -> "stdin") / {(!%sep .)+}
]], {
    true:lpeg.Cc(true), number:lpeg.R("09")^1/tonumber, sep:lpeg.P(sep)
    add_file: (f)-> file_queue\add(f)
    add_exec_string: (pos, s)->
        name = "command line arg @#{pos}.nom"
        Files.spoof(name, s)
        file_queue\add name
})
arg_string = table.concat(arg, sep)..sep
args = parser\match(arg_string)
if not args or args.help
    print usage
    os.exit(EXIT_FAILURE)
nomsu_environment.command_line_args = List(args.nomsu_args)
nomsu_environment.OPTIMIZATION = tonumber(args.optimization or 1)

if args.version
    nomsu_environment.run_file_1_in 'core', nomsu_environment, nomsu_environment.OPTIMIZATION
    nomsu_environment.run_1_in([[say (Nomsu version)]], nomsu_environment)
    os.exit(EXIT_SUCCESS)

run = ->
    input_files = {}
    for f in *file_queue
        if f == 'stdin'
            input_files[f] = true
            continue
        unless Files.exists(f)
            error("Could not find: '#{f}'")
        for _,filename in Files.walk(f)
            input_files[filename] = true

    unless args.no_core
        nomsu_environment.run_file_1_in 'core', nomsu_environment, nomsu_environment.OPTIMIZATION
    
    for f in *file_queue
        for _,filename in Files.walk(f)
            continue unless filename == "stdin" or filename\match("%.nom$")
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
                for chunk in *tree
                    lua = nomsu_environment.compile(chunk)
                    lua\declare_locals!
                    nomsu_environment.run_1_in(chunk, nomsu_environment)
                    output\write(tostring(lua), "\n")
                    if args.verbose then print(tostring(lua))
                print ("Compiled %-25s -> %s")\format(filename, filename\gsub("%.nom$", ".lua"))
                output\close!
            elseif args.verbose
                code = Files.read(filename)
                source = Source(filename, 1, #code)
                code = NomsuCode\from(source, code)
                tree = nomsu_environment._1_parsed(code)
                tree = {tree} unless tree.type == 'FileChunks'
                for chunk in *tree
                    lua = nomsu_environment.compile(chunk)
                    lua\declare_locals!
                    nomsu_environment.run_1_in(chunk, nomsu_environment)
                    print(tostring(lua))
            else
                -- Just run the file
                nomsu_environment.run_file_1_in(filename, nomsu_environment, 0)

    unless args.primary_file or args.exec_strings
        nomsu_environment.run_file_1_in("tools/repl.nom", nomsu_environment, nomsu_environment.OPTIMIZATION)

debugger = if args.debugger == "nil" then {}
else require(args.debugger or 'error_handling')
guard = if type(debugger) == 'function' then debugger
else debugger.guard or debugger.call or debugger.wrap or debugger.run or ((fn)->fn())
guard(run)
