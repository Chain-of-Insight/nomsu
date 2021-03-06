#!/usr/bin/env moon
-- This file contains the command-line Nomsu runner.
export NOMSU_VERSION
NOMSU_VERSION = {7, 0, 1}

clibtype = package.cpath\match("?%.(so)") or package.cpath\match("?%.(dll)")

if NOMSU_PREFIX
    package.path = "#{NOMSU_PREFIX}/share/nomsu/#{table.concat NOMSU_VERSION, "."}/?.lua;"..package.path
    package.cpath = "#{NOMSU_PREFIX}/lib/nomsu/#{table.concat NOMSU_VERSION, "."}/?.#{clibtype};"..package.cpath

export COLOR_ENABLED
COLOR_ENABLED = true
if clibtype == "dll"
    -- Enable colors:
    ok,enable_colors = pcall(require, 'wincolors')
    COLOR_ENABLED = false
    if ok
        ok,_ = pcall(enable_colors)
        if ok
            COLOR_ENABLED = false
    -- Special hack to enable utf8 for windows console applications:
    pcall(os.execute,"chcp 65001>nul")

{:List, :Dict} = require 'containers'
NOMSU_VERSION = List(NOMSU_VERSION)
Text = require 'text'
require 'builtin_metatables'
EXIT_FAILURE = 1
EXIT_SUCCESS = 0

usage = [=[
Nomsu Usage: nomsu [-V version] [--help | -h] [--version] [-O optimization level] [-v] [-c] [-s] [-d debugger] [--no-core] [(file | -t tool | -e "nomsu code..." | files... -- ) [nomsu args...]]

OPTIONS
    -t <tool> Run a tool.
    -e Execute the specified string.
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
{:NomsuCode, :LuaCode, :Source} = require "code_obj"

sep = "\3"
parser = re.compile([[
    args <- {| (flag %sep)*
         {:files: {|
          ( ("-m" %sep)? (file %sep)+ "--" %sep
           / file %sep
           / {~ '' %sep? -> 'nomsu://tools/repl.nom' ~}) |} :}
      {:nomsu_args: {| (nomsu_flag %sep)* {:extras: {| ({[^%sep]+} %sep)* |} :} |} :}
      |} ({.+}?)

    file <-
        (  "-e" %sep ({[^%sep]+} -> spoof)
         / "-t" %sep {~ {[^%sep]+} -> "nomsu://tools/%1.nom" ~}
         / !"--" {[^%sep]+})

    flag <- longflag / shortflag / "-" shortboolflag+
    longflag <-
        {:help: "--help" %true :}
      / {:version: "--version" %true :}
      / {:no_core: "--no-core" %true :}
    shortflag <-
        {:optimization: "-O" %sep? %number :}
      / {:debugger: ("-d" %sep? {[^%sep]+}) :}
      / {:requested_version: "-V" %sep? {([0-9.])+} :}
    shortboolflag <-
        {:check_syntax: "s" %true:}
      / {:compile: "c" %true:}
      / {:verbose: "v" %true :}
      / {:help: "h" %true :}

    nomsu_flag <- nomsu_longflag / "-" nomsu_shortboolflag+
    nomsu_shortboolflag <- {| {:key: [a-zA-Z] :} {:value: %true :} |}
    nomsu_longflag <- '--' {| {:key: [^%sep=]+ :} {:value: ('=' {[^%sep]+}) / %true :} |}
]], {
    true:lpeg.Cc(true), number:lpeg.R("09")^1/tonumber, sep:lpeg.P(sep),
    spoof: Files.spoof
})
arg_string = table.concat(arg, sep)..sep
args, err = parser\match(arg_string)
if not args or err
    if err
        print("Didn't understand: #{err}")
    print usage
    os.exit(EXIT_FAILURE)
if args.help
    print "Nomsu - A dynamically typed programming language with natural syntax and strong metaprogramming abilities."
    print "https://nomsu.org\n"
    print usage
    os.exit(EXIT_SUCCESS)
if args.version
    print(NOMSU_VERSION\joined_with("."))
    os.exit(EXIT_SUCCESS)
nomsu_args = Dict{}
for argpair in *args.nomsu_args
    nomsu_args[argpair.key] = argpair.value
nomsu_args.extras = List(args.nomsu_args.extras or {})
optimization = tonumber(args.optimization or 1)

nomsupath = {}
suffixes = if optimization > 0
    {"?.lua", "?/init.lua", "?.nom", "?/init.nom"}
else {"?.nom", "?/init.nom"}
add_path = (p)->
    for s in *suffixes do table.insert(nomsupath, p.."/"..s)
if NOMSU_PREFIX
    add_path "#{NOMSU_PREFIX}/share/nomsu/#{NOMSU_VERSION\joined_with(".")}/lib"
else
    add_path "./lib"
NOMSU_PACKAGEPATH or= "/opt/nomsu"
add_path NOMSU_PACKAGEPATH
add_path "."
package.nomsupath = table.concat(nomsupath, ";")
package.nomsuloaded = Dict{}

nomsu_environment = require('nomsu_environment')
nomsu_environment.NOMSU_VERSION = NOMSU_VERSION
nomsu_environment.COMMAND_LINE_ARGS = nomsu_args
nomsu_environment.OPTIMIZATION = optimization
nomsu_environment.NOMSU_PACKAGEPATH = NOMSU_PACKAGEPATH
nomsu_environment.NOMSU_PREFIX = NOMSU_PREFIX
nomsu_environment.COLOR_ENABLED = COLOR_ENABLED

run = ->
    unless args.no_core
        nomsu_environment\export("core")

    input_files = {}
    if args.files and #args.files > 0
        for f in *args.files
            if nomsu_name = f\match("^nomsu://(.*)%.nom")
                path, err = package.searchpath(nomsu_name, package.nomsupath, "/")
                if not path then error(err)
                table.insert input_files, path
            else
                table.insert input_files, f

    for filename in *input_files
        if args.check_syntax
            -- Check syntax
            code = Files.read(filename)
            source = Source(filename, 1, #code)
            nomsu_environment._1_parsed(NomsuCode\from(source, code))
            print("Parse succeeded: #{filename}")
        elseif args.compile
            code = Files.read(filename)
            if not code
                error("Could not find file '#{filename}'")
            -- Compile .nom files into .lua
            if filename\match("%.lua$")
                error("Cannot compile a lua file (expected a nomsu file as input)")
            output = if filename == 'stdin' then io.output()
            else io.open(filename\gsub("%.nom$", ".lua"), "w")
            source = Source(filename, 1, #code)
            code = NomsuCode\from(source, code)
            env = nomsu_environment.new_environment!
            env.MODULE_NAME = filename
            tree = env._1_parsed(code)
            if tree.shebang
                output\write tree.shebang
            tree = {tree} unless tree.type == 'FileChunks'
            for chunk_no, chunk in ipairs tree
                lua = env\compile(chunk)
                lua\declare_locals!
                lua\prepend((chunk_no > 1) and '\n' or '', "-- File #{filename} chunk ##{chunk_no}\n")
                if args.verbose then print(lua\text!)
                env\run(chunk)
                output\write(lua\text!, "\n")
            print ("Compiled %-25s -> %s")\format(filename, filename\gsub("%.nom$", ".lua"))
            output\close!
        elseif args.verbose
            env = nomsu_environment.new_environment!
            env.MODULE_NAME = filename
            env.WAS_RUN_DIRECTLY = true
            code = Files.read(filename)
            source = Source(filename, 1, #code)
            if filename\match("%.lua$")
                code = LuaCode\from(Source(filename, 1, #code), code)
                print(code\text!)
                env\run(code)
            else
                code = NomsuCode\from(source, code)
                tree = env._1_parsed(code)
                tree = {tree} unless tree.type == 'FileChunks'
                for chunk_no, chunk in ipairs tree
                    lua = env\compile(chunk)
                    lua\declare_locals!
                    lua\prepend((chunk_no > 1) and '\n' or '', "-- File #{filename} chunk ##{chunk_no}\n")
                    print(lua\text!)
                    env\run(lua)
        else
            -- Just run the file
            f = Files.read(filename)
            if not f
                if filename\match "^-"
                    print "Not a valid flag: "..filename.."\n"
                    print usage
                else
                    print "File not found: "..filename
                os.exit EXIT_FAILURE
            if filename\match("%.lua$")
                f = LuaCode\from(Source(filename, 1, #f), f)
            env = nomsu_environment.new_environment!
            env.MODULE_NAME = filename
            env.WAS_RUN_DIRECTLY = true
            env\run(f)

debugger = if args.debugger == "nil" then {}
else require(args.debugger or 'error_handling')
guard = if type(debugger) == 'function' then debugger
else debugger.guard or debugger.call or debugger.wrap or debugger.run or ((fn)->fn())
guard(run)
