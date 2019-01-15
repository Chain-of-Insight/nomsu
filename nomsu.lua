if NOMSU_VERSION and NOMSU_PREFIX then
  package.path = tostring(NOMSU_PREFIX) .. "/share/nomsu/" .. tostring(NOMSU_VERSION) .. "/?.lua;" .. package.path
  package.cpath = tostring(NOMSU_PREFIX) .. "/lib/nomsu/" .. tostring(NOMSU_VERSION) .. "/?.so;" .. package.cpath
end
local EXIT_SUCCESS, EXIT_FAILURE = 0, 1
if _VERSION == "Lua 5.1" and not jit then
  print("Sorry, Nomsu does not run on Lua 5.1. Please use LuaJIT 2+ or Lua 5.2+")
  os.exit(EXIT_FAILURE)
end
local usage = [=[Nomsu Compiler

Usage: (nomsu | lua nomsu.lua | moon nomsu.moon) [-V version] [--help | -h] [--version] [-O optimization level] [-v] [-c] [-s] [-d debugger] [--no-core] [(file | -t tool | -e "nomsu code..." | files... -- ) [nomsu args...]]

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
local ok, _ = pcall(function()
  lpeg = require('lpeg')
  re = require('re')
end)
if not ok then
  print("Error: unable to find the 'lpeg' Lua module. Please install LPEG either from http://www.inf.puc-rio.br/~roberto/lpeg/re.html or, if you use luarocks: `luarocks install lpeg`")
  os.exit(EXIT_FAILURE)
end
local Files = require("files")
local Errhand = require("error_handling")
local NomsuCode, LuaCode, Source
do
  local _obj_0 = require("code_obj")
  NomsuCode, LuaCode, Source = _obj_0.NomsuCode, _obj_0.LuaCode, _obj_0.Source
end
local List, Dict, Text
do
  local _obj_0 = require('containers')
  List, Dict, Text = _obj_0.List, _obj_0.Dict, _obj_0.Text
end
local sep = "\3"
local parser = re.compile([[    args <- {| (flag %sep)*
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
  ["true"] = lpeg.Cc(true),
  number = lpeg.R("09") ^ 1 / tonumber,
  sep = lpeg.P(sep),
  spoof = Files.spoof
})
local arg_string = table.concat(arg, sep) .. sep
local args, err = parser:match(arg_string)
if not args or err or args.help then
  if err then
    print("Didn't understand: \x1b[31;1m" .. tostring(err) .. "\x1b[0m")
  end
  print(usage)
  os.exit(EXIT_FAILURE)
end
local nomsu_args = Dict({ })
local _list_0 = args.nomsu_args
for _index_0 = 1, #_list_0 do
  local argpair = _list_0[_index_0]
  nomsu_args[argpair.key] = argpair.value
end
nomsu_args.extras = List(args.nomsu_args.extras or { })
local optimization = tonumber(args.optimization or 1)
local nomsupath = { }
local suffixes
if optimization > 0 then
  suffixes = {
    "?.lua",
    "?/init.lua",
    "?.nom",
    "?/init.nom"
  }
else
  suffixes = {
    "?.nom",
    "?/init.nom"
  }
end
local add_path
add_path = function(p)
  for _index_0 = 1, #suffixes do
    local s = suffixes[_index_0]
    table.insert(nomsupath, p .. "/" .. s)
  end
end
if NOMSU_VERSION and NOMSU_PREFIX then
  add_path(tostring(NOMSU_PREFIX) .. "/share/nomsu/" .. tostring(NOMSU_VERSION) .. "/lib")
else
  add_path("./lib")
end
local NOMSU_PACKAGEPATH = NOMSU_PACKAGEPATH or "/opt/nomsu"
add_path(NOMSU_PACKAGEPATH)
add_path(".")
package.nomsupath = table.concat(nomsupath, ";")
package.nomsuloaded = Dict({ })
local nomsu_environment = require('nomsu_environment')
nomsu_environment.COMMAND_LINE_ARGS = nomsu_args
nomsu_environment.OPTIMIZATION = optimization
nomsu_environment.NOMSU_PACKAGEPATH = NOMSU_PACKAGEPATH
nomsu_environment.NOMSU_PREFIX = NOMSU_PREFIX
local run
run = function()
  if not (args.no_core) then
    nomsu_environment:export("core")
  end
  if args.version then
    nomsu_environment:run("say (Nomsu version)")
    os.exit(EXIT_SUCCESS)
  end
  local input_files = { }
  if args.files then
    local _list_1 = args.files
    for _index_0 = 1, #_list_1 do
      local f = _list_1[_index_0]
      do
        local nomsu_name = f:match("^nomsu://(.*)%.nom")
        if nomsu_name then
          local path
          path, err = package.searchpath(nomsu_name, package.nomsupath, "/")
          if not path then
            error(err)
          end
          table.insert(input_files, path)
        else
          table.insert(input_files, f)
        end
      end
    end
  end
  for _index_0 = 1, #input_files do
    local filename = input_files[_index_0]
    if args.check_syntax then
      local code = Files.read(filename)
      local source = Source(filename, 1, #code)
      nomsu_environment._1_parsed(NomsuCode:from(source, code))
      print("Parse succeeded: " .. tostring(filename))
    elseif args.compile then
      local code = Files.read(filename)
      if not code then
        error("Could not find file '" .. tostring(filename) .. "'")
      end
      if filename:match("%.lua$") then
        error("Cannot compile a lua file (expected a nomsu file as input)")
      end
      local output
      if filename == 'stdin' then
        output = io.output()
      else
        output = io.open(filename:gsub("%.nom$", ".lua"), "w")
      end
      local source = Source(filename, 1, #code)
      code = NomsuCode:from(source, code)
      local env = nomsu_environment.new_environment()
      env.MODULE_NAME = filename
      local tree = env._1_parsed(code)
      if not (tree.type == 'FileChunks') then
        tree = {
          tree
        }
      end
      for chunk_no, chunk in ipairs(tree) do
        local lua = env:compile(chunk)
        lua:declare_locals()
        lua:prepend((chunk_no > 1) and '\n' or '', "-- File " .. tostring(filename) .. " chunk #" .. tostring(chunk_no) .. "\n")
        if args.verbose then
          print(lua:text())
        end
        env:run(chunk)
        output:write(lua:text(), "\n")
      end
      print(("Compiled %-25s -> %s"):format(filename, filename:gsub("%.nom$", ".lua")))
      output:close()
    elseif args.verbose then
      local env = nomsu_environment.new_environment()
      env.MODULE_NAME = filename
      env.WAS_RUN_DIRECTLY = true
      local code = Files.read(filename)
      local source = Source(filename, 1, #code)
      if filename:match("%.lua$") then
        code = LuaCode:from(Source(filename, 1, #code), code)
        print(code:text())
        env:run(code)
      else
        code = NomsuCode:from(source, code)
        local tree = env._1_parsed(code)
        if not (tree.type == 'FileChunks') then
          tree = {
            tree
          }
        end
        for chunk_no, chunk in ipairs(tree) do
          local lua = env:compile(chunk)
          lua:declare_locals()
          lua:prepend((chunk_no > 1) and '\n' or '', "-- File " .. tostring(filename) .. " chunk #" .. tostring(chunk_no) .. "\n")
          print(lua:text())
          env:run(lua)
        end
      end
    else
      local f = Files.read(filename)
      if filename:match("%.lua$") then
        f = LuaCode:from(Source(filename, 1, #f), f)
      end
      local env = nomsu_environment.new_environment()
      env.MODULE_NAME = filename
      env.WAS_RUN_DIRECTLY = true
      env:run(f)
    end
  end
end
local debugger
if args.debugger == "nil" then
  debugger = { }
else
  debugger = require(args.debugger or 'error_handling')
end
local guard
if type(debugger) == 'function' then
  guard = debugger
else
  guard = debugger.guard or debugger.call or debugger.wrap or debugger.run or (function(fn)
    return fn()
  end)
end
return guard(run)
