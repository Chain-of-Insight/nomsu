local EXIT_SUCCESS, EXIT_FAILURE = 0, 1
if _VERSION == "Lua 5.1" and not jit then
  print("Sorry, Nomsu does not run on Lua 5.1. Please use LuaJIT 2+ or Lua 5.2+")
  os.exit(EXIT_FAILURE)
end
if NOMSU_VERSION and NOMSU_PREFIX then
  local ver_bits
  do
    local _accum_0 = { }
    local _len_0 = 1
    for ver_bit in NOMSU_VERSION:gmatch("[0-9]+") do
      _accum_0[_len_0] = ver_bit
      _len_0 = _len_0 + 1
    end
    ver_bits = _accum_0
  end
  local partial_vers
  do
    local _accum_0 = { }
    local _len_0 = 1
    for i = #ver_bits, 1, -1 do
      _accum_0[_len_0] = table.concat(ver_bits, '.', 1, i)
      _len_0 = _len_0 + 1
    end
    partial_vers = _accum_0
  end
  package.path = table.concat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #partial_vers do
      local v = partial_vers[_index_0]
      _accum_0[_len_0] = tostring(NOMSU_PREFIX) .. "/share/nomsu/" .. tostring(v) .. "/?.lua"
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)(), ";") .. ";" .. package.path
  package.cpath = table.concat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #partial_vers do
      local v = partial_vers[_index_0]
      _accum_0[_len_0] = tostring(NOMSU_PREFIX) .. "/lib/nomsu/" .. tostring(v) .. "/?.so"
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)(), ";") .. ";" .. package.cpath
  package.nomsupath = table.concat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #partial_vers do
      local v = partial_vers[_index_0]
      _accum_0[_len_0] = tostring(NOMSU_PREFIX) .. "/share/nomsu/" .. tostring(v)
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)(), ";") .. ";."
else
  package.nomsupath = "."
end
local usage = [=[Nomsu Compiler

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
local nomsu_environment = require('nomsu_environment')
if not arg or debug.getinfo(2).func == require then
  return nomsu_environment
end
local file_queue = List({ })
local sep = "\3"
local parser = re.compile([[    args <- {| (flag %sep)* (({~ file ~} -> add_file) {:primary_file: %true :} %sep)?
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
  ["true"] = lpeg.Cc(true),
  number = lpeg.R("09") ^ 1 / tonumber,
  sep = lpeg.P(sep),
  add_file = function(f)
    return file_queue:add(f)
  end,
  add_exec_string = function(pos, s)
    local name = "command line arg @" .. tostring(pos) .. ".nom"
    Files.spoof(name, s)
    return file_queue:add(name)
  end
})
local arg_string = table.concat(arg, sep) .. sep
local args = parser:match(arg_string)
if not args or args.help then
  print(usage)
  os.exit(EXIT_FAILURE)
end
nomsu_environment.command_line_args = List(args.nomsu_args)
nomsu_environment.OPTIMIZATION = tonumber(args.optimization or 1)
if args.version then
  nomsu_environment.run_file_1_in('core', nomsu_environment, nomsu_environment.OPTIMIZATION)
  nomsu_environment.run_1_in([[say (Nomsu version)]], nomsu_environment)
  os.exit(EXIT_SUCCESS)
end
local run
run = function()
  local input_files = { }
  for _index_0 = 1, #file_queue do
    local _continue_0 = false
    repeat
      local f = file_queue[_index_0]
      if f == 'stdin' then
        input_files[f] = true
        _continue_0 = true
        break
      end
      if not (Files.exists(f)) then
        error("Could not find: '" .. tostring(f) .. "'")
      end
      for _, filename in Files.walk(f) do
        input_files[filename] = true
      end
      _continue_0 = true
    until true
    if not _continue_0 then
      break
    end
  end
  if not (args.no_core) then
    nomsu_environment.run_file_1_in('core', nomsu_environment, nomsu_environment.OPTIMIZATION)
  end
  for _index_0 = 1, #file_queue do
    local f = file_queue[_index_0]
    for _, filename in Files.walk(f) do
      local _continue_0 = false
      repeat
        if not (filename == "stdin" or filename:match("%.nom$")) then
          _continue_0 = true
          break
        end
        if args.check_syntax then
          local code = Files.read(filename)
          local source = Source(filename, 1, #code)
          nomsu_environment._1_parsed(NomsuCode:from(source, code))
          print("Parse succeeded: " .. tostring(filename))
        elseif args.compile then
          local output
          if filename == 'stdin' then
            output = io.output()
          else
            output = io.open(filename:gsub("%.nom$", ".lua"), "w")
          end
          local code = Files.read(filename)
          local source = Source(filename, 1, #code)
          code = NomsuCode:from(source, code)
          local tree = nomsu_environment._1_parsed(code)
          if not (tree.type == 'FileChunks') then
            tree = {
              tree
            }
          end
          for _index_1 = 1, #tree do
            local chunk = tree[_index_1]
            local lua = nomsu_environment.compile(chunk)
            lua:declare_locals()
            nomsu_environment.run_1_in(chunk, nomsu_environment)
            output:write(tostring(lua), "\n")
            if args.verbose then
              print(tostring(lua))
            end
          end
          print(("Compiled %-25s -> %s"):format(filename, filename:gsub("%.nom$", ".lua")))
          output:close()
        elseif args.verbose then
          local code = Files.read(filename)
          local source = Source(filename, 1, #code)
          code = NomsuCode:from(source, code)
          local tree = nomsu_environment._1_parsed(code)
          if not (tree.type == 'FileChunks') then
            tree = {
              tree
            }
          end
          for _index_1 = 1, #tree do
            local chunk = tree[_index_1]
            local lua = nomsu_environment.compile(chunk)
            lua:declare_locals()
            nomsu_environment.run_1_in(chunk, nomsu_environment)
            print(tostring(lua))
          end
        else
          nomsu_environment.run_file_1_in(filename, nomsu_environment, 0)
        end
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
  end
  if not (args.primary_file or args.exec_strings) then
    return nomsu_environment.run_file_1_in("tools/repl.nom", nomsu_environment, nomsu_environment.OPTIMIZATION)
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
