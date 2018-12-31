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
local sep = "\3"
local parser = re.compile([[    args <- {| (flag %sep)*
        (("-e" %sep {:execute: {[^%sep]+} :} %sep)
         / {:files: {|
            ("-t" %sep {~ {[^%sep]+} -> "nomsu://tools/%1.nom" ~} %sep
             / "-m" %sep (!("--" %sep) {[^%sep]+} %sep)* ("--" %sep)?
             / {[^%sep]+} %sep
             / {~ '' %sep? -> 'nomsu://tools/repl.nom' ~}) |} :})
      {:nomsu_args: {| (nomsu_flag %sep)* {:extras: {| ({[^%sep]+} %sep)* |} :} |} :}
      |} !.

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
  sep = lpeg.P(sep)
})
local arg_string = table.concat(arg, sep) .. sep
local args = parser:match(arg_string)
if not args or args.help then
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
nomsu_environment.COMMAND_LINE_ARGS = nomsu_args
nomsu_environment.OPTIMIZATION = tonumber(args.optimization or 1)
local run
run = function()
  if not (args.no_core) then
    for nomsupath in package.nomsupath:gmatch("[^;]+") do
      local _continue_0 = false
      repeat
        local files = Files.list(nomsupath .. "/core")
        if not (files) then
          _continue_0 = true
          break
        end
        for _index_0 = 1, #files do
          local _continue_1 = false
          repeat
            local f = files[_index_0]
            if not (f:match("%.nom$")) then
              _continue_1 = true
              break
            end
            nomsu_environment.run_file_1_in(f, nomsu_environment, nomsu_environment.OPTIMIZATION)
            _continue_1 = true
          until true
          if not _continue_1 then
            break
          end
        end
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
  end
  if args.version then
    nomsu_environment.run_1_in("say (Nomsu version)", nomsu_environment)
    os.exit(EXIT_SUCCESS)
  end
  local input_files = { }
  if args.execute then
    table.insert(input_files, Files.spoof("<input command>", args.execute))
  end
  if args.files then
    local _list_1 = args.files
    for _index_0 = 1, #_list_1 do
      local f = _list_1[_index_0]
      local files
      do
        local nomsu_name = f:match("^nomsu://(.*)")
        if nomsu_name then
          for nomsupath in package.nomsupath:gmatch("[^;]+") do
            files = Files.list(nomsupath .. "/" .. nomsu_name)
            if files then
              break
            end
          end
        else
          files = Files.list(f)
        end
      end
      if not (files and #files > 0) then
        error("Could not find: '" .. tostring(f) .. "'")
      end
      for _index_1 = 1, #files do
        local filename = files[_index_1]
        table.insert(input_files, filename)
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
      for chunk_no, chunk in ipairs(tree) do
        local lua = nomsu_environment.compile(chunk)
        lua:declare_locals()
        lua:prepend((chunk_no > 1) and '\n' or '', "-- File " .. tostring(filename) .. " chunk #" .. tostring(chunk_no) .. "\n")
        if args.verbose then
          print(lua:text())
        end
        nomsu_environment.run_1_in(chunk, nomsu_environment)
        output:write(lua:text(), "\n")
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
      for chunk_no, chunk in ipairs(tree) do
        local lua = nomsu_environment.compile(chunk)
        lua:declare_locals()
        lua:prepend((chunk_no > 1) and '\n' or '', "-- File " .. tostring(filename) .. " chunk #" .. tostring(chunk_no) .. "\n")
        print(lua:text())
        nomsu_environment.run_1_in(lua, nomsu_environment)
      end
    else
      nomsu_environment.run_file_1_in(filename, nomsu_environment, 0)
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
