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
  end)(), ";")
end
local EXIT_SUCCESS, EXIT_FAILURE = 0, 1
local usage = [=[Nomsu Compiler

Usage: (nomsu | lua nomsu.lua | moon nomsu.moon) [-V version] [-O] [-v] [-c] [-s] [-t] [-I file] [--help | -h] [--version] [file [nomsu args...]]

OPTIONS
    -O Run the compiler in optimized mode (use precompiled .lua versions of Nomsu files, when available).
    -v Verbose: print compiled lua code.
    -c Compile the input files into a .lua files.
    -s Check the input files for syntax errors.
    -t Run tests.
    -I <file> Add an additional input file or directory.
    -d <debugger> Attempt to use the specified debugger to wrap the main body of execution.
    -h/--help Print this message.
    --version Print the version number and exit.
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
local Errhand = require("error_handling")
local NomsuCompiler = require("nomsu_compiler")
local NomsuCode, LuaCode, Source
do
  local _obj_0 = require("code_obj")
  NomsuCode, LuaCode, Source = _obj_0.NomsuCode, _obj_0.LuaCode, _obj_0.Source
end
local repr
repr = require("utils").repr
if not arg or debug.getinfo(2).func == require then
  return NomsuCompiler
end
local file_queue = { }
local parser = re.compile([[    args <- {| (flag ";")* (({~ file ~} -> add_file) ";")? {:nomsu_args: {| ({[^;]*} ";")* |} :} ";"? |} !.
    flag <-
        {:optimized: ("-O" -> true) :}
      / ("-I" (";")? ({~ file ~} -> add_file))
      / ({:check_syntax: ("-s" -> true):})
      / ({:compile: ("-c" -> true):})
      / {:run_tests: ("-t" -> true) :}
      / {:verbose: ("-v" -> true) :}
      / {:help: (("-h" / "--help") -> true) :}
      / {:version: ("--version" -> true) :}
      / {:debugger: ("-d" (";")? {([^;])*}) :}
      / {:requested_version: "-V" ((";")? {([0-9.])+})? :}
    file <- ("-" -> "stdin") / {[^;]+}
]], {
  ["true"] = function()
    return true
  end,
  add_file = function(f)
    return table.insert(file_queue, f)
  end
})
local arg_string = table.concat(arg, ";") .. ";"
local args = parser:match(arg_string)
if not args or args.help then
  print(usage)
  os.exit(EXIT_FAILURE)
end
local files = require("files")
local nomsu = NomsuCompiler
nomsu.arg = NomsuCompiler.list(args.nomsu_args)
if args.version then
  nomsu:run([[use "core"
say (Nomsu version)]])
  os.exit(EXIT_SUCCESS)
end
FILE_CACHE = setmetatable({ }, {
  __index = function(self, filename)
    local file = io.open(filename)
    if not (file) then
      return nil
    end
    local contents = file:read("*a")
    file:close()
    self[filename] = contents
    return contents
  end
})
local run
run = function()
  local input_files = { }
  for _index_0 = 1, #file_queue do
    local f = file_queue[_index_0]
    if not (files.exists(f)) then
      error("Could not find: " .. tostring(f))
    end
    for filename in files.walk(f) do
      input_files[filename] = true
    end
  end
  nomsu.can_optimize = function(f)
    if not (args.optimized) then
      return false
    end
    if args.compile and input_files[f] then
      return false
    end
    return true
  end
  local tests = { }
  if args.run_tests then
    nomsu.COMPILE_ACTIONS["test %"] = function(self, tree, _body)
      if not (tests[tree.source.filename]) then
        tests[tree.source.filename] = { }
      end
      table.insert(tests[tree.source.filename], _body)
      return LuaCode("")
    end
  end
  local get_file_and_source
  get_file_and_source = function(filename)
    local file, source
    if filename == 'stdin' then
      file = io.read("*a")
      files.spoof('stdin', file)
      source = Source('stdin', 1, #file)
    elseif filename:match("%.nom$") then
      file = files.read(filename)
      if not file then
        error("File does not exist: " .. tostring(filename), 0)
      end
      source = Source(filename, 1, #file)
    else
      return nil
    end
    source = Source(filename, 1, #file)
    return file, source
  end
  local run_file
  run_file = function(filename, lua_handler)
    if lua_handler == nil then
      lua_handler = nil
    end
    local file, source = get_file_and_source(filename)
    if not (file) then
      return 
    end
    local tree = nomsu:parse(file, source)
    if tree then
      if tree.type ~= "FileChunks" then
        tree = {
          tree
        }
      end
      for _index_0 = 1, #tree do
        local chunk = tree[_index_0]
        local lua = nomsu:compile(chunk):as_statements("return ")
        lua:declare_locals()
        lua:prepend("-- File: " .. tostring(source.filename:gsub("\n.*", "...")) .. "\n")
        if lua_handler and input_files[filename] then
          lua_handler(tostring(lua))
        end
        nomsu:run_lua(lua)
      end
      if args.run_tests and tests[filename] and input_files[filename] then
        local _list_0 = tests[filename]
        for _index_0 = 1, #_list_0 do
          local t = _list_0[_index_0]
          local lua = nomsu:compile(t)
          if lua_handler then
            lua_handler(tostring(lua))
          end
          nomsu:run_lua(lua, t.source)
        end
      end
    end
  end
  local parse_errs = { }
  for _index_0 = 1, #file_queue do
    local f = file_queue[_index_0]
    for filename in files.walk(f) do
      local _continue_0 = false
      repeat
        if not (filename == "stdin" or filename:match("%.nom$")) then
          _continue_0 = true
          break
        end
        if args.check_syntax then
          local file, source = get_file_and_source(filename)
          if not (file) then
            _continue_0 = true
            break
          end
          nomsu:parse(file, source)
          print("Parse succeeded: " .. tostring(filename))
        end
        if args.compile then
          local output
          if filename == 'stdin' then
            output = io.output()
          else
            output = io.open(filename:gsub("%.nom$", ".lua"), "w")
          end
          run_file(filename, function(lua)
            output:write(tostring(lua), "\n")
            if args.verbose then
              return print(tostring(lua))
            end
          end)
          print(("Compiled %-25s -> %s"):format(filename, filename:gsub("%.nom$", ".lua")))
          output:close()
        end
        if not args.check_syntax and not args.compile then
          run_file(filename, (args.verbose and print or nil))
        end
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
  end
  if #file_queue == 0 then
    nomsu:run([[use "core"
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
    \("")]])
    for repl_line = 1, math.huge do
      io.write(colored.bright(colored.yellow(">> ")))
      local buff = { }
      while true do
        io.write(colors.bright)
        local line = io.read("*L")
        io.write(colors.reset)
        if line == "\n" or not line then
          if #buff > 0 then
            io.write("\027[1A\027[2K")
          end
          break
        end
        line = line:gsub("\t", "    ")
        table.insert(buff, line)
        io.write(colored.dim(colored.yellow(".. ")))
      end
      if #buff == 0 then
        break
      end
      buff = table.concat(buff)
      local pseudo_filename = "user input #" .. repl_line
      files.spoof(pseudo_filename, buff)
      local err_hand
      err_hand = function(error_message)
        return Errhand.print_error(error_message)
      end
      local ret
      ok, ret = xpcall(nomsu.run, err_hand, nomsu, buff, Source(pseudo_filename, 1, #buff))
      if ok and ret ~= nil then
        print("= " .. repr(ret))
      elseif not ok then
        Errhand.print_error(ret)
      end
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
