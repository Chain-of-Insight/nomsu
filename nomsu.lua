if NOMSU_VERSION and NOMSU_LIB and NOMSU_SHARE then
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
      _accum_0[_len_0] = tostring(NOMSU_SHARE) .. "/" .. tostring(v) .. "/?.lua"
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)(), ";") .. ";" .. package.path
  package.cpath = table.concat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #partial_vers do
      local v = partial_vers[_index_0]
      _accum_0[_len_0] = tostring(NOMSU_LIB) .. "/" .. tostring(v) .. "/?.so"
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)(), ";") .. ";" .. package.cpath
  package.nomsupath = table.concat((function()
    local _accum_0 = { }
    local _len_0 = 1
    for _index_0 = 1, #partial_vers do
      local v = partial_vers[_index_0]
      _accum_0[_len_0] = tostring(NOMSU_SHARE) .. "/" .. tostring(v)
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)(), ";")
end
local EXIT_SUCCESS, EXIT_FAILURE = 0, 1
local usage = [=[Nomsu Compiler

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
local STDIN, STDOUT, STDERR = "/dev/fd/0", "/dev/fd/1", "/dev/fd/2"
if not arg or debug.getinfo(2).func == require then
  return NomsuCompiler
end
local parser = re.compile([[    args <- {| (flag ";")* {:inputs: {| ({file} ";")* |} :} {:nomsu_args: {| ("--;" ({[^;]*} ";")*)? |} :} ";"? |} !.
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
]], {
  ["true"] = function()
    return true
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
nomsu.arg = args.nomsu_args
if args.version then
  nomsu:run('use "core"\nsay (Nomsu version)')
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
  for i, input in ipairs(args.inputs) do
    if input == "-" then
      args.inputs[i] = STDIN
    end
  end
  if #args.inputs == 0 and not args.interactive then
    args.inputs = {
      "core"
    }
    args.interactive = true
  end
  local print_file
  if args.print_file == "-" then
    print_file = io.stdout
  elseif args.print_file then
    print_file = io.open(args.print_file, 'w')
  else
    print_file = io.stdout
  end
  if print_file == nil then
    nomsu.print = function() end
  elseif print_file ~= io.stdout then
    nomsu.print = function(...)
      local N = select("#", ...)
      if N > 0 then
        print_file:write(tostring(select(1, ...)))
        for i = 2, N do
          print_file:write('\t', tostring(select(1, ...)))
        end
      end
      print_file:write('\n')
      return print_file:flush()
    end
  end
  local input_files = { }
  local to_run = { }
  local _list_0 = args.inputs
  for _index_0 = 1, #_list_0 do
    local input = _list_0[_index_0]
    local found = false
    for f in files.walk(input) do
      input_files[#input_files + 1] = f
      to_run[f] = true
      found = true
    end
    if not found then
      error("Could not find: " .. tostring(input))
    end
  end
  nomsu.can_optimize = function(f)
    if not (args.optimized) then
      return false
    end
    if to_run[f] then
      return false
    end
    return true
  end
  if args.compile or args.verbose then
    nomsu.on_compile = function(code, from_file)
      if not (to_run[from_file]) then
        return 
      end
      if args.verbose then
        io.write(tostring(code), "\n")
      end
      if args.compile and from_file:match("%.nom$") then
        local output_filename = from_file:gsub("%.nom$", ".lua")
        local output_file = io.open(output_filename, 'w')
        output_file:write(tostring(code))
        output_file:flush()
        print(("Compiled %-25s -> %s"):format(from_file, output_filename))
        return output_file:close()
      end
    end
  end
  local parse_errs = { }
  for _index_0 = 1, #input_files do
    local filename = input_files[_index_0]
    if args.syntax then
      local file_contents = io.open(filename):read('*a')
      local err
      ok, err = pcall(nomsu.parse, nomsu, file_contents, Source(filename, 1, #file_contents))
      if not ok then
        table.insert(parse_errs, err)
      elseif print_file then
        print_file:write("Parse succeeded: " .. tostring(filename) .. "\n")
        print_file:flush()
      end
    elseif args.format then
      local file = files.read(filename)
      if not file then
        error("File does not exist: " .. tostring(filename), 0)
      end
      local tree = nomsu:parse(file, Source(filename, 1, #file))
      local formatted = tostring(nomsu:tree_to_nomsu(tree))
      if print_file then
        print_file:write(formatted, "\n")
        print_file:flush()
      end
    elseif filename == STDIN then
      local file = io.input():read("*a")
      files.spoof('stdin', file)
      nomsu:run(file, Source('stdin', 1, #file))
    else
      nomsu:run_file(filename)
    end
  end
  if #parse_errs > 0 then
    io.stderr:write(table.concat(parse_errs, "\n\n"))
    io.stderr:flush()
    os.exit(EXIT_FAILURE)
  elseif args.syntax then
    os.exit(EXIT_SUCCESS)
  end
  if args.interactive then
    nomsu:run('say "\\n\\(bright)\\(underscore)Welcome to the Nomsu v\\(Nomsu version) interactive console!\\(reset color)\\n     press \'enter\' twice to run a command\\n"')
    local ready_to_quit = false
    nomsu.A_quit = function()
      ready_to_quit = true
      return print("Goodbye!")
    end
    nomsu.A_exit = nomsu.A_quit
    nomsu.A_help = function()
      print("This is the Nomsu v" .. tostring(nomsu.A_Nomsu_version()) .. " interactive console.")
      print("You can type in Nomsu code here and hit 'enter' twice to run it.")
      return print("To exit, type 'exit' or 'quit' and hit enter twice")
    end
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
      if ready_to_quit then
        break
      end
    end
  end
end
local has_ldt, ldt = pcall(require, 'ldt')
if has_ldt then
  return ldt.guard(run)
else
  return Errhand.run_safely(run)
end
