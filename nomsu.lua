local usage = [=[Nomsu Compiler

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
local lpeg = require('lpeg')
local re = require('re')
local run_safely = require("error_handling")
local NomsuCompiler = require("nomsu_compiler")
local NomsuCode, LuaCode, Source
do
  local _obj_0 = require("code_obj")
  NomsuCode, LuaCode, Source = _obj_0.NomsuCode, _obj_0.LuaCode, _obj_0.Source
end
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
    file <- "-" / [^;]+
]], {
  ["true"] = function()
    return true
  end
})
local args = table.concat(arg, ";") .. ";"
args = parser:match(args)
if not args or args.help then
  print(usage)
  os.exit()
end
local nomsu = NomsuCompiler
nomsu.arg = args.nomsu_args
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
  nomsu.skip_precompiled = not args.optimized
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
    for f in all_files(input) do
      input_files[#input_files + 1] = f
      to_run[f] = true
    end
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
      local ok, err = pcall(nomsu.parse, nomsu, file_contents, Source(filename, 1, #file_contents))
      if not ok then
        table.insert(parse_errs, err)
      elseif print_file then
        print_file:write("Parse succeeded: " .. tostring(filename) .. "\n")
        print_file:flush()
      end
    elseif args.format then
      local file = FILE_CACHE[filename]
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
      FILE_CACHE.stdin = file
      nomsu:run(file, Source('stdin', 1, #file))
    else
      nomsu:run_file(filename)
    end
  end
  if #parse_errs > 0 then
    io.stderr:write(table.concat(parse_errs, "\n\n"))
    io.stderr:flush()
    os.exit(false, true)
  elseif args.syntax then
    os.exit(true, true)
  end
  if args.interactive then
    for repl_line = 1, math.huge do
      io.write(colored.bright(colored.yellow(">> ")))
      local buff = { }
      while true do
        local line = io.read("*L")
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
      FILE_CACHE["REPL#" .. repl_line] = buff
      local err_hand
      err_hand = function(error_message)
        return print_err_msg(error_message)
      end
      local ok, ret = xpcall(nomsu.run, err_hand, nomsu, buff, Source("REPL#" .. repl_line, 1, #buff))
      if ok and ret ~= nil then
        print("= " .. repr(ret))
      elseif not ok then
        print_err_msg(ret)
      end
    end
  end
end
local has_ldt, ldt = pcall(require, 'ldt')
if has_ldt then
  return ldt.guard(run)
else
  return run_safely(run)
end
