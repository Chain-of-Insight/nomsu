local NomsuCode, LuaCode, Source
do
  local _obj_0 = require("code_obj")
  NomsuCode, LuaCode, Source = _obj_0.NomsuCode, _obj_0.LuaCode, _obj_0.Source
end
local Importer, import_to_1_from, _1_forked
do
  local _obj_0 = require('importer')
  Importer, import_to_1_from, _1_forked = _obj_0.Importer, _obj_0.import_to_1_from, _obj_0._1_forked
end
local List, Dict, Text
do
  local _obj_0 = require('containers')
  List, Dict, Text = _obj_0.List, _obj_0.Dict, _obj_0.Text
end
local SyntaxTree = require("syntax_tree")
local Files = require("files")
local make_parser = require("parser")
local pretty_error = require("pretty_errors")
local make_tree
make_tree = function(tree, userdata)
  tree.source = Source(userdata.filename, tree.start, tree.stop)
  tree.start, tree.stop = nil, nil
  tree = SyntaxTree(tree)
  return tree
end
local Parsers = { }
local max_parser_version = 0
for version = 1, 999 do
  local peg_file
  if package.nomsupath then
    for path in package.nomsupath:gmatch("[^;]+") do
      peg_file = io.open(path .. "/nomsu." .. tostring(version) .. ".peg")
      if peg_file then
        break
      end
    end
  else
    peg_file = io.open("nomsu." .. tostring(version) .. ".peg")
  end
  if not (peg_file) then
    break
  end
  max_parser_version = version
  local peg_contents = peg_file:read('*a')
  peg_file:close()
  Parsers[version] = make_parser(peg_contents, make_tree)
end
local tree_to_nomsu, tree_to_inline_nomsu
do
  local _obj_0 = require("nomsu_decompiler")
  tree_to_nomsu, tree_to_inline_nomsu = _obj_0.tree_to_nomsu, _obj_0.tree_to_inline_nomsu
end
local compile = require('nomsu_compiler')
local nomsu_environment = Importer({
  NOMSU_COMPILER_VERSION = 12,
  NOMSU_SYNTAX_VERSION = max_parser_version,
  next = next,
  unpack = unpack or table.unpack,
  setmetatable = setmetatable,
  coroutine = coroutine,
  rawequal = rawequal,
  getmetatable = getmetatable,
  pcall = pcall,
  error = error,
  package = package,
  os = os,
  require = require,
  tonumber = tonumber,
  tostring = tostring,
  string = string,
  xpcall = xpcall,
  module = module,
  print = print,
  loadfile = loadfile,
  rawset = rawset,
  _VERSION = _VERSION,
  collectgarbage = collectgarbage,
  rawget = rawget,
  rawlen = rawlen,
  table = table,
  assert = assert,
  dofile = dofile,
  loadstring = loadstring,
  lua_type_of = type,
  select = select,
  math = math,
  io = io,
  load = load,
  pairs = pairs,
  ipairs = ipairs,
  jit = jit,
  _VERSION = _VERSION,
  bit = (jit or _VERSION == "Lua 5.2") and require('bitops') or nil,
  List = List,
  Dict = Dict,
  lpeg = lpeg,
  re = re,
  Files = Files,
  SyntaxTree = SyntaxTree,
  TESTS = Dict({ }),
  globals = Dict({ }),
  LuaCode = LuaCode,
  NomsuCode = NomsuCode,
  Source = Source,
  SOURCE_MAP = Importer({ }),
  _1_as_nomsu = tree_to_nomsu,
  _1_as_inline_nomsu = tree_to_inline_nomsu,
  compile = compile,
  _1_as_lua = compile,
  _1_forked = _1_forked,
  import_to_1_from = import_to_1_from,
  _1_parsed = function(nomsu_code)
    if type(nomsu_code) == 'string' then
      local filename = Files.spoof(nomsu_code)
      nomsu_code = NomsuCode(Source(filename, 1, #nomsu_code), nomsu_code)
    end
    local source = nomsu_code.source
    nomsu_code = tostring(nomsu_code)
    local version = nomsu_code:match("^#![^\n]*nomsu[ ]+-V[ ]*([0-9.]+)")
    local syntax_version = version and tonumber(version:match("^[0-9]+")) or max_parser_version
    local parse = Parsers[syntax_version] or Parsers[max_parser_version]
    local tree = parse(nomsu_code, source.filename)
    local find_errors
    find_errors = function(t)
      if t.type == "Error" then
        return coroutine.yield(t)
      else
        for k, v in pairs(t) do
          local _continue_0 = false
          repeat
            if not (SyntaxTree:is_instance(v)) then
              _continue_0 = true
              break
            end
            find_errors(v)
            _continue_0 = true
          until true
          if not _continue_0 then
            break
          end
        end
      end
    end
    local errs
    do
      local _accum_0 = { }
      local _len_0 = 1
      for err in coroutine.wrap(function()
        return find_errors(tree)
      end) do
        _accum_0[_len_0] = err
        _len_0 = _len_0 + 1
      end
      errs = _accum_0
    end
    local num_errs = #errs
    if num_errs > 0 then
      local err_strings
      do
        local _accum_0 = { }
        local _len_0 = 1
        for i, e in ipairs(errs) do
          if i <= 3 then
            _accum_0[_len_0] = pretty_error({
              title = "Parse error",
              error = e.error,
              hint = e.hint,
              source = e:get_source_file(),
              start = e.source.start,
              stop = e.source.stop,
              filename = e.source.filename
            })
            _len_0 = _len_0 + 1
          end
        end
        err_strings = _accum_0
      end
      if num_errs > #err_strings then
        table.insert(err_strings, "\027[31;1m +" .. tostring(num_errs - #err_strings) .. " additional errors...\027[0m\n")
      end
      error(table.concat(err_strings, '\n\n'), 0)
    end
    return tree
  end,
  run_1_in = function(to_run, environment)
    if type(to_run) == 'string' then
      local filename = Files.spoof(to_run)
      to_run = NomsuCode(Source(filename, 1, #to_run), to_run)
      local ret = environment.run_1_in(to_run, environment)
      return ret
    elseif NomsuCode:is_instance(to_run) then
      local tree = environment._1_parsed(to_run)
      if tree == nil then
        return nil
      end
      local ret = environment.run_1_in(tree, environment)
      return ret
    elseif SyntaxTree:is_instance(to_run) then
      local filename = to_run.source.filename:gsub("\n.*", "...")
      if to_run.type ~= "FileChunks" then
        to_run = {
          to_run
        }
      end
      local ret = nil
      for _index_0 = 1, #to_run do
        local chunk = to_run[_index_0]
        local lua = environment.compile(chunk)
        lua:declare_locals()
        lua:prepend("-- File: " .. tostring(filename) .. "\n")
        ret = environment.run_1_in(lua, environment)
      end
      return ret
    elseif LuaCode:is_instance(to_run) then
      local source = to_run.source
      local lua_string = to_run:text()
      local run_lua_fn, err = load(lua_string, tostring(source), "t", environment)
      if not run_lua_fn then
        local lines
        do
          local _accum_0 = { }
          local _len_0 = 1
          for i, line in ipairs(Files.get_lines(lua_string)) do
            _accum_0[_len_0] = ("%3d|%s"):format(i, line)
            _len_0 = _len_0 + 1
          end
          lines = _accum_0
        end
        local line_numbered_lua = table.concat(lines, "\n")
        error("Failed to compile generated code:\n\027[1;34m" .. tostring(line_numbered_lua) .. "\027[0m\n\n" .. tostring(err), 0)
      end
      local source_key = tostring(source)
      if not (environment.SOURCE_MAP[source_key]) then
        local map = { }
        local file = Files.read(source.filename)
        if not file then
          error("Failed to find file: " .. tostring(source.filename))
        end
        local nomsu_str = file:sub(source.start, source.stop)
        local lua_line = 1
        local nomsu_line = Files.get_line_number(file, source.start)
        local map_sources
        map_sources = function(s)
          if type(s) == 'string' then
            for nl in s:gmatch("\n") do
              map[lua_line] = map[lua_line] or nomsu_line
              lua_line = lua_line + 1
            end
          else
            if s.source and s.source.filename == source.filename then
              nomsu_line = Files.get_line_number(file, s.source.start)
            end
            local _list_0 = s.bits
            for _index_0 = 1, #_list_0 do
              local b = _list_0[_index_0]
              map_sources(b)
            end
          end
        end
        map_sources(to_run)
        map[lua_line] = map[lua_line] or nomsu_line
        map[0] = 0
        environment.SOURCE_MAP[source_key] = map
      end
      return run_lua_fn()
    else
      return error("Attempt to run unknown thing: " .. tostring(to_run))
    end
  end,
  FILE_CACHE = { },
  run_file_1_in = function(path, environment, optimization)
    if optimization == nil then
      optimization = 1
    end
    if environment.FILE_CACHE[path] then
      import_to_1_from(environment, environment.FILE_CACHE[path])
      return 
    end
    local mod = _1_forked(environment)
    assert(mod._1_parsed)
    mod._ENV = mod
    for _, filename in Files.walk(path) do
      local _continue_0 = false
      repeat
        if not (filename == "stdin" or filename:match("%.nom$")) then
          _continue_0 = true
          break
        end
        local lua_filename = filename:gsub("%.nom$", ".lua")
        local code
        if optimization ~= 0 and Files.read(lua_filename) then
          local file = Files.read(lua_filename)
          code = LuaCode(Source(filename, 1, #file), file)
        else
          local file = Files.read(filename)
          code = NomsuCode(Source(filename, 1, #file), file)
        end
        environment.run_1_in(code, mod)
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
    import_to_1_from(environment, mod)
    environment.FILE_CACHE[path] = mod
  end,
  compile_error_at = function(tree, err_msg, hint)
    if hint == nil then
      hint = nil
    end
    local err_str = pretty_error({
      title = "Compile error",
      error = err_msg,
      hint = hint,
      source = tree:get_source_file(),
      start = tree.source.start,
      stop = tree.source.stop,
      filename = tree.source.filename
    })
    return error(err_str, 0)
  end
})
nomsu_environment._ENV = nomsu_environment
SOURCE_MAP = nomsu_environment.SOURCE_MAP
return nomsu_environment
