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
      local _continue_0 = false
      repeat
        if path == "." and package.nomsupath ~= "." then
          _continue_0 = true
          break
        end
        peg_file = io.open(path .. "/nomsu." .. tostring(version) .. ".peg")
        if peg_file then
          break
        end
        _continue_0 = true
      until true
      if not _continue_0 then
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
local compile, compile_error
do
  local _obj_0 = require('nomsu_compiler')
  compile, compile_error = _obj_0.compile, _obj_0.compile_error
end
local _currently_running_files = List({ })
local nomsu_environment = Importer({
  NOMSU_COMPILER_VERSION = 12,
  NOMSU_SYNTAX_VERSION = max_parser_version,
  next = next,
  unpack = unpack or table.unpack,
  setmetatable = setmetatable,
  rawequal = rawequal,
  getmetatable = getmetatable,
  pcall = pcall,
  yield = coroutine.yield,
  resume = coroutine.resume,
  coroutine_status_of = coroutine.status,
  coroutine_wrap = coroutine.wrap,
  coroutine_from = coroutine.create,
  error = error,
  package = package,
  os = os,
  require = require,
  tonumber = tonumber,
  tostring = tostring,
  string = string,
  xpcall = xpcall,
  module = module,
  say = print,
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
  LuaCode_from = (function(src, ...)
    return LuaCode:from(src, ...)
  end),
  NomsuCode_from = (function(src, ...)
    return NomsuCode:from(src, ...)
  end),
  SOURCE_MAP = Importer({ }),
  _1_as_nomsu = tree_to_nomsu,
  _1_as_inline_nomsu = tree_to_inline_nomsu,
  compile = compile,
  _1_as_lua = compile,
  compile_error_at = compile_error,
  _1_forked = _1_forked,
  import_to_1_from = import_to_1_from,
  _1_parsed = function(nomsu_code, syntax_version)
    if type(nomsu_code) == 'string' then
      local filename = Files.spoof(nomsu_code)
      nomsu_code = NomsuCode:from(Source(filename, 1, #nomsu_code), nomsu_code)
    end
    local source = nomsu_code.source
    nomsu_code = tostring(nomsu_code)
    local version = nomsu_code:match("^#![^\n]*nomsu[ ]+-V[ ]*([0-9.]+)")
    if syntax_version then
      syntax_version = tonumber(syntax_version:match("^[0-9]+"))
    end
    syntax_version = syntax_version or (version and tonumber(version:match("^[0-9]+")) or max_parser_version)
    local parse = Parsers[syntax_version] or Parsers[max_parser_version]
    local tree = parse(nomsu_code, source.filename)
    if tree.shebang then
      tree.version = tree.version or tree.shebang:match("nomsu %-V[ ]*([%d.]*)")
    end
    local errs = { }
    local find_errors
    find_errors = function(t)
      if t.type == "Error" then
        errs[#errs + 1] = t
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
    find_errors(tree)
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
      to_run = NomsuCode:from(Source(filename, 1, #to_run), to_run)
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
      for chunk_no, chunk in ipairs(to_run) do
        local lua = environment.compile(chunk)
        lua:declare_locals()
        lua:prepend("-- File: " .. tostring(filename) .. " chunk #" .. tostring(chunk_no) .. "\n")
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
  run_file_1_in = function(path, environment, optimization, prefix)
    if prefix == nil then
      prefix = nil
    end
    if not optimization then
      optimization = environment.OPTIMIZATION
    end
    if environment.FILE_CACHE[path] then
      import_to_1_from(environment, environment.FILE_CACHE[path], prefix)
      return 
    end
    if _currently_running_files:has(path) then
      local i = _currently_running_files:index_of(path)
      _currently_running_files:add(path)
      local circle = _currently_running_files:from_1_to(i, -1)
      error("Circular import detected:\n           " .. circle:joined_with("\n..imports  "))
    end
    _currently_running_files:add(path)
    local mod = _1_forked(environment)
    local did_anything = false
    for nomsupath in package.nomsupath:gmatch("[^;]+") do
      local _continue_0 = false
      repeat
        do
          local full_path = nomsupath == "." and path or nomsupath .. "/" .. path
          local files = Files.list(full_path)
          if not (files) then
            _continue_0 = true
            break
          end
          for _index_0 = 1, #files do
            local filename = files[_index_0]
            local lua_filename = filename:gsub("%.nom$", ".lua")
            local code
            if optimization ~= 0 and Files.read(lua_filename) then
              local file = Files.read(lua_filename)
              code = LuaCode:from(Source(filename, 1, #file), file)
            else
              local file = Files.read(filename)
              code = NomsuCode:from(Source(filename, 1, #file), file)
            end
            environment.run_1_in(code, mod)
            did_anything = true
          end
          break
        end
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
    if not (did_anything) then
      error("File not found: " .. tostring(path), 0)
    end
    import_to_1_from(environment, mod, prefix)
    environment.FILE_CACHE[path] = mod
    return _currently_running_files:remove()
  end
})
nomsu_environment._ENV = nomsu_environment
SOURCE_MAP = nomsu_environment.SOURCE_MAP
return nomsu_environment
