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
local SyntaxTree = require("syntax_tree")
local Files = require("files")
local Errhand = require("error_handling")
local make_parser = require("parser")
local pretty_error = require("pretty_errors")
local make_tree
make_tree = function(tree, userdata)
  tree.source = Source(userdata.filename, tree.start, tree.stop)
  tree.start, tree.stop = nil, nil
  tree = SyntaxTree(tree)
  return tree
end
table.map = function(t, fn)
  return setmetatable((function()
    local _accum_0 = { }
    local _len_0 = 1
    for _, v in ipairs(t) do
      _accum_0[_len_0] = fn(v)
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)(), getmetatable(t))
end
local Parsers = { }
local max_parser_version = 0
for version = 1, 999 do
  local peg_file
  if package.nomsupath then
    local pegpath = package.nomsupath:gsub("lib/%?%.nom", "?.peg"):gsub("lib/%?%.lua", "?.peg")
    do
      local path = package.searchpath("nomsu." .. tostring(version), pegpath, "/")
      if path then
        peg_file = io.open(path)
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
local compile, fail_at
do
  local _obj_0 = require('nomsu_compiler')
  compile, fail_at = _obj_0.compile, _obj_0.fail_at
end
local _currently_running_files = List({ })
local _module_imports = { }
local _importer_mt = {
  __index = function(self, k)
    return _module_imports[self][k]
  end
}
local Importer
Importer = function(t, imports)
  _module_imports[t] = imports or { }
  t._IMPORTS = _module_imports[t]
  return setmetatable(t, _importer_mt)
end
local nomsu_environment
nomsu_environment = Importer({
  NOMSU_COMPILER_VERSION = 13,
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
  enhance_error = Errhand.enhance_error,
  SOURCE_MAP = { },
  getfenv = getfenv,
  _1_as_nomsu = tree_to_nomsu,
  _1_as_inline_nomsu = tree_to_inline_nomsu,
  compile = compile,
  at_1_fail = fail_at,
  exit = os.exit,
  quit = os.exit,
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
  Module = function(self, package_name)
    local path
    if package_name:match("%.nom$") or package_name:match("%.lua") then
      path = package_name
    else
      local err
      path, err = package.searchpath(package_name, package.nomsupath, "/")
      if not path then
        error(err)
      end
    end
    path = path:gsub("^%./", "")
    do
      local ret = package.nomsuloaded[package_name] or package.nomsuloaded[path]
      if ret then
        return ret
      end
    end
    if _currently_running_files:has(path) then
      local i = _currently_running_files:index_of(path)
      _currently_running_files:add(path)
      local circle = _currently_running_files:from_1_to(i, -1)
      error("Circular import detected:\n           " .. circle:joined_with("\n..imports  "))
    end
    local mod = self:new_environment()
    mod.MODULE_NAME = package_name
    local code = Files.read(path)
    if path:match("%.lua$") then
      code = LuaCode:from(Source(path, 1, #code), code)
    else
      code = NomsuCode:from(Source(path, 1, #code), code)
    end
    _currently_running_files:add(path)
    mod:run(code)
    _currently_running_files:pop()
    package.nomsuloaded[package_name] = mod
    package.nomsuloaded[path] = mod
    return mod
  end,
  use = function(self, package_name)
    local mod = self:Module(package_name)
    local imports = assert(_module_imports[self])
    for k, v in pairs(mod) do
      imports[k] = v
    end
    local cr_imports = assert(_module_imports[self.COMPILE_RULES])
    for k, v in pairs(mod.COMPILE_RULES) do
      cr_imports[k] = v
    end
    return mod
  end,
  export = function(self, package_name)
    local mod = self:Module(package_name)
    local imports = assert(_module_imports[self])
    for k, v in pairs(_module_imports[mod]) do
      if rawget(imports, k) == nil then
        imports[k] = v
      end
    end
    for k, v in pairs(mod) do
      if rawget(self, k) == nil then
        self[k] = v
      end
    end
    local cr_imports = assert(_module_imports[self.COMPILE_RULES])
    for k, v in pairs(_module_imports[mod.COMPILE_RULES]) do
      if rawget(cr_imports, k) == nil then
        cr_imports[k] = v
      end
    end
    for k, v in pairs(mod.COMPILE_RULES) do
      if rawget(self.COMPILE_RULES, k) == nil then
        self.COMPILE_RULES[k] = v
      end
    end
    return mod
  end,
  run = function(self, to_run)
    if not to_run then
      error("Need both something to run and an environment")
    end
    if type(to_run) == 'string' then
      local filename = Files.spoof(to_run)
      to_run = NomsuCode:from(Source(filename, 1, #to_run), to_run)
      return self:run(to_run)
    elseif NomsuCode:is_instance(to_run) then
      local tree = self._1_parsed(to_run)
      if tree == nil then
        return nil
      end
      return self:run(tree)
    elseif SyntaxTree:is_instance(to_run) then
      local filename = to_run.source.filename:gsub("\n.*", "...")
      if to_run.type ~= "FileChunks" then
        to_run = {
          to_run
        }
      end
      local ret = nil
      for chunk_no, chunk in ipairs(to_run) do
        local lua = self:compile(chunk)
        lua:declare_locals()
        lua:prepend("-- File: " .. tostring(filename) .. " chunk #" .. tostring(chunk_no) .. "\n")
        ret = self:run(lua)
      end
      return ret
    elseif LuaCode:is_instance(to_run) then
      local source = to_run.source
      local lua_string = to_run:text()
      local run_lua_fn, err = load(lua_string, tostring(source), "t", self)
      if not run_lua_fn then
        local lines
        do
          local _accum_0 = { }
          local _len_0 = 1
          for i, line in ipairs(lua_string:lines()) do
            _accum_0[_len_0] = ("%3d|%s"):format(i, line)
            _len_0 = _len_0 + 1
          end
          lines = _accum_0
        end
        local line_numbered_lua = table.concat(lines, "\n")
        error("Failed to compile generated code:\n\027[1;34m" .. tostring(line_numbered_lua) .. "\027[0m\n\n" .. tostring(err), 0)
      end
      local source_key = tostring(source)
      if not (self.SOURCE_MAP[source_key] or self.OPTIMIZATION >= 2) then
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
        self.SOURCE_MAP[source_key] = map
      end
      return run_lua_fn()
    else
      return error("Attempt to run unknown thing: " .. tostring(to_run))
    end
  end,
  new_environment = function()
    local env = Importer({ }, (function()
      local _tbl_0 = { }
      for k, v in pairs(nomsu_environment) do
        _tbl_0[k] = v
      end
      return _tbl_0
    end)())
    env._ENV = env
    env._G = env
    env.TESTS = Dict({ })
    env.COMPILE_RULES = Importer({ }, (function()
      local _tbl_0 = { }
      for k, v in pairs(nomsu_environment.COMPILE_RULES) do
        _tbl_0[k] = v
      end
      return _tbl_0
    end)())
    return env
  end
})
nomsu_environment._ENV = nomsu_environment
nomsu_environment._G = nomsu_environment
nomsu_environment.COMPILE_RULES = Importer(require('bootstrap'))
nomsu_environment.MODULE_NAME = "nomsu"
SOURCE_MAP = nomsu_environment.SOURCE_MAP
return nomsu_environment
