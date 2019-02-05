local NomsuCode, LuaCode, Source
do
  local _obj_0 = require("code_obj")
  NomsuCode, LuaCode, Source = _obj_0.NomsuCode, _obj_0.LuaCode, _obj_0.Source
end
local List, Dict
do
  local _obj_0 = require('containers')
  List, Dict = _obj_0.List, _obj_0.Dict
end
local Text = require('text')
local SyntaxTree = require("syntax_tree")
local Files = require("files")
local Errhand = require("error_handling")
local C = require("colors")
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
local _1_as_text
_1_as_text = function(x)
  if x == true then
    return "yes"
  end
  if x == false then
    return "no"
  end
  return tostring(x)
end
local _1_as_list
_1_as_list = function(x)
  local mt = getmetatable(x)
  if mt.as_list then
    return mt.as_list(x)
  end
  return x
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
  LUA_VERSION = (jit and jit.version or _VERSION),
  LUA_API = _VERSION,
  Bit = (jit or _VERSION == "Lua 5.2") and require('bitops') or nil,
  a_List = List,
  a_Dict = Dict,
  Text = Text,
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
  _1_as_text = _1_as_text,
  _1_as_list = _1_as_list,
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
    local currently_running = { }
    for i = 2, 999 do
      local info = debug.getinfo(i, 'f')
      if not (info.func) then
        break
      end
      if info.func == self.Module then
        local n, upper_path = debug.getlocal(i, 3)
        table.insert(currently_running, upper_path)
        assert(n == "path")
        if upper_path == path then
          local circle = table.concat(currently_running, "', which imports '")
          local err_i = 2
          info = debug.getinfo(err_i)
          while info and (info.func == self.Module or info.func == self.use or info.func == self.export) do
            err_i = err_i + 1
            info = debug.getinfo(err_i)
          end
          fail_at((info or debug.getinfo(2)), "Circular import: File '" .. tostring(path) .. "' imports '" .. circle .. "'")
        end
      end
    end
    local mod = self:new_environment()
    mod.MODULE_NAME = package_name
    local code = Files.read(path)
    if path:match("%.lua$") then
      code = LuaCode:from(Source(path, 1, #code), code)
    else
      code = NomsuCode:from(Source(path, 1, #code), code)
    end
    local ret = mod:run(code)
    if ret ~= nil then
      mod = ret
    end
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
    if mod.COMPILE_RULES then
      for k, v in pairs(mod.COMPILE_RULES) do
        cr_imports[k] = v
      end
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
    if mod.COMPILE_RULES then
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
      lua_string = lua_string:gsub("^#![^\n]*\n", "")
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
        error("Failed to compile generated code:\n" .. tostring(C("bright blue", line_numbered_lua)) .. "\n\n" .. tostring(err), 0)
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
      return error("Attempt to run unknown thing: " .. _1_as_lua(to_run))
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
