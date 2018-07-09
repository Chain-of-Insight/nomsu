local lpeg = require('lpeg')
local re = require('re')
local utils = require('utils')
local files = require('files')
local repr, stringify, equivalent
repr, stringify, equivalent = utils.repr, utils.stringify, utils.equivalent
colors = require('consolecolors')
colored = setmetatable({ }, {
  __index = function(_, color)
    return (function(msg)
      return colors[color] .. tostring(msg or '') .. colors.reset
    end)
  end
})
local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local unpack = unpack or table.unpack
local match, sub, gsub, format, byte, find
do
  local _obj_0 = string
  match, sub, gsub, format, byte, find = _obj_0.match, _obj_0.sub, _obj_0.gsub, _obj_0.format, _obj_0.byte, _obj_0.find
end
local NomsuCode, LuaCode, Source
do
  local _obj_0 = require("code_obj")
  NomsuCode, LuaCode, Source = _obj_0.NomsuCode, _obj_0.LuaCode, _obj_0.Source
end
local AST = require("nomsu_tree")
local Parser = require("parser")
SOURCE_MAP = { }
string.as_lua_id = function(str)
  local argnum = 0
  str = gsub(str, "x([0-9A-F][0-9A-F])", "x\0%1")
  str = gsub(str, "%W", function(c)
    if c == ' ' then
      return '_'
    elseif c == '%' then
      argnum = argnum + 1
      return tostring(argnum)
    else
      return format("x%02X", byte(c))
    end
  end)
  return '_' .. str
end
table.map = function(self, fn)
  local _accum_0 = { }
  local _len_0 = 1
  for _, v in ipairs(self) do
    _accum_0[_len_0] = fn(v)
    _len_0 = _len_0 + 1
  end
  return _accum_0
end
table.fork = function(t, values)
  return setmetatable(values or { }, {
    __index = t
  })
end
do
  local STRING_METATABLE = getmetatable("")
  STRING_METATABLE.__add = function(self, other)
    return self .. stringify(other)
  end
  STRING_METATABLE.__index = function(self, i)
    local ret = string[i]
    if ret ~= nil then
      return ret
    end
    if type(i) == 'number' then
      return sub(self, i, i)
    elseif type(i) == 'table' then
      return sub(self, i[1], i[2])
    end
  end
end
local _list_mt = {
  __eq = equivalent,
  __tostring = function(self)
    return "[" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #self do
        local b = self[_index_0]
        _accum_0[_len_0] = repr(b)
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), ", ") .. "]"
  end,
  __lt = function(self, other)
    assert(type(self) == 'table' and type(other) == 'table', "Incompatible types for comparison")
    for i = 1, math.max(#self, #other) do
      if self[i] < other[i] then
        return true
      elseif self[i] > other[i] then
        return false
      end
    end
    return false
  end,
  __le = function(self, other)
    assert(type(self) == 'table' and type(other) == 'table', "Incompatible types for comparison")
    for i = 1, math.max(#self, #other) do
      if self[i] < other[i] then
        return true
      elseif self[i] > other[i] then
        return false
      end
    end
    return true
  end
}
local list
list = function(t)
  return setmetatable(t, _list_mt)
end
local _dict_mt = {
  __eq = equivalent,
  __tostring = function(self)
    return "{" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for k, v in pairs(self) do
        _accum_0[_len_0] = tostring(repr(k)) .. ": " .. tostring(repr(v))
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), ", ") .. "}"
  end
}
local dict
dict = function(t)
  return setmetatable(t, _dict_mt)
end
local MAX_LINE = 80
local NomsuCompiler = setmetatable({ }, {
  __index = function(self, k)
    do
      local _self = rawget(self, "self")
      if _self then
        return _self[k]
      else
        return nil
      end
    end
  end
})
do
  NomsuCompiler.NOMSU_COMPILER_VERSION = 3
  NomsuCompiler.NOMSU_SYNTAX_VERSION = Parser.version
  NomsuCompiler._ENV = NomsuCompiler
  NomsuCompiler.nomsu = NomsuCompiler
  NomsuCompiler.parse = function(self, ...)
    return Parser.parse(...)
  end
  NomsuCompiler.can_optimize = function()
    return false
  end
  local to_add = {
    repr = repr,
    stringify = stringify,
    utils = utils,
    lpeg = lpeg,
    re = re,
    files = files,
    next = next,
    unpack = unpack,
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
    type = type,
    select = select,
    debug = debug,
    math = math,
    io = io,
    load = load,
    pairs = pairs,
    ipairs = ipairs,
    list = list,
    dict = dict
  }
  for k, v in pairs(to_add) do
    NomsuCompiler[k] = v
  end
  for k, v in pairs(AST) do
    NomsuCompiler[k] = v
  end
  NomsuCompiler.LuaCode = LuaCode
  NomsuCompiler.NomsuCode = NomsuCode
  NomsuCompiler.Source = Source
  NomsuCompiler.ALIASES = setmetatable({ }, {
    __mode = "k"
  })
  NomsuCompiler.LOADED = { }
  NomsuCompiler.AST = AST
  NomsuCompiler.compile_error = function(self, tok, err_format_string, ...)
    local file = files.read(tok.source.filename)
    local line_starts = files.get_line_starts(file)
    local line_no = files.get_line_number(file, tok.source.start)
    local line_start = line_starts[line_no]
    local src = colored.dim(file:sub(line_start, tok.source.start - 1))
    src = src .. colored.underscore(colored.bright(colored.red(file:sub(tok.source.start, tok.source.stop - 1))))
    local end_of_line = (line_starts[files.get_line_number(file, tok.source.stop) + 1] or 0) - 1
    src = src .. colored.dim(file:sub(tok.source.stop, end_of_line - 1))
    src = '    ' .. src:gsub('\n', '\n    ')
    local err_msg = err_format_string:format(src, ...)
    return error(tostring(tok.source.filename) .. ":" .. tostring(line_no) .. ": " .. err_msg, 0)
  end
  local math_expression = re.compile([[ ([+-] " ")* "%" (" " [*/^+-] (" " [+-])* " %")+ !. ]])
  local add_lua_bits
  add_lua_bits = function(self, lua, code)
    for _index_0 = 1, #code do
      local bit = code[_index_0]
      if type(bit) == "string" then
        lua:append(bit)
      else
        local bit_lua = self:compile(bit)
        if not (bit_lua.is_value) then
          self:compile_error(bit, "Cannot use:\n%s\nas a string interpolation value, since it's not an expression.")
        end
        lua:append(bit_lua)
      end
    end
    return lua
  end
  local add_lua_string_bits
  add_lua_string_bits = function(self, lua, code)
    local line_len = 0
    if code.type ~= "Text" then
      lua:append(", ", self:compile(code))
      return 
    end
    for _index_0 = 1, #code do
      local bit = code[_index_0]
      local bit_lua
      if type(bit) == "string" then
        bit_lua = repr(bit)
      else
        bit_lua = self:compile(bit)
        if not (bit_lua.is_value) then
          self:compile_error(bit, "Cannot use:\n%s\nas a string interpolation value, since it's not an expression.")
        end
        bit_lua = bit_lua
      end
      line_len = line_len + #tostring(bit_lua)
      if line_len > MAX_LINE then
        lua:append(",\n    ")
        line_len = 4
      else
        lua:append(", ")
      end
      lua:append(bit_lua)
    end
  end
  NomsuCompiler.COMPILE_ACTIONS = setmetatable({
    ["# compile math expr #"] = function(self, tree, ...)
      local lua = LuaCode.Value(tree.source)
      for i, tok in ipairs(tree) do
        if type(tok) == 'string' then
          lua:append(tok)
        else
          local tok_lua = self:compile(tok)
          if not (tok_lua.is_value) then
            self:compile_error(tok, "Non-expression value inside math expression:\n%s")
          end
          if tok.type == "Action" then
            tok_lua:parenthesize()
          end
          lua:append(tok_lua)
        end
        if i < #tree then
          lua:append(" ")
        end
      end
      return lua
    end,
    ["Lua %"] = function(self, tree, _code)
      local lua = LuaCode.Value(_code.source, "LuaCode(", repr(tostring(_code.source)))
      add_lua_string_bits(self, lua, _code)
      lua:append(")")
      return lua
    end,
    ["Lua value %"] = function(self, tree, _code)
      local lua = LuaCode.Value(_code.source, "LuaCode.Value(", repr(tostring(_code.source)))
      add_lua_string_bits(self, lua, _code)
      lua:append(")")
      return lua
    end,
    ["lua > %"] = function(self, tree, _code)
      if _code.type ~= "Text" then
        return LuaCode(tree.source, "nomsu:run_lua(", self:compile(_code), ");")
      end
      return add_lua_bits(self, LuaCode(tree.source), _code)
    end,
    ["= lua %"] = function(self, tree, _code)
      if _code.type ~= "Text" then
        return LuaCode.Value(tree.source, "nomsu:run_lua(", self:compile(_code), ":as_statements('return '))")
      end
      return add_lua_bits(self, LuaCode.Value(tree.source), _code)
    end,
    ["use %"] = function(self, tree, _path)
      if _path.type == 'Text' and #_path == 1 and type(_path[1]) == 'string' then
        local path = _path[1]
        for f in files.walk(path) do
          self:run_file(f)
        end
      end
      return LuaCode(tree.source, "for f in files.walk(", self:compile(_path), ") do nomsu:run_file(f) end")
    end
  }, {
    __index = function(self, stub)
      if math_expression:match(stub) then
        return self["# compile math expr #"]
      end
    end
  })
  NomsuCompiler.run = function(self, to_run, source)
    if source == nil then
      source = nil
    end
    source = source or (to_run.source or Source(to_run, 1, #to_run))
    if not files.read(source.filename) then
      files.spoof(source.filename, to_run)
    end
    local tree
    if AST.is_syntax_tree(to_run) then
      tree = to_run
    else
      tree = self:parse(to_run, source)
    end
    if tree == nil then
      return nil
    end
    if tree.type == "FileChunks" then
      local ret = nil
      local all_lua = { }
      for _index_0 = 1, #tree do
        local chunk = tree[_index_0]
        local lua = self:compile(chunk):as_statements("return ")
        lua:declare_locals()
        lua:prepend("-- File: " .. tostring(source.filename:gsub("\n.*", "...")) .. "\n")
        insert(all_lua, tostring(lua))
        ret = self:run_lua(lua)
      end
      return ret
    else
      local lua = self:compile(tree):as_statements("return ")
      lua:declare_locals()
      lua:prepend("-- File: " .. tostring(source.filename:gsub("\n.*", "...")) .. "\n")
      return self:run_lua(lua)
    end
  end
  local _running_files = { }
  NomsuCompiler.run_file = function(self, filename)
    if self.LOADED[filename] then
      return self.LOADED[filename]
    end
    for i, running in ipairs(_running_files) do
      if running == filename then
        local loop
        do
          local _accum_0 = { }
          local _len_0 = 1
          for j = i, #_running_files do
            _accum_0[_len_0] = _running_files[j]
            _len_0 = _len_0 + 1
          end
          loop = _accum_0
        end
        insert(loop, filename)
        error("Circular import, this loops forever: " .. tostring(concat(loop, " -> ")) .. "...")
      end
    end
    insert(_running_files, filename)
    local ret = nil
    if match(filename, "%.lua$") then
      local file = assert(files.read(filename), "Could not find file: " .. tostring(filename))
      ret = self:run_lua(file, Source(filename, 1, #file))
    elseif match(filename, "%.nom$") or match(filename, "^/dev/fd/[012]$") then
      local ran_lua
      if self.can_optimize(filename) then
        local lua_filename = gsub(filename, "%.nom$", ".lua")
        do
          local file = files.read(lua_filename)
          if file then
            ret = self:run_lua(file, Source(lua_filename, 1, #file))
            ran_lua = true
          end
        end
      end
      if not (ran_lua) then
        local file = files.read(filename)
        if not file then
          error("File does not exist: " .. tostring(filename), 0)
        end
        ret = self:run(file, Source(filename, 1, #file))
      end
    else
      error("Invalid filetype for " .. tostring(filename), 0)
    end
    self.LOADED[filename] = ret or true
    remove(_running_files)
    self.LOADED[filename] = ret or true
    return ret
  end
  NomsuCompiler.run_lua = function(self, lua, source)
    if source == nil then
      source = nil
    end
    local lua_string = tostring(lua)
    local run_lua_fn, err = load(lua_string, nil and tostring(source or lua.source), "t", self)
    if not run_lua_fn then
      local line_numbered_lua = concat((function()
        local _accum_0 = { }
        local _len_0 = 1
        for i, line in ipairs(files.get_lines(lua_string)) do
          _accum_0[_len_0] = format("%3d|%s", i, line)
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), "\n")
      error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(colored.onblack(line_numbered_lua)))) .. "\n\n" .. tostring(err), 0)
    end
    local source_key = tostring(source or lua.source)
    if not (SOURCE_MAP[source_key]) then
      local map = { }
      local offset = 1
      source = source or lua.source
      local file = files.read(source.filename)
      if not file then
        error("Failed to find file: " .. tostring(source.filename))
      end
      local nomsu_str = tostring(file:sub(source.start, source.stop))
      local lua_line = 1
      local nomsu_line = files.get_line_number(nomsu_str, source.start)
      local fn
      fn = function(s)
        if type(s) == 'string' then
          for nl in s:gmatch("\n") do
            map[lua_line] = map[lua_line] or nomsu_line
            lua_line = lua_line + 1
          end
        else
          local old_line = nomsu_line
          if s.source then
            nomsu_line = files.get_line_number(nomsu_str, s.source.start)
          end
          local _list_0 = s.bits
          for _index_0 = 1, #_list_0 do
            local b = _list_0[_index_0]
            fn(b)
          end
        end
      end
      fn(lua)
      map[lua_line] = map[lua_line] or nomsu_line
      map[0] = 0
      SOURCE_MAP[source_key] = map
    end
    return run_lua_fn()
  end
  NomsuCompiler.compile = function(self, tree)
    local _exp_0 = tree.type
    if "Action" == _exp_0 then
      local stub = tree.stub
      do
        local compile_action = self.COMPILE_ACTIONS[stub]
        if compile_action then
          local args
          do
            local _accum_0 = { }
            local _len_0 = 1
            for _index_0 = 1, #tree do
              local arg = tree[_index_0]
              if type(arg) ~= "string" then
                _accum_0[_len_0] = arg
                _len_0 = _len_0 + 1
              end
            end
            args = _accum_0
          end
          local ret = compile_action(self, tree, unpack(args))
          if not ret then
            self:compile_error(tree, "Compile-time action:\n%s\nfailed to produce any Lua")
          end
          return ret
        end
      end
      local lua = LuaCode.Value(tree.source, "A", string.as_lua_id(stub), "(")
      local args = { }
      for i, tok in ipairs(tree) do
        local _continue_0 = false
        repeat
          if type(tok) == "string" then
            _continue_0 = true
            break
          end
          local arg_lua = self:compile(tok)
          if not (arg_lua.is_value) then
            self:compile_error(tok, "Cannot use:\n%s\nas an argument to %s, since it's not an expression, it produces: %s", stub, repr(arg_lua))
          end
          insert(args, arg_lua)
          _continue_0 = true
        until true
        if not _continue_0 then
          break
        end
      end
      lua:concat_append(args, ", ")
      lua:append(")")
      return lua
    elseif "EscapedNomsu" == _exp_0 then
      local make_tree
      make_tree = function(t)
        if not (AST.is_syntax_tree(t)) then
          return repr(t)
        end
        local bits
        do
          local _accum_0 = { }
          local _len_0 = 1
          for _index_0 = 1, #t do
            local bit = t[_index_0]
            _accum_0[_len_0] = make_tree(bit)
            _len_0 = _len_0 + 1
          end
          bits = _accum_0
        end
        return t.type .. "(" .. repr(tostring(t.source)) .. ", " .. table.concat(bits, ", ") .. ")"
      end
      return LuaCode.Value(tree.source, make_tree(tree[1]))
    elseif "Block" == _exp_0 then
      local lua = LuaCode(tree.source)
      lua:concat_append((function()
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #tree do
          local line = tree[_index_0]
          _accum_0[_len_0] = self:compile(line):as_statements()
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), "\n")
      return lua
    elseif "Text" == _exp_0 then
      local lua = LuaCode.Value(tree.source)
      local string_buffer = ""
      for i, bit in ipairs(tree) do
        local _continue_0 = false
        repeat
          if type(bit) == "string" then
            string_buffer = string_buffer .. bit
            _continue_0 = true
            break
          end
          if string_buffer ~= "" then
            if #lua.bits > 0 then
              lua:append("..")
            end
            lua:append(repr(string_buffer))
            string_buffer = ""
          end
          local bit_lua = self:compile(bit)
          if not (bit_lua.is_value) then
            local src = '    ' .. gsub(tostring(recurse(bit)), '\n', '\n    ')
            local line = tostring(bit.source.filename) .. ":" .. tostring(files.get_line_number(files.read(bit.source.filename), bit.source.start))
            self:compile_error(bit, "Cannot use:\n%s\nas a string interpolation value, since it's not an expression.")
          end
          if #lua.bits > 0 then
            lua:append("..")
          end
          if bit.type ~= "Text" then
            bit_lua = LuaCode.Value(bit.source, "stringify(", bit_lua, ")")
          end
          lua:append(bit_lua)
          _continue_0 = true
        until true
        if not _continue_0 then
          break
        end
      end
      if string_buffer ~= "" or #lua.bits == 0 then
        if #lua.bits > 0 then
          lua:append("..")
        end
        lua:append(repr(string_buffer))
      end
      if #lua.bits > 1 then
        lua:parenthesize()
      end
      return lua
    elseif "List" == _exp_0 then
      local lua = LuaCode.Value(tree.source, "list{")
      local items = { }
      for i, item in ipairs(tree) do
        local item_lua = self:compile(item)
        if not (item_lua.is_value) then
          self:compile_error(item, "Cannot use:\n%s\nas a list item, since it's not an expression.")
        end
        items[i] = item_lua
      end
      lua:concat_append(items, ", ", ",\n  ")
      lua:append("}")
      return lua
    elseif "Dict" == _exp_0 then
      local lua = LuaCode.Value(tree.source, "dict{")
      lua:concat_append((function()
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #tree do
          local e = tree[_index_0]
          _accum_0[_len_0] = self:compile(e)
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), ", ", ",\n  ")
      lua:append("}")
      return lua
    elseif "DictEntry" == _exp_0 then
      local key, value = tree[1], tree[2]
      local key_lua = self:compile(key)
      if not (key_lua.is_value) then
        self:compile_error(tree[1], "Cannot use:\n%s\nas a dict key, since it's not an expression.")
      end
      local value_lua = value and self:compile(value) or LuaCode.Value(key.source, "true")
      if not (value_lua.is_value) then
        self:compile_error(tree[2], "Cannot use:\n%s\nas a dict value, since it's not an expression.")
      end
      local key_str = match(tostring(key_lua), [=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
      if key_str then
        return LuaCode(tree.source, key_str, "=", value_lua)
      elseif sub(tostring(key_lua), 1, 1) == "[" then
        return LuaCode(tree.source, "[ ", key_lua, "]=", value_lua)
      else
        return LuaCode(tree.source, "[", key_lua, "]=", value_lua)
      end
    elseif "IndexChain" == _exp_0 then
      local lua = self:compile(tree[1])
      if not (lua.is_value) then
        self:compile_error(tree[1], "Cannot index:\n%s\nsince it's not an expression.")
      end
      local first_char = sub(tostring(lua), 1, 1)
      if first_char == "{" or first_char == '"' or first_char == "[" then
        lua:parenthesize()
      end
      for i = 2, #tree do
        local key = tree[i]
        local key_lua = self:compile(key)
        if not (key_lua.is_value) then
          self:compile_error(key, "Cannot use:\n%s\nas an index, since it's not an expression.")
        end
        local key_lua_str = tostring(key_lua)
        do
          local lua_id = match(key_lua_str, "^['\"]([a-zA-Z_][a-zA-Z0-9_]*)['\"]$")
          if lua_id then
            lua:append("." .. tostring(lua_id))
          elseif sub(key_lua_str, 1, 1) == '[' then
            lua:append("[ ", key_lua, " ]")
          else
            lua:append("[", key_lua, "]")
          end
        end
      end
      return lua
    elseif "Number" == _exp_0 then
      return LuaCode.Value(tree.source, tostring(tree[1]))
    elseif "Var" == _exp_0 then
      return LuaCode.Value(tree.source, string.as_lua_id(tree[1]))
    elseif "FileChunks" == _exp_0 then
      return error("Cannot convert FileChunks to a single block of lua, since each chunk's " .. "compilation depends on the earlier chunks")
    else
      return error("Unknown type: " .. tostring(tree.type))
    end
  end
  NomsuCompiler.tree_to_nomsu = function(self, tree, options)
    options = options or { }
    local comments = options.comments
    if comments == nil and tree.comments then
      do
        local _accum_0 = { }
        local _len_0 = 1
        for p, c in pairs(tree.comments) do
          _accum_0[_len_0] = {
            comment = c,
            pos = p
          }
          _len_0 = _len_0 + 1
        end
        comments = _accum_0
      end
      table.sort(comments, function(a, b)
        return a.pos > b.pos
      end)
    end
    local recurse
    recurse = function(t, opts)
      opts = opts or { }
      opts.comments = comments
      return self:tree_to_nomsu(t, opts)
    end
    local pop_comments
    pop_comments = function(pos)
      if not (comments) then
        return ''
      end
      local nomsu = NomsuCode(tree.source)
      while #comments > 0 and comments[#comments].pos <= pos do
        local comment = table.remove(comments)
        nomsu:append("#" .. (gsub(comment.comment, "\n", "\n    ")) .. "\n")
      end
      if #nomsu.bits == 0 then
        return ''
      end
      return nomsu
    end
    local inline, can_use_colon = options.inline, options.can_use_colon
    local _exp_0 = tree.type
    if "FileChunks" == _exp_0 then
      if inline then
        return nil
      end
      local nomsu = NomsuCode(tree.source)
      for i, chunk in ipairs(tree) do
        if i > 1 then
          nomsu:append("\n\n" .. tostring(("~"):rep(80)) .. "\n\n")
        end
        nomsu:append(pop_comments(chunk.source.start))
        nomsu:append(recurse(chunk))
      end
      return nomsu
    elseif "Action" == _exp_0 then
      if inline then
        local nomsu = NomsuCode(tree.source)
        for i, bit in ipairs(tree) do
          if type(bit) == "string" then
            if i > 1 then
              nomsu:append(" ")
            end
            nomsu:append(bit)
          else
            local arg_nomsu = recurse(bit, {
              inline = true
            })
            if not (arg_nomsu) then
              return nil
            end
            if not (i == 1) then
              nomsu:append(" ")
            end
            if bit.type == "Action" or bit.type == "Block" then
              arg_nomsu:parenthesize()
            end
            nomsu:append(arg_nomsu)
          end
        end
        return nomsu
      else
        local nomsu = NomsuCode(tree.source)
        local next_space = ""
        local line_len, last_colon = 0, nil
        for i, bit in ipairs(tree) do
          if type(bit) == "string" then
            line_len = line_len + #next_space + #bit
            nomsu:append(next_space, bit)
            next_space = " "
          else
            local arg_nomsu
            if last_colon == i - 1 and bit.type == "Action" then
              arg_nomsu = nil
            elseif bit.type == "Block" then
              arg_nomsu = nil
            else
              arg_nomsu = recurse(bit, {
                inline = true
              })
            end
            if arg_nomsu and line_len + #tostring(arg_nomsu) < MAX_LINE then
              if bit.type == "Action" then
                if can_use_colon and i > 1 then
                  nomsu:append(match(next_space, "[^ ]*"), ": ", arg_nomsu)
                  next_space = "\n.."
                  line_len = 2
                  last_colon = i
                else
                  nomsu:append(next_space, "(", arg_nomsu, ")")
                  line_len = line_len + #next_space + 2 + #tostring(arg_nomsu)
                  next_space = " "
                end
              else
                nomsu:append(next_space, arg_nomsu)
                line_len = line_len + #next_space + #tostring(arg_nomsu)
                next_space = " "
              end
            else
              arg_nomsu = recurse(bit, {
                can_use_colon = true
              })
              if not (arg_nomsu) then
                return nil
              end
              if bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
                if i == 1 then
                  arg_nomsu = NomsuCode(bit.source, "(..)\n    ", pop_comments(bit.source.start), arg_nomsu)
                else
                  arg_nomsu = NomsuCode(bit.source, "\n    ", pop_comments(bit.source.start), arg_nomsu)
                end
              end
              if last_colon == i - 1 and (bit.type == "Action" or bit.type == "Block") then
                next_space = ""
              end
              nomsu:append(next_space, arg_nomsu)
              next_space = "\n.."
              line_len = 2
            end
            if next_space == " " and #(match(tostring(nomsu), "[^\n]*$")) > MAX_LINE then
              next_space = "\n.."
            end
          end
        end
        return nomsu
      end
    elseif "EscapedNomsu" == _exp_0 then
      local nomsu = recurse(tree[1], {
        inline = true
      })
      if nomsu == nil and not inline then
        nomsu = recurse(tree[1])
        return nomsu and NomsuCode(tree.source, "\\:\n    ", pop_comments(tree.source.start), nomsu)
      end
      return nomsu and NomsuCode(tree.source, "\\(", nomsu, ")")
    elseif "Block" == _exp_0 then
      if inline then
        local nomsu = NomsuCode(tree.source)
        for i, line in ipairs(tree) do
          if i > 1 then
            nomsu:append("; ")
          end
          local line_nomsu = recurse(line, {
            inline = true
          })
          if not (line_nomsu) then
            return nil
          end
          nomsu:append(line_nomsu)
        end
        return nomsu
      end
      local nomsu = NomsuCode(tree.source)
      for i, line in ipairs(tree) do
        nomsu:append(pop_comments(line.source.start))
        line = assert(recurse(line, {
          can_use_colon = true
        }), "Could not convert line to nomsu")
        nomsu:append(line)
        if i < #tree then
          nomsu:append("\n")
          if match(tostring(line), "\n") then
            nomsu:append("\n")
          end
        end
      end
      return nomsu
    elseif "Text" == _exp_0 then
      if inline then
        local nomsu = NomsuCode(tree.source, '"')
        for _index_0 = 1, #tree do
          local bit = tree[_index_0]
          if type(bit) == 'string' then
            nomsu:append((gsub(gsub(gsub(bit, "\\", "\\\\"), "\n", "\\n"), '"', '\\"')))
          else
            local interp_nomsu = recurse(bit, {
              inline = true
            })
            if interp_nomsu then
              if bit.type ~= "Var" and bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
                interp_nomsu:parenthesize()
              end
              nomsu:append("\\", interp_nomsu)
            else
              return nil
            end
          end
        end
        nomsu:append('"')
        return nomsu
      else
        local inline_version = recurse(tree, {
          inline = true
        })
        if inline_version and #inline_version <= MAX_LINE then
          return inline_version
        end
        local nomsu = NomsuCode(tree.source, '".."\n    ')
        for i, bit in ipairs(tree) do
          if type(bit) == 'string' then
            local bit_lines = files.get_lines(bit)
            for j, line in ipairs(bit_lines) do
              if j > 1 then
                nomsu:append("\n    ")
              end
              if #line > 1.25 * MAX_LINE then
                local remainder = line
                while #remainder > 0 do
                  local split = find(remainder, " ", MAX_LINE, true)
                  if split then
                    local chunk
                    chunk, remainder = sub(remainder, 1, split), sub(remainder, split + 1, -1)
                    nomsu:append(chunk)
                  elseif #remainder > 1.75 * MAX_LINE then
                    split = math.floor(1.5 * MAX_LINE)
                    local chunk
                    chunk, remainder = sub(remainder, 1, split), sub(remainder, split + 1, -1)
                    nomsu:append(chunk)
                  else
                    nomsu:append(remainder)
                    break
                  end
                  if #remainder > 0 then
                    nomsu:append("\\\n    ..")
                  end
                end
              else
                nomsu:append(line)
              end
            end
          else
            local interp_nomsu = recurse(bit, {
              inline = true
            })
            if interp_nomsu then
              if bit.type ~= "Var" and bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
                interp_nomsu:parenthesize()
              end
              nomsu:append("\\", interp_nomsu)
            else
              interp_nomsu = assert(recurse(bit))
              if not (interp_nomsu) then
                return nil
              end
              nomsu:append("\\\n        ", interp_nomsu)
              if i < #tree then
                nomsu:append("\n    ..")
              end
            end
          end
        end
        return nomsu
      end
    elseif "List" == _exp_0 then
      if inline then
        local nomsu = NomsuCode(tree.source, "[")
        for i, item in ipairs(tree) do
          local item_nomsu = recurse(item, {
            inline = true
          })
          if not (item_nomsu) then
            return nil
          end
          if i > 1 then
            nomsu:append(", ")
          end
          nomsu:append(item_nomsu)
        end
        nomsu:append("]")
        return nomsu
      else
        local inline_version = recurse(tree, {
          inline = true
        })
        if inline_version and #inline_version <= MAX_LINE then
          return inline_version
        end
        local nomsu = NomsuCode(tree.source, "[..]")
        local line = NomsuCode(tree.source, "\n    ")
        local line_comments
        if #tree > 0 then
          line_comments = pop_comments(tree[1].source.start)
        else
          line_comments = ''
        end
        for i, item in ipairs(tree) do
          local item_nomsu = recurse(item, {
            inline = true
          })
          if item_nomsu and #tostring(line) + #", " + #item_nomsu <= MAX_LINE then
            if #line.bits > 1 then
              line:append(", ")
            end
            line:append(item_nomsu)
          else
            if not (item_nomsu) then
              item_nomsu = recurse(item)
              if not (item_nomsu) then
                return nil
              end
            end
            if #line.bits > 1 then
              if #tostring(line_comments) > 0 then
                nomsu:append('\n    ', line_comments)
              end
              nomsu:append(line)
              line = NomsuCode(line.source, "\n    ")
              if i < #tree then
                line_comments = pop_comments(tree[i + 1].source.start)
              else
                line_comments = ''
              end
            end
            line:append(item_nomsu)
          end
        end
        if #line.bits > 1 then
          nomsu:append(line)
        end
        return nomsu
      end
    elseif "Dict" == _exp_0 then
      if inline then
        local nomsu = NomsuCode(tree.source, "{")
        for i, entry in ipairs(tree) do
          local entry_nomsu = recurse(entry, {
            inline = true
          })
          if not (entry_nomsu) then
            return nil
          end
          if i > 1 then
            nomsu:append(", ")
          end
          nomsu:append(entry_nomsu)
        end
        nomsu:append("}")
        return nomsu
      else
        local inline_version = recurse(tree, {
          inline = true
        })
        if inline_version then
          return inline_version
        end
        local nomsu = NomsuCode(tree.source, "{..}")
        local line = NomsuCode(tree.source, "\n    ")
        local line_comments
        if #tree > 0 then
          line_comments = pop_comments(tree[1].source.start)
        else
          line_comments = ''
        end
        for i, entry in ipairs(tree) do
          local entry_nomsu = recurse(entry)
          if not (entry_nomsu) then
            return nil
          end
          if #line + #tostring(entry_nomsu) <= MAX_LINE then
            if #line.bits > 1 then
              line:append(", ")
            end
            line:append(entry_nomsu)
          else
            if #line.bits > 1 then
              if #tostring(line_comments) > 0 then
                nomsu:append("\n    ", line_comments)
              end
              nomsu:append(line)
              line = NomsuCode(line.source, "\n    ")
              if i < #tree then
                line_comments = pop_comments(tree[1].source.start)
              else
                line_comments = ''
              end
            end
            line:append(entry_nomsu)
          end
        end
        if #line.bits > 1 then
          nomsu:append(line)
        end
        return nomsu
      end
    elseif "DictEntry" == _exp_0 then
      local key, value = tree[1], tree[2]
      local key_nomsu = recurse(key, {
        inline = true
      })
      if not (key_nomsu) then
        return nil
      end
      if key.type == "Action" or key.type == "Block" then
        key_nomsu:parenthesize()
      end
      local value_nomsu
      if value then
        value_nomsu = recurse(value, {
          inline = true
        })
      else
        value_nomsu = NomsuCode(tree.source, "")
      end
      if inline and not value_nomsu then
        return nil
      end
      if not value_nomsu then
        if inline then
          return nil
        end
        value_nomsu = recurse(value)
        if not (value_nomsu) then
          return nil
        end
      end
      return NomsuCode(tree.source, key_nomsu, ":", value_nomsu)
    elseif "IndexChain" == _exp_0 then
      local nomsu = NomsuCode(tree.source)
      for i, bit in ipairs(tree) do
        if i > 1 then
          nomsu:append(".")
        end
        local bit_nomsu
        if bit.type == "Text" and #bit == 1 and type(bit[1]) == 'string' then
          if bit[1]:match("[_a-zA-Z][_a-zA-Z0-9]*") then
            bit_nomsu = bit[1]
          end
        end
        if not (bit_nomsu) then
          bit_nomsu = recurse(bit, {
            inline = true
          })
        end
        if not (bit_nomsu) then
          return nil
        end
        local _exp_1 = bit.type
        if "Action" == _exp_1 or "Block" == _exp_1 or "IndexChain" == _exp_1 then
          bit_nomsu:parenthesize()
        elseif "Number" == _exp_1 then
          if i < #tree then
            bit_nomsu:parenthesize()
          end
        end
        nomsu:append(bit_nomsu)
      end
      return nomsu
    elseif "Number" == _exp_0 then
      return NomsuCode(tree.source, tostring(tree[1]))
    elseif "Var" == _exp_0 then
      return NomsuCode(tree.source, "%", tree[1])
    else
      return error("Unknown type: " .. tostring(tree.type))
    end
  end
end
return NomsuCompiler
