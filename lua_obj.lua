local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local immutable = require('immutable')
local Lua, LuaValue, Location
Location = immutable({
  "filename",
  "start",
  "stop"
}, {
  name = "Location",
  __new = function(self, filename, start, stop)
    assert(type(filename) == 'string' and type(start) == 'number' and type(stop) == 'number')
    return filename, start, stop or start
  end,
  __tostring = function(self)
    return "Location(\"" .. tostring(self.filename) .. "\", " .. tostring(self.start) .. ", " .. tostring(self.stop) .. ")"
  end,
  __lt = function(self, other)
    assert(self.filename == other.filename, "Cannot compare sources from different files")
    if self.start == other.start then
      return self.stop < other.stop
    else
      return self.start < other.start
    end
  end,
  __le = function(self, other)
    assert(self.filename == other.filename, "Cannot compare sources from different files")
    if self.start == other.start then
      return self.stop <= other.stop
    else
      return self.start <= other.start
    end
  end,
  get_text = function(self)
    return FILE_CACHE[self.filename]:sub(self.start, self.stop)
  end,
  get_line_number = function(self)
    local line_starts = LINE_STARTS[FILE_CACHE[self.filename]]
    local start_line = 1
    while (line_starts[start_line + 1] or (#src + 1)) <= self.start do
      start_line = start_line + 1
    end
    local stop_line = start_line
    while (line_starts[stop_line + 1] or (#src + 1)) <= self.stop do
      stop_line = stop_line + 1
    end
    return start_line, stop_line
  end,
  get_line = function(self)
    return tostring(self.filename) .. ":" .. tostring(self:get_line_number())
  end,
  get_line_range = function(self)
    local start_line, stop_line = self:get_line_number()
    if stop_line == start_line then
      return tostring(self.filename) .. ":" .. tostring(start_line)
    else
      return tostring(self.filename) .. ":" .. tostring(start_line) .. "-" .. tostring(stop_line)
    end
  end
})
do
  local _class_0
  local _base_0 = {
    is_statement = true,
    is_value = false,
    add_free_vars = function(self, free_vars)
      local seen
      do
        local _tbl_0 = { }
        local _list_0 = self.free_vars
        for _index_0 = 1, #_list_0 do
          local v = _list_0[_index_0]
          local _key_0, _val_0 = {
            [v] = true
          }
          _tbl_0[_key_0] = _val_0
        end
        seen = _tbl_0
      end
      for _index_0 = 1, #free_vars do
        local var = free_vars[_index_0]
        if not (seen[var]) then
          self.free_vars[#self.free_vars + 1] = var
          seen[var] = true
        end
      end
    end,
    as_statements = function(self)
      return self
    end,
    declare_locals = function(self, skip)
      if skip == nil then
        skip = { }
      end
      if next(skip) == 1 then
        do
          local _tbl_0 = { }
          for _index_0 = 1, #skip do
            local s = skip[_index_0]
            local _key_0, _val_0 = {
              [s] = true
            }
            _tbl_0[_key_0] = _val_0
          end
          skip = _tbl_0
        end
      end
      if #self.free_vars > 0 then
        self:prepend("local " .. tostring(concat(self.free_vars, ", ")) .. ";\n")
      end
      local _list_0 = self.free_vars
      for _index_0 = 1, #_list_0 do
        local var = _list_0[_index_0]
        skip[var] = true
      end
      local _list_1 = self.bits
      for _index_0 = 1, #_list_1 do
        local bit = _list_1[_index_0]
        if type(bit) == Lua then
          bit:declare_locals(skip)
        end
      end
    end,
    __tostring = function(self)
      local buff = { }
      local _list_0 = self.bits
      for _index_0 = 1, #_list_0 do
        local b = _list_0[_index_0]
        buff[#buff + 1] = tostring(b)
      end
      local ret = concat(buff, "")
      assert(not ret:match(".*table: 0x.*"))
      return ret
    end,
    __len = function(self)
      local len = 0
      local _list_0 = self.bits
      for _index_0 = 1, #_list_0 do
        local b = _list_0[_index_0]
        len = len + #b
      end
      return len
    end,
    append = function(self, ...)
      local n = select("#", ...)
      local bits = self.bits
      for i = 1, n do
        local x = select(i, ...)
        assert(type(x) ~= 'table' or getmetatable(x))
        bits[#bits + 1] = select(i, ...)
      end
    end,
    prepend = function(self, ...)
      local n = select("#", ...)
      local bits = self.bits
      for i = #bits + n, n + 1, -1 do
        local x = select(i, ...)
        assert(type(x) ~= 'table' or getmetatable(x))
        bits[i] = bits[i - n]
      end
      for i = 1, n do
        local x = select(i, ...)
        assert(type(x) ~= 'table' or getmetatable(x))
        bits[i] = select(i, ...)
      end
    end,
    make_offset_table = function(self, lua_chunkname)
      local lua_str = tostring(self)
      local metadata = {
        nomsu_filename = self.source.filename,
        lua_filename = lua_chunkname,
        lua_file = lua_str,
        lua_to_nomsu = { },
        nomsu_to_lua = { }
      }
      local lua_offset = 1
      local walk
      walk = function(lua)
        if type(lua) == 'string' then
          lua_offset = lua_offset + #lua
        else
          local lua_start = lua_offset
          local _list_0 = lua.bits
          for _index_0 = 1, #_list_0 do
            local b = _list_0[_index_0]
            walk(b)
          end
          local lua_stop = lua_offset
          local nomsu_src, lua_src = lua.source, Location(lua_chunkname, lua_start, lua_stop)
          metadata.lua_to_nomsu[lua_src] = nomsu_src
          metadata.nomsu_to_lua[nomsu_src] = lua_src
        end
      end
      walk(self)
      return lua_str, metadata
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, source, ...)
      self.source = source
      if type(self.source) == 'string' then
        local filename, start, stop = self.source:match("^(.-)[(%d+):(%d+)]$")
        self.source = Location(filename, tonumber(start), tonumber(stop))
      end
      for i = 1, select("#", ...) do
        local x = select(i, ...)
        assert(type(x) ~= 'table' or getmetatable(x))
      end
      self.bits = {
        ...
      }
      self.free_vars = { }
    end,
    __base = _base_0,
    __name = "Lua"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Lua = _class_0
end
do
  local _class_0
  local _parent_0 = Lua
  local _base_0 = {
    is_statement = false,
    is_value = true,
    __tostring = function(self)
      local buff = { }
      local _list_0 = self.bits
      for _index_0 = 1, #_list_0 do
        local b = _list_0[_index_0]
        buff[#buff + 1] = tostring(b)
      end
      local ret = concat(buff, "")
      assert(not ret:match(".*table: 0x.*"))
      return ret
    end,
    as_statements = function(self, prefix, suffix)
      if prefix == nil then
        prefix = ""
      end
      if suffix == nil then
        suffix = ";"
      end
      local bits = {
        prefix,
        unpack(self.bits)
      }
      bits[#bits + 1] = suffix
      return Lua(self.source, unpack(bits))
    end,
    parenthesize = function(self)
      self:prepend("(")
      return self:append(")")
    end
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  _class_0 = setmetatable({
    __init = function(self, source, ...)
      self.source = source
      self.bits = {
        ...
      }
    end,
    __base = _base_0,
    __name = "LuaValue",
    __parent = _parent_0
  }, {
    __index = function(cls, name)
      local val = rawget(_base_0, name)
      if val == nil then
        local parent = rawget(cls, "__parent")
        if parent then
          return parent[name]
        end
      else
        return val
      end
    end,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  if _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  LuaValue = _class_0
end
return {
  Lua = Lua,
  LuaValue = LuaValue,
  Location = Location
}
