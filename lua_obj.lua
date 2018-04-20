local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local immutable = require('immutable')
local Lua, Source
Source = immutable({
  "filename",
  "start",
  "stop"
}, {
  name = "Source",
  __new = function(self, filename, start, stop)
    if stop then
      assert(start <= stop, "Invalid range: " .. tostring(start) .. ", " .. tostring(stop))
    end
    return filename, start, stop
  end,
  __tostring = function(self)
    if self.stop then
      return "\"" .. tostring(self.filename) .. "[" .. tostring(self.start) .. ":" .. tostring(self.stop) .. "]\""
    else
      return "\"" .. tostring(self.filename) .. "[" .. tostring(self.start) .. "]\""
    end
  end,
  __lt = function(self, other)
    assert(self.filename == other.filename, "Cannot compare sources from different files")
    if self.start == other.start then
      return (self.stop or self.start) < (other.stop or other.start)
    else
      return self.start < other.start
    end
  end,
  __le = function(self, other)
    assert(self.filename == other.filename, "Cannot compare sources from different files")
    if self.start == other.start then
      return (self.stop or self.start) <= (other.stop or other.start)
    else
      return self.start <= other.start
    end
  end,
  __add = function(self, offset)
    if type(self) == 'number' then
      offset, self = self, offset
    else
      assert(type(offset) == 'number', "Cannot add Source and " .. tostring(type(offset)))
    end
    return Source(self.filename, self.start + offset, self.stop)
  end,
  sub = function(self, start, stop)
    start = start or 1
    assert(start > 0 and (stop == nil or stop > 0), "Negative subscripts not supported")
    if not self.stop then
      assert(not stop, "cannot subscript non-range with range")
      return Source(self.filename, self.start + start - 1)
    else
      stop = stop or self.stop
      return Source(self.filename, self.start + start - 1, self.start + stop - 1)
    end
  end,
  get_text = function(self)
    return FILE_CACHE[self.filename]:sub(self.start, self.stop)
  end,
  get_line_number = function(self)
    local src = FILE_CACHE[self.filename]
    local line_starts = LINE_STARTS[src]
    local start_line = 1
    while (line_starts[start_line + 1] or math.huge) <= self.start do
      start_line = start_line + 1
    end
    local stop_line = start_line
    while (line_starts[stop_line + 1] or math.huge) <= self.stop do
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
local Code
do
  local _class_0
  local _base_0 = {
    clone = function(self)
      local cls = self.__class
      local copy = cls(self.source, unpack(self.bits))
      copy.is_value = self.is_value
      for k, v in pairs(self.free_vars) do
        copy.free_vars[k] = v
      end
      return copy
    end,
    __tostring = function(self)
      local buff = { }
      for i, b in ipairs(self.bits) do
        buff[#buff + 1] = tostring(b)
      end
      local ret = concat(buff, "")
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
    sub = function(self, start, stop)
      local str = tostring(self):sub(start, stop)
      local cls = self.__class
      return cls(self.source:sub(start, stop), str)
    end,
    append = function(self, ...)
      local n = select("#", ...)
      local bits = self.bits
      for i = 1, n do
        bits[#bits + 1] = select(i, ...)
      end
    end,
    prepend = function(self, ...)
      local n = select("#", ...)
      local bits = self.bits
      for i = #bits + n, n + 1, -1 do
        bits[i] = bits[i - n]
      end
      for i = 1, n do
        bits[i] = select(i, ...)
      end
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, source, ...)
      self.source = source
      self.bits = {
        ...
      }
      if type(self.source) == 'string' then
        local filename, start, stop = self.source:match("^(.-)%[(%d+):(%d+)%]$")
        if not (filename) then
          filename, start = self.source:match("^(.-)%[(%d+)%]$")
        end
        if start or stop then
          self.source = Source(filename, tonumber(start), tonumber(stop))
        else
          self.source = Source(self.source, 1, #self + 1)
        end
      end
    end,
    __base = _base_0,
    __name = "Code"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Code = _class_0
end
do
  local _class_0
  local _parent_0 = Code
  local _base_0 = {
    add_free_vars = function(self, ...)
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
      for i = 1, select("#", ...) do
        local var = select(i, ...)
        if type(var) == 'userdata' and var.type == "Var" then
          var = tostring(var:as_lua())
        elseif type(var) ~= 'string' then
          var = tostring(var)
        end
        if not (seen[var]) then
          self.free_vars[#self.free_vars + 1] = var
          seen[var] = true
        end
      end
    end,
    remove_free_vars = function(self, ...)
      local removals = { }
      for i = 1, select("#", ...) do
        local var = select(i, ...)
        if type(var) == 'userdata' and var.type == "Var" then
          var = tostring(var:as_lua())
        elseif type(var) ~= 'string' then
          var = tostring(var)
        end
        removals[var] = true
      end
      local remove_from
      remove_from = function(self)
        for i = #self.free_vars, 1, -1 do
          if removals[self.free_vars[i]] then
            remove(self.free_vars, i)
          end
        end
        local _list_0 = self.bits
        for _index_0 = 1, #_list_0 do
          local b = _list_0[_index_0]
          if type(b) ~= 'string' then
            remove_from(b)
          end
        end
      end
      return remove_from(self)
    end,
    convert_to_statements = function(self, prefix, suffix)
      if prefix == nil then
        prefix = ""
      end
      if suffix == nil then
        suffix = ";"
      end
      if not (self.is_value) then
        return 
      end
      if prefix ~= "" then
        self:prepend(prefix)
      end
      if suffix ~= "" then
        return self:append(suffix)
      end
    end,
    declare_locals = function(self, to_declare)
      if to_declare == nil then
        to_declare = nil
      end
      if to_declare == nil then
        local seen
        to_declare, seen = { }, { }
        local gather_from
        gather_from = function(self)
          local _list_0 = self.free_vars
          for _index_0 = 1, #_list_0 do
            local var = _list_0[_index_0]
            if not (seen[var]) then
              seen[var] = true
              to_declare[#to_declare + 1] = var
            end
          end
          local _list_1 = self.bits
          for _index_0 = 1, #_list_1 do
            local bit = _list_1[_index_0]
            if bit.__class == Lua then
              gather_from(bit)
            end
          end
        end
        gather_from(self)
      end
      self:remove_free_vars(to_declare)
      if #to_declare > 0 then
        return self:prepend("local " .. tostring(concat(to_declare, ", ")) .. ";\n")
      end
    end,
    __tostring = function(self)
      local buff = { }
      for i, b in ipairs(self.bits) do
        buff[#buff + 1] = tostring(b)
      end
      local ret = concat(buff, "")
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
        local bit = select(i, ...)
        bits[#bits + 1] = bit
        if type(bit) ~= 'string' and not bit.is_value and #self.bits > 0 then
          bits[#bits + 1] = "\n"
        end
      end
    end,
    prepend = function(self, ...)
      local n = select("#", ...)
      local bits = self.bits
      local insert_index = 1
      for i = 1, n do
        local bit = select(i, ...)
        insert(bits, insert_index, bit)
        insert_index = insert_index + 1
        if type(bit) ~= 'string' and not bit.is_value and insert_index < #self.bits + 1 then
          insert(bits, insert_index, "\n")
          insert_index = insert_index + 1
        end
      end
    end,
    make_offset_table = function(self)
      local lua_chunkname = tostring(self.source) .. ".lua"
      local lua_str = tostring(self)
      local metadata = {
        nomsu_filename = self.source.filename,
        lua_filename = lua_chunkname,
        lua_file = lua_str,
        lua_to_nomsu = { },
        nomsu_to_lua = { }
      }
      local walk
      walk = function(lua, output_range)
        local pos = 1
        local _list_0 = lua.bits
        for _index_0 = 1, #_list_0 do
          local b = _list_0[_index_0]
          if type(b) == 'string' then
            local output = output_range:sub(pos, pos + #b)
            metadata.lua_to_nomsu[output] = lua.source
            metadata.nomsu_to_lua[lua.source] = output
          else
            walk(b, output_range:sub(pos, pos + #b))
          end
          pos = pos + #b
        end
      end
      walk(self, Source(lua_chunkname, 1, #lua_str))
      return lua_str, metadata
    end,
    parenthesize = function(self)
      if self.is_value then
        self:prepend("(")
        return self:append(")")
      else
        return error("Cannot parenthesize lua statements")
      end
    end
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  _class_0 = setmetatable({
    __init = function(self, ...)
      _class_0.__parent.__init(self, ...)
      self.free_vars = { }
      self.is_value = false
    end,
    __base = _base_0,
    __name = "Lua",
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
  local self = _class_0
  self.Value = function(...)
    local lua = Lua(...)
    lua.is_value = true
    return lua
  end
  if _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  Lua = _class_0
end
local Nomsu
do
  local _class_0
  local _parent_0 = Code
  local _base_0 = {
    __tostring = function(self)
      local buff = { }
      for i, b in ipairs(self.bits) do
        buff[#buff + 1] = tostring(b)
      end
      local ret = concat(buff, "")
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
    end
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  _class_0 = setmetatable({
    __init = function(self, ...)
      return _class_0.__parent.__init(self, ...)
    end,
    __base = _base_0,
    __name = "Nomsu",
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
  Nomsu = _class_0
end
return {
  Code = Code,
  Nomsu = Nomsu,
  Lua = Lua,
  Source = Source
}
