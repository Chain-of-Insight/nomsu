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
    if not start then
      start, stop = 1, #FILE_CACHE[filename]
    end
    if stop then
      assert(start <= stop + 1, "Invalid range: " .. tostring(start) .. ", " .. tostring(stop))
    end
    return filename, start, stop
  end,
  from_string = function(self, str)
    local filename, start, stop = str:match("^(.-)%[(%d+):(%d+)%]$")
    if not (filename) then
      filename, start = str:match("^(.-)%[(%d+)%]$")
    end
    return Source(filename or str, tonumber(start or 1), tonumber(stop))
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
    append = function(self, ...)
      local n = select("#", ...)
      local bits, indents = self.bits, self.indents
      local match = string.match
      for i = 1, n do
        local b = select(i, ...)
        assert(b ~= self, "No recursion please.")
        assert(not Source:is_instance(b))
        bits[#bits + 1] = b
        if type(b) == 'string' then
          do
            local spaces = match(b, "\n([ ]*)[^\n]*$")
            if spaces then
              self.current_indent = #spaces
            end
          end
        elseif self.current_indent ~= 0 then
          indents[#bits] = self.current_indent
        end
      end
      self.__str = nil
    end,
    prepend = function(self, ...)
      local n = select("#", ...)
      local bits, indents = self.bits, self.indents
      for i = #bits + n, n + 1, -1 do
        bits[i] = bits[i - n]
      end
      for i = 1, n do
        bits[i] = select(i, ...)
      end
      self.current_indent = 0
      for i, b in ipairs(bits) do
        assert(b ~= self, "No recursion please.")
        if type(b) == 'string' then
          do
            local spaces = b:match("\n([ ]*)[^\n]*$")
            if spaces then
              self.current_indent = #spaces
            end
          end
        elseif self.current_indent ~= 0 then
          indents[i] = self.current_indent
        else
          indents[i] = nil
        end
      end
      self.__str = nil
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, source, ...)
      self.source = source
      self.bits, self.indents, self.current_indent = { }, { }, 0
      self:append(...)
      if type(self.source) == 'string' then
        self.source = Source:from_string(self.source)
      end
      return assert(self.source)
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
    add_free_vars = function(self, vars)
      if not (#vars > 0) then
        return 
      end
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
      for _index_0 = 1, #vars do
        local var = vars[_index_0]
        assert(type(var) == 'userdata' and var.type == "Var")
        if not (seen[var]) then
          self.free_vars[#self.free_vars + 1] = var
          seen[var] = true
        end
      end
      self.__str = nil
    end,
    remove_free_vars = function(self, vars)
      if not (#vars > 0) then
        return 
      end
      local removals = { }
      for _index_0 = 1, #vars do
        local var = vars[_index_0]
        assert(type(var) == 'userdata' and var.type == "Var")
        removals[var.value] = true
      end
      local stack = {
        self
      }
      while #stack > 0 do
        local lua
        lua, stack[#stack] = stack[#stack], nil
        for i = #lua.free_vars, 1, -1 do
          if removals[lua.free_vars[i].value] then
            remove(lua.free_vars, i)
          end
        end
        local _list_0 = lua.bits
        for _index_0 = 1, #_list_0 do
          local b = _list_0[_index_0]
          if type(b) ~= 'string' then
            stack[#stack + 1] = b
          end
        end
      end
      self.__str = nil
    end,
    as_statements = function(self, prefix, suffix)
      if prefix == nil then
        prefix = ""
      end
      if suffix == nil then
        suffix = ";"
      end
      if not (self.is_value) then
        return self
      end
      local statements = Lua(self.source)
      if prefix ~= "" then
        statements:append(prefix)
      end
      statements:append(self)
      if suffix ~= "" then
        statements:append(suffix)
      end
      return statements
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
      if #to_declare > 0 then
        self:remove_free_vars(to_declare)
        self:prepend("local " .. tostring(concat((function()
          local _accum_0 = { }
          local _len_0 = 1
          for _index_0 = 1, #to_declare do
            local v = to_declare[_index_0]
            _accum_0[_len_0] = v:as_lua_id()
            _len_0 = _len_0 + 1
          end
          return _accum_0
        end)(), ", ")) .. ";\n")
      end
      return to_declare
    end,
    __tostring = function(self)
      if self.__str == nil then
        local buff, indents = { }, self.indents
        for i, b in ipairs(self.bits) do
          b = tostring(b)
          if indents[i] then
            b = b:gsub("\n", "\n" .. ((" "):rep(indents[i])))
          end
          buff[#buff + 1] = b
        end
        self.__str = concat(buff, "")
      end
      return self.__str
    end,
    __len = function(self)
      return #tostring(self)
    end,
    make_offset_table = function(self)
      local lua_to_nomsu, nomsu_to_lua = { }, { }
      local walk
      walk = function(lua, pos)
        local _list_0 = lua.bits
        for _index_0 = 1, #_list_0 do
          local b = _list_0[_index_0]
          if type(b) == 'string' then
            if lua.source then
              lua_to_nomsu[pos] = lua.source.start
              nomsu_to_lua[lua.source.start] = pos
            end
          else
            walk(b, pos)
          end
          pos = pos + #tostring(b)
        end
      end
      walk(self, 1)
      return {
        nomsu_filename = self.source.filename,
        lua_filename = tostring(self.source) .. ".lua",
        lua_file = self:stringify(),
        lua_to_nomsu = lua_to_nomsu,
        nomsu_to_lua = nomsu_to_lua
      }
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
      self.__str = nil
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
      if self.__str == nil then
        local buff, indents = { }, self.indents
        for i, b in ipairs(self.bits) do
          b = tostring(b)
          if indents[i] then
            b = b:gsub("\n", "\n" .. ((" "):rep(indents[i])))
          end
          buff[#buff + 1] = b
        end
        self.__str = concat(buff, "")
      end
      return self.__str
    end,
    __len = function(self)
      return #tostring(self)
    end,
    parenthesize = function(self)
      self:prepend("(")
      return self:append(")")
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
