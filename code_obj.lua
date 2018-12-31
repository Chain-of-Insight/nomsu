local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local unpack = unpack or table.unpack
local LuaCode, NomsuCode, Source
do
  local _class_0
  local _base_0 = {
    __tostring = function(self)
      return "@" .. tostring(self.filename) .. "[" .. tostring(self.start) .. tostring(self.stop and ':' .. self.stop or '') .. "]"
    end,
    as_lua = function(self)
      return "Source(" .. tostring(self.filename:as_lua()) .. ", " .. tostring(self.start) .. tostring(self.stop and ', ' .. self.stop or '') .. ")"
    end,
    __eq = function(self, other)
      return getmetatable(self) == getmetatable(other) and self.filename == other.filename and self.start == other.start and self.stop == other.stop
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
        if type(offset) ~= 'number' then
          error("Cannot add Source and " .. tostring(type(offset)))
        end
      end
      return Source(self.filename, self.start + offset, self.stop)
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, filename, start, stop)
      self.filename, self.start, self.stop = filename, start, stop
    end,
    __base = _base_0,
    __name = "Source"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  local self = _class_0
  self.from_string = function(self, str)
    local filename, start, stop = str:match("^@(.-)%[(%d+):(%d+)%]$")
    if not (filename) then
      filename, start = str:match("^@(.-)%[(%d+)%]$")
    end
    return self(filename or str, tonumber(start or 1), tonumber(stop))
  end
  self.is_instance = function(self, x)
    return type(x) == 'table' and x.__class == self
  end
  Source = _class_0
end
local Code
do
  local _class_0
  local _base_0 = {
    text = function(self)
      if self.__str == nil then
        local buff, indent = { }, 0
        local match, gsub, rep
        do
          local _obj_0 = string
          match, gsub, rep = _obj_0.match, _obj_0.gsub, _obj_0.rep
        end
        for i, b in ipairs(self.bits) do
          if type(b) == 'string' then
            do
              local spaces = match(b, "\n([ ]*)[^\n]*$")
              if spaces then
                indent = #spaces
              end
            end
          else
            b = b:text()
            if indent > 0 then
              b = gsub(b, "\n", "\n" .. rep(" ", indent))
            end
          end
          buff[#buff + 1] = b
        end
        self.__str = concat(buff, "")
      end
      return self.__str
    end,
    __tostring = function(self)
      return self:text()
    end,
    as_lua = function(self)
      if self.source then
        return tostring(self.__class.__name) .. ":from(" .. tostring(concat({
          tostring(self.source):as_lua(),
          unpack((function()
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = self.bits
            for _index_0 = 1, #_list_0 do
              local b = _list_0[_index_0]
              _accum_0[_len_0] = b:as_lua()
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)())
        }, ", ")) .. ")"
      else
        return tostring(self.__class.__name) .. "(" .. tostring(concat((function()
          local _accum_0 = { }
          local _len_0 = 1
          local _list_0 = self.bits
          for _index_0 = 1, #_list_0 do
            local b = _list_0[_index_0]
            _accum_0[_len_0] = b:as_lua()
            _len_0 = _len_0 + 1
          end
          return _accum_0
        end)(), ", ")) .. ")"
      end
    end,
    __len = function(self)
      return #self:text()
    end,
    match = function(self, ...)
      return self:text():match(...)
    end,
    gmatch = function(self, ...)
      return self:text():gmatch(...)
    end,
    dirty = function(self)
      self.__str = nil
      self._trailing_line_len = nil
      if self._is_multiline == false then
        self._is_multiline = nil
      end
    end,
    add = function(self, ...)
      local n = select("#", ...)
      local match = string.match
      local bits = self.bits
      for i = 1, n do
        local _continue_0 = false
        repeat
          local b = select(i, ...)
          assert(b, "code bit is nil")
          assert(not Source:is_instance(b), "code bit is a Source")
          if b == '' then
            _continue_0 = true
            break
          end
          bits[#bits + 1] = b
          _continue_0 = true
        until true
        if not _continue_0 then
          break
        end
      end
      return self:dirty()
    end,
    trailing_line_len = function(self)
      if self._trailing_line_len == nil then
        self._trailing_line_len = #self:text():match("[^\n]*$")
      end
      return self._trailing_line_len
    end,
    is_multiline = function(self)
      if self._is_multiline == nil then
        local match = string.match
        self._is_multiline = false
        local _list_0 = self.bits
        for _index_0 = 1, #_list_0 do
          local b = _list_0[_index_0]
          if type(b) == 'string' then
            if match(b, '\n') then
              self._is_multiline = true
              break
            end
          elseif b:is_multiline() then
            self._is_multiline = true
            break
          end
        end
      end
      return self._is_multiline
    end,
    concat_add = function(self, values, joiner, wrapping_joiner)
      wrapping_joiner = wrapping_joiner or joiner
      local match = string.match
      local bits = self.bits
      local line_len = 0
      for i = 1, #values do
        local b = values[i]
        if i > 1 then
          if line_len > 80 then
            bits[#bits + 1] = wrapping_joiner
            line_len = 0
          else
            bits[#bits + 1] = joiner
          end
        end
        bits[#bits + 1] = b
        if type(b) ~= 'string' then
          b.dirty = error
        end
        if not (type(b) == 'string') then
          b = b:text()
        end
        local line = match(b, "\n([^\n]*)$")
        if line then
          line_len = #line
        else
          line_len = line_len + #b
        end
      end
      return self:dirty()
    end,
    prepend = function(self, ...)
      local n = select("#", ...)
      local bits = self.bits
      for i = #bits + n, n + 1, -1 do
        bits[i] = bits[i - n]
      end
      for i = 1, n do
        local b = select(i, ...)
        if type(b) ~= 'string' then
          b.dirty = error
        end
        bits[i] = b
      end
      return self:dirty()
    end,
    parenthesize = function(self)
      self:prepend("(")
      return self:add(")")
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, ...)
      self.bits = { }
      return self:add(...)
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
  local self = _class_0
  self.from = function(self, source, ...)
    local inst = self(...)
    if type(source) == 'string' then
      source = Source:from_string(source)
    end
    inst.source = source
    return inst
  end
  self.is_instance = function(self, x)
    return type(x) == 'table' and x.__class == self
  end
  Code = _class_0
end
do
  local _class_0
  local _parent_0 = Code
  local _base_0 = {
    __tostring = Code.__tostring,
    as_lua = Code.as_lua,
    __len = Code.__len,
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
        assert(type(var) == 'string')
        if not (seen[var]) then
          self.free_vars[#self.free_vars + 1] = var
          seen[var] = true
        end
      end
      return self:dirty()
    end,
    remove_free_vars = function(self, vars)
      if not (#vars > 0) then
        return 
      end
      local removals = { }
      for _index_0 = 1, #vars do
        local var = vars[_index_0]
        assert(type(var) == 'string')
        removals[var] = true
      end
      local stack = {
        self
      }
      while #stack > 0 do
        local lua
        lua, stack[#stack] = stack[#stack], nil
        for i = #lua.free_vars, 1, -1 do
          local free_var = lua.free_vars[i]
          if removals[free_var] then
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
      return self:dirty()
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
            if not (type(bit) == 'string') then
              gather_from(bit)
            end
          end
        end
        gather_from(self)
      end
      if #to_declare > 0 then
        self:remove_free_vars(to_declare)
        self:prepend("local " .. tostring(concat(to_declare, ", ")) .. ";\n")
      end
      return to_declare
    end,
    make_offset_table = function(self)
      assert(self.source, "This code doesn't have a source")
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
            b = b:text()
          end
          pos = pos + #b
        end
      end
      walk(self, 1)
      return {
        nomsu_filename = self.source.filename,
        lua_filename = tostring(self.source) .. ".lua",
        lua_file = self:text(),
        lua_to_nomsu = lua_to_nomsu,
        nomsu_to_lua = nomsu_to_lua
      }
    end,
    parenthesize = function(self)
      self:prepend("(")
      return self:add(")")
    end
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  _class_0 = setmetatable({
    __init = function(self, ...)
      _class_0.__parent.__init(self, ...)
      self.free_vars = { }
    end,
    __base = _base_0,
    __name = "LuaCode",
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
  LuaCode = _class_0
end
do
  local _class_0
  local _parent_0 = Code
  local _base_0 = {
    __tostring = Code.__tostring,
    as_lua = Code.as_lua,
    __len = Code.__len
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  _class_0 = setmetatable({
    __init = function(self, ...)
      return _class_0.__parent.__init(self, ...)
    end,
    __base = _base_0,
    __name = "NomsuCode",
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
  NomsuCode = _class_0
end
Code.__base.add_1_joined_with = assert(Code.__base.concat_add)
Code.__base.add = assert(Code.__base.add)
return {
  Code = Code,
  NomsuCode = NomsuCode,
  LuaCode = LuaCode,
  Source = Source
}
