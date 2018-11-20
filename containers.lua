local List, Dict
local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local as_nomsu
as_nomsu = function(self)
  if type(self) == 'number' then
    return tostring(self)
  end
  do
    local mt = getmetatable(self)
    if mt then
      do
        local _as_nomsu = mt.as_nomsu
        if _as_nomsu then
          return _as_nomsu(self)
        end
      end
    end
  end
  return tostring(self)
end
local as_lua
as_lua = function(self)
  if type(self) == 'number' then
    return tostring(self)
  end
  do
    local mt = getmetatable(self)
    if mt then
      do
        local _as_lua = mt.as_lua
        if _as_lua then
          return _as_lua(self)
        end
      end
    end
  end
  return tostring(self)
end
local nth_to_last
nth_to_last = function(self, n)
  return self[#self - n + 1]
end
local _list_mt = {
  __type = "List",
  __eq = function(self, other)
    if not (type(other) == 'table' and getmetatable(other) == getmetatable(self) and #other == #self) then
      return false
    end
    for i, x in ipairs(self) do
      if not (x == other[i]) then
        return false
      end
    end
    return true
  end,
  __tostring = function(self)
    return "[" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #self do
        local b = self[_index_0]
        _accum_0[_len_0] = as_nomsu(b)
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), ", ") .. "]"
  end,
  as_nomsu = function(self)
    return "[" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #self do
        local b = self[_index_0]
        _accum_0[_len_0] = as_nomsu(b)
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), ", ") .. "]"
  end,
  as_lua = function(self)
    return "List{" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #self do
        local b = self[_index_0]
        _accum_0[_len_0] = as_lua(b)
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), ", ") .. "}"
  end,
  __lt = function(self, other)
    assert(type(self) == 'table' and type(other) == 'table', "Incompatible types for comparison")
    for i = 1, math.max(#self, #other) do
      if not self[i] and other[i] then
        return true
      elseif self[i] and not other[i] then
        return false
      elseif self[i] < other[i] then
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
      if not self[i] and other[i] then
        return true
      elseif self[i] and not other[i] then
        return false
      elseif self[i] < other[i] then
        return true
      elseif self[i] > other[i] then
        return false
      end
    end
    return true
  end,
  __add = function(self, other)
    local ret = List((function()
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #self do
        local x = self[_index_0]
        _accum_0[_len_0] = x
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())
    for _index_0 = 1, #other do
      local x = other[_index_0]
      insert(ret, x)
    end
    return ret
  end,
  __index = {
    add = insert,
    append = insert,
    add_1_at_index = function(t, x, i)
      return insert(t, i, x)
    end,
    at_index_1_add = insert,
    pop = remove,
    remove_last = remove,
    remove_index = remove,
    last = (function(self)
      return self[#self]
    end),
    first = (function(self)
      return self[1]
    end),
    _1_st_to_last = nth_to_last,
    _1_nd_to_last = nth_to_last,
    _1_rd_to_last = nth_to_last,
    _1_th_to_last = nth_to_last,
    joined = function(self)
      return table.concat((function()
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #self do
          local x = self[_index_0]
          _accum_0[_len_0] = tostring(x)
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)())
    end,
    joined_with = function(self, glue)
      return table.concat((function()
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #self do
          local x = self[_index_0]
          _accum_0[_len_0] = tostring(x)
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), glue)
    end,
    has = function(self, item)
      for _index_0 = 1, #self do
        local x = self[_index_0]
        if x == item then
          return true
        end
      end
      return false
    end,
    remove = function(self, item)
      for i, x in ipairs(self) do
        if x == item then
          remove(self, i)
        end
      end
    end,
    index_of = function(self, item)
      for i, x in ipairs(self) do
        if x == item then
          return i
        end
      end
      return nil
    end,
    from_1_to = function(self, start, stop)
      local n = #self
      if start < 0 then
        start = (n + 1 - start)
      end
      if stop < 0 then
        stop = (n + 1 - stop)
      end
      return List((function()
        local _accum_0 = { }
        local _len_0 = 1
        for i = start, stop do
          _accum_0[_len_0] = self[i]
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)())
    end
  },
  __newindex = function(self, k, v)
    assert(type(k) == 'number', "List indices must be numbers")
    return rawset(self, k, v)
  end
}
_list_mt.__index.as_lua = _list_mt.as_lua
_list_mt.__index.as_nomsu = _list_mt.as_nomsu
List = function(t)
  return setmetatable(t, _list_mt)
end
local walk_items
walk_items = function(self, i)
  i = i + 1
  local k, v = next(self.table, self.key)
  if k ~= nil then
    self.key = k
    return i, Dict({
      key = k,
      value = v
    })
  end
end
local _dict_mt = {
  __type = "Dict",
  __eq = function(self, other)
    if not (type(other) == 'table' and getmetatable(other) == getmetatable(self)) then
      return false
    end
    for k, v in pairs(self) do
      if not (v == other[k]) then
        return false
      end
    end
    for k, v in pairs(other) do
      if not (v == self[k]) then
        return false
      end
    end
    return true
  end,
  __len = function(self)
    local n = 0
    for _ in pairs(self) do
      n = n + 1
    end
    return n
  end,
  __tostring = function(self)
    return "{" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for k, v in pairs(self) do
        _accum_0[_len_0] = tostring(as_nomsu(k)) .. ": " .. tostring(as_nomsu(v))
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), ", ") .. "}"
  end,
  as_nomsu = function(self)
    return "{" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for k, v in pairs(self) do
        _accum_0[_len_0] = tostring(as_nomsu(k)) .. ": " .. tostring(as_nomsu(v))
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), ", ") .. "}"
  end,
  as_lua = function(self)
    return "Dict{" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for k, v in pairs(self) do
        _accum_0[_len_0] = "[ " .. tostring(as_lua(k)) .. "]= " .. tostring(as_lua(v))
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), ", ") .. "}"
  end,
  __ipairs = function(self)
    return walk_items, {
      table = self,
      key = nil
    }, 0
  end,
  __band = function(self, other)
    return Dict((function()
      local _tbl_0 = { }
      for k, v in pairs(self) do
        if other[k] ~= nil then
          _tbl_0[k] = v
        end
      end
      return _tbl_0
    end)())
  end,
  __bor = function(self, other)
    local ret
    do
      local _tbl_0 = { }
      for k, v in pairs(self) do
        _tbl_0[k] = v
      end
      ret = _tbl_0
    end
    for k, v in pairs(other) do
      if ret[k] == nil then
        ret[k] = v
      end
    end
    return Dict(ret)
  end,
  __bxor = function(self, other)
    local ret
    do
      local _tbl_0 = { }
      for k, v in pairs(self) do
        _tbl_0[k] = v
      end
      ret = _tbl_0
    end
    for k, v in pairs(other) do
      if ret[k] == nil then
        ret[k] = v
      else
        ret[k] = nil
      end
    end
    return Dict(ret)
  end,
  __add = function(self, other)
    local ret
    do
      local _tbl_0 = { }
      for k, v in pairs(self) do
        _tbl_0[k] = v
      end
      ret = _tbl_0
    end
    for k, v in pairs(other) do
      if ret[k] == nil then
        ret[k] = v
      else
        ret[k] = ret[k] + v
      end
    end
    return Dict(ret)
  end,
  __sub = function(self, other)
    local ret
    do
      local _tbl_0 = { }
      for k, v in pairs(self) do
        _tbl_0[k] = v
      end
      ret = _tbl_0
    end
    for k, v in pairs(other) do
      if ret[k] == nil then
        ret[k] = -v
      else
        ret[k] = ret[k] - v
      end
    end
    return Dict(ret)
  end
}
Dict = function(t)
  return setmetatable(t, _dict_mt)
end
for i, entry in ipairs(Dict({
  x = 99
})) do
  assert(i == 1 and entry.key == "x" and entry.value == 99, "ipairs compatibility issue")
end
do
  local reverse, upper, lower, find, byte, match, gmatch, gsub, sub, format, rep
  do
    local _obj_0 = string
    reverse, upper, lower, find, byte, match, gmatch, gsub, sub, format, rep = _obj_0.reverse, _obj_0.upper, _obj_0.lower, _obj_0.find, _obj_0.byte, _obj_0.match, _obj_0.gmatch, _obj_0.gsub, _obj_0.sub, _obj_0.format, _obj_0.rep
  end
  local string2 = require('string2')
  local lines, line, line_at, as_lua_id, is_lua_id
  lines, line, line_at, as_lua_id, is_lua_id = string2.lines, string2.line, string2.line_at, string2.as_lua_id, string2.is_lua_id
  local text_methods = {
    formatted_with = format,
    byte = byte,
    position_of = find,
    position_of_1_after = find,
    as_a_lua_identifier = as_lua_id,
    is_a_lua_identifier = is_lua_id,
    as_a_lua_id = as_lua_id,
    is_a_lua_id = is_lua_id,
    bytes_1_to = function(self, start, stop)
      return List({
        byte(tostring(self), start, stop)
      })
    end,
    [as_lua_id("with 1 ->")] = function(...)
      return (gsub(...))
    end,
    bytes = function(self)
      return List({
        byte(tostring(self), 1, -1)
      })
    end,
    lines = function(self)
      return List(lines(self))
    end,
    line = line,
    wrapped_to = function(self, maxlen)
      local _lines = { }
      local _list_0 = self:lines()
      for _index_0 = 1, #_list_0 do
        local line = _list_0[_index_0]
        while #line > maxlen do
          local chunk = line:sub(1, maxlen)
          local split = chunk:find(' ', maxlen - 8) or maxlen
          chunk = line:sub(1, split)
          line = line:sub(split + 1, -1)
          _lines[#_lines + 1] = chunk
        end
        _lines[#_lines + 1] = line
      end
      return table.concat(_lines, "\n")
    end,
    line_at = function(self, i)
      return (line_at(self, i))
    end,
    line_number_at = function(self, i)
      return select(2, line_at(self, i))
    end,
    line_position_at = function(self, i)
      return select(3, line_at(self, i))
    end,
    matches = function(self, patt)
      return match(self, patt) and true or false
    end,
    matching = function(self, patt)
      return (match(self, patt))
    end,
    matching_groups = function(self, patt)
      return {
        match(self, patt)
      }
    end,
    [as_lua_id("* 1")] = function(self, n)
      return rep(self, n)
    end,
    all_matches_of = function(self, patt)
      local result = { }
      local stepper, x, i = gmatch(self, patt)
      while true do
        local tmp = List({
          stepper(x, i)
        })
        if #tmp == 0 then
          break
        end
        i = tmp[1]
        result[#result + 1] = (#tmp == 1) and tmp[1] or tmp
      end
      return List(result)
    end,
    from_1_to = sub,
    from = sub,
    character = function(self, i)
      return sub(self, i, i)
    end
  }
  setmetatable(text_methods, {
    __index = string2
  })
  setmetatable(string2, {
    __index = error
  })
  getmetatable("").__methods = text_methods
  getmetatable("").__index = text_methods
  getmetatable("").__add = function(self, x)
    return tostring(self) .. tostring(x)
  end
end
return {
  List = List,
  Dict = Dict
}
