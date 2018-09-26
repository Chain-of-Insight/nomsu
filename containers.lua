local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local equivalent, nth_to_last, size
do
  local _obj_0 = require('utils')
  equivalent, nth_to_last, size = _obj_0.equivalent, _obj_0.nth_to_last, _obj_0.size
end
local lpeg = require('lpeg')
local re = require('re')
local List, Dict
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
  return error("Not supported: " .. tostring(self))
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
  return error("Not supported: " .. tostring(self))
end
local _list_mt = {
  __eq = equivalent,
  __tostring = function(self)
    return "[" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #self do
        local b = self[_index_0]
        _accum_0[_len_0] = tostring(b)
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
    return "_List{" .. concat((function()
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
    add_1 = insert,
    append_1 = insert,
    add_1_at_index_2 = function(t, x, i)
      return insert(t, i, x)
    end,
    at_index_1_add_2 = insert,
    pop = remove,
    remove_last = remove,
    remove_index_1 = remove,
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
    joined_with_1 = function(self, glue)
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
    has_1 = function(self, item)
      for _index_0 = 1, #self do
        local x = self[_index_0]
        if x == item then
          return true
        end
      end
      return false
    end,
    index_of_1 = function(self, item)
      for i, x in ipairs(self) do
        if x == item then
          return i
        end
      end
      return nil
    end,
    slice_1_to_2 = function(self, start, stop)
      local n = #self
      if n < 0 then
        start = (n + 1 - start)
      end
      if n < 0 then
        stop = (n + 1 - stop)
      end
      local _accum_0 = { }
      local _len_0 = 1
      for i = start, stop do
        _accum_0[_len_0] = self[i]
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end
  },
  __newindex = function(self, k, v)
    assert(type(k) == 'number', "List indices must be numbers")
    return rawset(self, k, v)
  end
}
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
  __eq = equivalent,
  __len = size,
  __tostring = function(self)
    return "{" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for k, v in pairs(self) do
        _accum_0[_len_0] = tostring(tostring(k)) .. ": " .. tostring(tostring(v))
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
    return "_Dict{" .. concat((function()
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
    formatted_with_1 = format,
    byte_1 = byte,
    position_of_1 = find,
    position_of_1_after_2 = find,
    as_a_lua_identifier = as_lua_id,
    is_a_lua_identifier = is_lua_id,
    as_a_lua_id = as_lua_id,
    is_a_lua_id = is_lua_id,
    bytes_1_to_2 = function(self, start, stop)
      return List({
        byte(tostring(self), start, stop)
      })
    end,
    [as_lua_id("with 1 -> 2")] = gsub,
    bytes = function(self)
      return List({
        byte(tostring(self), 1, -1)
      })
    end,
    lines = function(self)
      return List(lines(self))
    end,
    line_1 = line,
    wrap_to_1 = function(self, maxlen)
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
    line_at_1 = function(self, i)
      return (line_at(self, i))
    end,
    line_number_of_1 = function(self, i)
      return select(2, line_at(self, i))
    end,
    line_position_of_1 = function(self, i)
      return select(3, line_at(self, i))
    end,
    matches_1 = function(self, patt)
      return match(self, patt) and true or false
    end,
    matching_1 = function(self, patt)
      return (match(self, patt))
    end,
    matching_groups_1 = function(self, patt)
      return {
        match(self, patt)
      }
    end,
    [as_lua_id("* 1")] = function(self, n)
      return rep(self, n)
    end,
    all_matches_of_1 = function(self, patt)
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
        result[#result + 1] = tmp
      end
      return List(result)
    end
  }
  setmetatable(text_methods, {
    __index = string2
  })
  getmetatable("").__index = function(self, i)
    if type(i) == 'number' then
      return sub(self, i, i)
    elseif type(i) == 'table' then
      return sub(self, i[1], i[2])
    else
      return text_methods[i]
    end
  end
  getmetatable("").__add = function(self, x)
    return tostring(self) .. tostring(x)
  end
end
return {
  List = List,
  Dict = Dict
}
