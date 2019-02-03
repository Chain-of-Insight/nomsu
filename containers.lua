local List, Dict, Undict, _undict_mt, _dict_mt
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
  __type = "a List",
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
    return "a_List{" .. concat((function()
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
    end,
    copy = function(self)
      return List((function()
        local _accum_0 = { }
        local _len_0 = 1
        for i = 1, #self do
          _accum_0[_len_0] = self[i]
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)())
    end,
    reverse = function(self)
      local n = #self
      for i = 1, math.floor(n / 2) do
        self[i], self[n - i + 1] = self[n - i + 1], self[i]
      end
    end,
    reversed = function(self)
      return List((function()
        local _accum_0 = { }
        local _len_0 = 1
        for i = #self, 1, -1 do
          _accum_0[_len_0] = self[i]
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)())
    end,
    sort = function(self)
      return table.sort(self)
    end,
    sort_by = function(self, fn)
      local keys = setmetatable({ }, {
        __index = function(self, k)
          local key = fn(k)
          self[k] = key
          return key
        end
      })
      return table.sort(self, function(a, b)
        return keys[a] <= keys[b]
      end)
    end,
    sorted = function(self)
      local c = self:copy()
      c:sort()
      return c
    end,
    sorted_by = function(self, fn)
      local c = self:copy()
      c:sort_by(fn)
      return c
    end,
    filter_by = function(self, keep)
      local deleted = 0
      for i = 1, #self do
        if not (keep(self[i])) then
          deleted = deleted + 1
        elseif deleted > 0 then
          self[i - deleted] = self[i]
        end
      end
      for i = #self - deleted + 1, #self do
        self[i] = nil
      end
    end,
    filtered_by = function(self, keep)
      local c = self:copy()
      c:filter_by(keep)
      return c
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
  if type(t) == 'table' then
    return setmetatable(t, _list_mt)
  elseif type(t) == 'function' then
    local l = setmetatable({ }, _list_mt)
    local add
    add = function(...)
      for i = 1, select('#', ...) do
        l[#l + 1] = select(i, ...)
      end
    end
    t(add)
    return l
  else
    return error("Unsupported List type: " .. type(t))
  end
end
local compliments = setmetatable({ }, {
  __mode = 'k'
})
_undict_mt = {
  __index = function(self, k)
    return not compliments[self][k] and true or nil
  end,
  __newindex = function(self, k, v)
    if k then
      compliments[self][k] = nil
    else
      compliments[self][k] = true
    end
  end,
  __eq = function(self, other)
    if not (type(other) == 'table' and getmetatable(other) == getmetatable(self)) then
      return false
    end
    return compliments[self] == compliments[other]
  end,
  __len = function(self)
    return math.huge
  end,
  __tostring = function(self)
    return "~" .. _dict_mt.__tostring(compliments[self])
  end,
  as_nomsu = function(self)
    return "~" .. _dict_mt.as_nomsu(compliments[self])
  end,
  as_lua = function(self)
    return "~" .. __dict_mt.as_lua(compliments[self])
  end,
  __band = function(self, other)
    if getmetatable(other) == _undict_mt then
      return Undict(_dict_mt.__bor(compliments[self], compliments[other]))
    else
      return _dict_mt.__band(other, self)
    end
  end,
  __bor = function(self, other)
    if getmetatable(other) == _undict_mt then
      return Undict(_dict_mt.__band(compliments[self], compliments[other]))
    else
      return Undict((function()
        local _tbl_0 = { }
        for k, v in pairs(compliments[self]) do
          if not other[k] then
            _tbl_0[k] = v
          end
        end
        return _tbl_0
      end)())
    end
  end,
  __bxor = function(self, other)
    if getmetatable(other) == _undict_mt then
      return _dict_mt.__bxor(compliments[self], compliments[other])
    else
      return Undict(_dict_mt.__band(other, self))
    end
  end,
  __bnot = function(self)
    return Dict((function()
      local _tbl_0 = { }
      for k, v in pairs(compliments[self]) do
        _tbl_0[k] = v
      end
      return _tbl_0
    end)())
  end
}
Undict = function(d)
  local u = setmetatable({ }, _undict_mt)
  compliments[u] = Dict((function()
    local _tbl_0 = { }
    for k, v in pairs(d) do
      if v then
        _tbl_0[k] = true
      end
    end
    return _tbl_0
  end)())
  return u
end
_dict_mt = {
  __type = "a Dict",
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
        _accum_0[_len_0] = v == true and "." .. as_nomsu(k) or "." .. tostring(k) .. " = " .. tostring(v)
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
        _accum_0[_len_0] = v == true and "." .. as_nomsu(k) or "." .. tostring(as_nomsu(k)) .. " = " .. tostring(as_nomsu(v))
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), ", ") .. "}"
  end,
  as_lua = function(self)
    return "a_Dict{" .. concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for k, v in pairs(self) do
        _accum_0[_len_0] = "[ " .. tostring(as_lua(k)) .. "]= " .. tostring(as_lua(v))
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), ", ") .. "}"
  end,
  as_list = function(self)
    return List((function()
      local _accum_0 = { }
      local _len_0 = 1
      for k, v in pairs(self) do
        _accum_0[_len_0] = k
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())
  end,
  __band = function(self, other)
    return Dict((function()
      local _tbl_0 = { }
      for k, v in pairs(self) do
        if other[k] then
          _tbl_0[k] = v
        end
      end
      return _tbl_0
    end)())
  end,
  __bor = function(self, other)
    if getmetatable(other) == _undict_mt then
      return _undict_mt.__bor(other, self)
    end
    local ret = Dict((function()
      local _tbl_0 = { }
      for k, v in pairs(self) do
        _tbl_0[k] = v
      end
      return _tbl_0
    end)())
    for k, v in pairs(other) do
      if ret[k] == nil then
        ret[k] = v
      end
    end
    return ret
  end,
  __bxor = function(self, other)
    if getmetatable(other) == _undict_mt then
      return _undict_mt.__bxor(other, self)
    end
    local ret = Dict((function()
      local _tbl_0 = { }
      for k, v in pairs(self) do
        _tbl_0[k] = v
      end
      return _tbl_0
    end)())
    for k, v in pairs(other) do
      if ret[k] == nil then
        ret[k] = v
      else
        ret[k] = nil
      end
    end
    return ret
  end,
  __bnot = Undict,
  __add = function(self, other)
    local ret = Dict((function()
      local _tbl_0 = { }
      for k, v in pairs(self) do
        _tbl_0[k] = v
      end
      return _tbl_0
    end)())
    for k, v in pairs(other) do
      if ret[k] == nil then
        ret[k] = v
      else
        ret[k] = ret[k] + v
      end
    end
    return ret
  end,
  __sub = function(self, other)
    local ret = Dict((function()
      local _tbl_0 = { }
      for k, v in pairs(self) do
        _tbl_0[k] = v
      end
      return _tbl_0
    end)())
    for k, v in pairs(other) do
      if ret[k] == nil then
        ret[k] = -v
      else
        ret[k] = ret[k] - v
      end
    end
    return ret
  end
}
Dict = function(t)
  if type(t) == 'table' then
    return setmetatable(t, _dict_mt)
  elseif type(t) == 'function' then
    local d = setmetatable({ }, _dict_mt)
    local add
    add = function(...)
      for i = 1, select('#', ...) do
        d[select(i, ...)] = true
      end
    end
    local add_1_eq_2
    add_1_eq_2 = function(k, v)
      d[k] = v
    end
    t(add, add_1_eq_2)
    return d
  else
    return error("Unsupported Dict type: " .. type(t))
  end
end
return {
  List = List,
  Dict = Dict
}
