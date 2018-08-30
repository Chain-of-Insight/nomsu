local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local repr, stringify, equivalent, nth_to_last, size
do
  local _obj_0 = require('utils')
  repr, stringify, equivalent, nth_to_last, size = _obj_0.repr, _obj_0.stringify, _obj_0.equivalent, _obj_0.nth_to_last, _obj_0.size
end
local list, dict
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
    local ret = list((function()
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
    end
  }
}
list = function(t)
  return setmetatable(t, _list_mt)
end
local walk_items
walk_items = function(self, i)
  i = i + 1
  local k, v = next(self.table, self.key)
  if k ~= nil then
    self.key = k
    return i, dict({
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
        _accum_0[_len_0] = tostring(repr(k)) .. ": " .. tostring(repr(v))
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
    return dict((function()
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
    return dict(ret)
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
    return dict(ret)
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
    return dict(ret)
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
    return dict(ret)
  end
}
dict = function(t)
  return setmetatable(t, _dict_mt)
end
for i, entry in ipairs(dict({
  x = 99
})) do
  assert(i == 1 and entry.key == "x" and entry.value == 99, "ipairs compatibility issue")
end
return {
  list = list,
  dict = dict
}
