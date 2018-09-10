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
local lpeg = require('lpeg')
local re = require('re')
local List, Dict
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
local Text
do
  local reverse, upper, lower, find, byte, match, gmatch, gsub, sub, format, rep
  do
    local _obj_0 = string
    reverse, upper, lower, find, byte, match, gmatch, gsub, sub, format, rep = _obj_0.reverse, _obj_0.upper, _obj_0.lower, _obj_0.find, _obj_0.byte, _obj_0.match, _obj_0.gmatch, _obj_0.gsub, _obj_0.sub, _obj_0.format, _obj_0.rep
  end
  local as_lua_id
  as_lua_id = function(str)
    str = gsub(str, "^\3*$", "%1\3")
    str = gsub(str, "x([0-9A-F][0-9A-F])", "x78%1")
    str = gsub(str, "%W", function(c)
      if c == ' ' then
        return '_'
      else
        return format("x%02X", byte(c))
      end
    end)
    str = gsub(str, "^_*%d", "_%1")
    return str
  end
  local line_matcher = re.compile([[ 
        lines <- {| line (%nl line)* |}
        line <- {(!%nl .)*}
    ]], {
    nl = lpeg.P("\r") ^ -1 * lpeg.P("\n")
  })
  local text_methods = {
    reversed = function(self)
      return reverse(tostring(self))
    end,
    uppercase = function(self)
      return upper(tostring(self))
    end,
    lowercase = function(self)
      return lower(tostring(self))
    end,
    as_lua_id = function(self)
      return as_lua_id(tostring(self))
    end,
    formatted_with_1 = function(self, args)
      return format(tostring(self), unpack(args))
    end,
    byte_1 = function(self, i)
      return byte(tostring(self), i)
    end,
    position_of_1 = function(self)
      return find(tostring(self))
    end,
    position_of_1_after_2 = function(self, i)
      return find(tostring(self), i)
    end,
    bytes_1_to_2 = function(self, start, stop)
      return List({
        byte(tostring(self), start, stop)
      })
    end,
    bytes = function(self)
      return List({
        byte(tostring(self), 1, #self)
      })
    end,
    capitalized = function(self)
      return gsub(tostring(self), '%l', upper, 1)
    end,
    lines = function(self)
      return List(line_matcher:match(self))
    end,
    matches_1 = function(self, patt)
      return match(tostring(self), patt) and true or false
    end,
    [as_lua_id("* 1")] = function(self, n)
      return rep(self, n)
    end,
    matching_1 = function(self, patt)
      local result = { }
      local stepper, x, i = gmatch(tostring(self), patt)
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
    end,
    [as_lua_id("with 1 -> 2")] = function(self, patt, sub)
      return gsub(tostring(self), patt, sub)
    end,
    _coalesce = function(self)
      if rawlen(self) > 1 then
        local s = table.concat(self)
        for i = rawlen(self), 2, -1 do
          self[i] = nil
        end
        self[1] = s
      end
      return self
    end
  }
  setmetatable(text_methods, {
    __index = string
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
  assert(("abc"):matches_1("ab"))
  local _ = [==[    text_metatable =
        __mul: (other)=>
            assert(type(other) == 'number', "Invalid type for multiplication")
            return rep(@, other)
        __index: (i)=>
            -- Use [] for accessing text characters, or s[{3,4}] for s:sub(3,4)
            if type(i) == 'number' then return sub(@, i, i)
            elseif type(i) == 'table' then return sub(@, i[1], i[2])
            else return text_methods[i]
        __tostring: => @_coalesce![1]
        __len: => #tostring(@)
        __concat: (other)=> tostring(@), tostring(other)
        __len: => #tostring(@)
        __eq: (other)=>
            type(@) == type(other) and getmetatable(@) == getmetatable(other) and tostring(@) == tostring(other)
        __lt: (other)=> tostring(@) < tostring(other)
        __le: (other)=> tostring(@) <= tostring(other)
        __newindex: => error("Cannot modify Text")

    Text = (s)-> setmetatable(s, text_metatable)
        ]==]
end
return {
  List = List,
  Dict = Dict,
  Text = Text
}
