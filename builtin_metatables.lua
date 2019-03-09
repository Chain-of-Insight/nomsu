require("text")
local number_mt = {
  __type = "a Number",
  as_lua = tostring,
  as_nomsu = tostring,
  as_text = tostring,
  as_a_number = function(self)
    return self
  end,
  rounded = function(self)
    return math.floor(self + .5)
  end,
  rounded_down = math.floor,
  rounded_up = math.ceil,
  to_the_nearest = function(self, rounder)
    return rounder * math.floor(self / rounder + 0.5)
  end,
  base16 = function(self)
    return ("%X"):format(self)
  end
}
number_mt.__index = number_mt
debug.setmetatable(0, number_mt)
local bool_mt = {
  __type = "a Boolean",
  __len = function(self)
    return self and 1 or 0
  end,
  as_lua = tostring,
  as_nomsu = function(self)
    return self and "yes" or "no"
  end,
  as_text = function(self)
    return self and "yes" or "no"
  end,
  _and = function(self, cond)
    return self and cond
  end,
  _or = function(self, cond)
    return self or cond
  end,
  xor = function(self, cond)
    return self == (not cond)
  end
}
bool_mt.__index = bool_mt
debug.setmetatable(true, bool_mt)
local fn_mt = {
  __type = "an Action",
  as_text = function(self)
    return (tostring(self):gsub("function", "Action"))
  end,
  __add = function(self, other)
    if type(self) == 'function' and type(other) == 'function' then
      return function(...)
        return (self(...) + other(...))
      end
    elseif type(self) == 'function' then
      return function(...)
        return (self(...) + other)
      end
    else
      return function(...)
        return (self + other(...))
      end
    end
  end,
  __sub = function(self, other)
    if type(self) == 'function' and type(other) == 'function' then
      return function(...)
        return (self(...) - other(...))
      end
    elseif type(self) == 'function' then
      return function(...)
        return (self(...) - other)
      end
    else
      return function(...)
        return (self - other(...))
      end
    end
  end,
  __mul = function(self, other)
    if type(self) == 'function' and type(other) == 'function' then
      return function(...)
        return (self(...) * other(...))
      end
    elseif type(self) == 'function' then
      return function(...)
        return (self(...) * other)
      end
    else
      return function(...)
        return (self * other(...))
      end
    end
  end,
  __div = function(self, other)
    if type(self) == 'function' and type(other) == 'function' then
      return function(...)
        return (self(...)(other(...)))
      end
    elseif type(self) == 'function' then
      return function(...)
        return (self(...)(other))
      end
    else
      return function(...)
        return (self(other(...)))
      end
    end
  end,
  __mod = function(self, other)
    if type(self) == 'function' and type(other) == 'function' then
      return function(...)
        return (self(...) % other(...))
      end
    elseif type(self) == 'function' then
      return function(...)
        return (self(...) % other)
      end
    else
      return function(...)
        return (self % other(...))
      end
    end
  end,
  __band = function(self, other)
    if type(self) == 'function' and type(other) == 'function' then
      return function(...)
        return (self(...) and other(...))
      end
    elseif type(self) == 'function' then
      return function(...)
        return (self(...) and other)
      end
    else
      return function(...)
        return (self and other(...))
      end
    end
  end,
  __bor = function(self, other)
    if type(self) == 'function' and type(other) == 'function' then
      return function(...)
        return (self(...) or other(...))
      end
    elseif type(self) == 'function' then
      return function(...)
        return (self(...) or other)
      end
    else
      return function(...)
        return (self or other(...))
      end
    end
  end,
  __bxor = function(self, other)
    if type(self) == 'function' and type(other) == 'function' then
      return function(...)
        return (self(...) ~= other(...))
      end
    elseif type(self) == 'function' then
      return function(...)
        return (self(...) ~= other)
      end
    else
      return function(...)
        return (self ~= other(...))
      end
    end
  end
}
fn_mt.__index = fn_mt
debug.setmetatable((function() end), fn_mt)
local _last_co_i = setmetatable({ }, {
  __mode = 'k'
})
local co_mt
co_mt = {
  __type = "a Coroutine",
  as_text = function(self)
    return (tostring(self):gsub("thread", "Coroutine")) .. " (" .. coroutine.status(self) .. ")"
  end,
  __len = function(self)
    return math.huge
  end,
  __call = coroutine.resume,
  __next = function(self, k)
    local ok, val = coroutine.resume(self)
    if ok then
      return (k or 0) + 1, val
    end
  end,
  __index = function(self, k)
    if k == (_last_co_i[self] or 0) + 1 then
      local ret = {
        coroutine.resume(self, k)
      }
      _last_co_i[self] = k
      if ret[1] then
        return table.unpack(ret, 2)
      else
        return nil
      end
    end
    return co_mt[k]
  end
}
debug.setmetatable(coroutine.create(function() end), co_mt)
local nil_mt = {
  __type = "Nil",
  as_lua = function(self)
    return "nil"
  end,
  as_nomsu = function(self)
    return "nil"
  end,
  as_text = function(self)
    return "nil"
  end
}
nil_mt.__index = nil_mt
return debug.setmetatable(nil, nil_mt)
