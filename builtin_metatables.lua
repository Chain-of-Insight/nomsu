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
  as_lua = tostring,
  as_nomsu = function(self)
    return self and "yes" or "no"
  end,
  as_text = function(self)
    return self and "yes" or "no"
  end
}
bool_mt.__index = bool_mt
debug.setmetatable(true, bool_mt)
local fn_mt = {
  __type = "an Action",
  as_text = function(self)
    return (tostring(self):gsub("function", "Action"))
  end
}
fn_mt.__index = fn_mt
debug.setmetatable((function() end), fn_mt)
local co_mt = {
  __type = "a Coroutine",
  as_text = function(self)
    return (tostring(self):gsub("thread", "Coroutine"))
  end
}
co_mt.__index = co_mt
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
