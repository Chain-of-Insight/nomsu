local bitlib
if jit then
  bitlib = require('bit')
elseif _VERSION == "Lua 5.2" then
  bitlib = bit32
else
  bitlib = error("no bit library for Lua 5.3+")
end
local ret
do
  local _tbl_0 = { }
  for k, v in pairs(bitlib) do
    _tbl_0[k] = v
  end
  ret = _tbl_0
end
ret.bnot = function(x)
  do
    local mt = getmetatable(x)
    if mt then
      if mt.__bnot then
        return mt.__bnot(x)
      end
    end
  end
  return bitlib.bnot(x)
end
ret.band = function(x, y)
  do
    local mt_x = getmetatable(x)
    if mt_x then
      if mt_x.__band then
        return mt_x.__band(x, y)
      end
    end
  end
  do
    local mt_y = getmetatable(x)
    if mt_y then
      if mt_y.__band then
        return mt_y.__band(x, y)
      end
    end
  end
  return bitlib.band(x, y)
end
ret.bor = function(x, y)
  do
    local mt_x = getmetatable(x)
    if mt_x then
      if mt_x.__bor then
        return mt_x.__bor(x, y)
      end
    end
  end
  do
    local mt_y = getmetatable(x)
    if mt_y then
      if mt_y.__bor then
        return mt_y.__bor(x, y)
      end
    end
  end
  return bitlib.bor(x, y)
end
ret.bxor = function(x, y)
  do
    local mt_x = getmetatable(x)
    if mt_x then
      if mt_x.__bxor then
        return mt_x.__bxor(x, y)
      end
    end
  end
  do
    local mt_y = getmetatable(x)
    if mt_y then
      if mt_y.__bxor then
        return mt_y.__bxor(x, y)
      end
    end
  end
  return bitlib.bxor(x, y)
end
ret.lshift = function(x, y)
  do
    local mt_x = getmetatable(x)
    if mt_x then
      if mt_x.__shl then
        return mt_x.__shl(x, y)
      end
    end
  end
  do
    local mt_y = getmetatable(x)
    if mt_y then
      if mt_y.__shl then
        return mt_y.__shl(x, y)
      end
    end
  end
  return bitlib.lshift(x, y)
end
ret.rshift = function(x, y)
  do
    local mt_x = getmetatable(x)
    if mt_x then
      if mt_x.__shr then
        return mt_x.__shr(x, y)
      end
    end
  end
  do
    local mt_y = getmetatable(x)
    if mt_y then
      if mt_y.__shr then
        return mt_y.__shr(x, y)
      end
    end
  end
  return bitlib.rshift(x, y)
end
return ret
