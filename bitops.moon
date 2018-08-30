-- This file defines wrapper functions around the Lua 5.2/LuaJIT bitwise operators
-- The wrapper functions check for the appropriate metatable functions like
-- "__bxor" for bit.bxor(), etc.
bitlib = if jit then require('bit')
elseif _VERSION == "Lua 5.2" bit32
else error("no bit library for Lua 5.3+")

ret = {k,v for k,v in pairs(bitlib)}
ret.bnot = (x)->
    if mt = getmetatable(x)
        if mt.__bnot then return mt.__bnot(x)
    return bitlib.bnot(x)
ret.band = (x,y)->
    if mt_x = getmetatable(x)
        if mt_x.__band then return mt_x.__band(x, y)
    if mt_y = getmetatable(x)
        if mt_y.__band then return mt_y.__band(x, y)
    return bitlib.band(x, y)
ret.bor = (x,y)->
    if mt_x = getmetatable(x)
        if mt_x.__bor then return mt_x.__bor(x, y)
    if mt_y = getmetatable(x)
        if mt_y.__bor then return mt_y.__bor(x, y)
    return bitlib.bor(x, y)
ret.bxor = (x,y)->
    if mt_x = getmetatable(x)
        if mt_x.__bxor then return mt_x.__bxor(x, y)
    if mt_y = getmetatable(x)
        if mt_y.__bxor then return mt_y.__bxor(x, y)
    return bitlib.bxor(x, y)
ret.lshift = (x,y)->
    if mt_x = getmetatable(x)
        if mt_x.__shl then return mt_x.__shl(x, y)
    if mt_y = getmetatable(x)
        if mt_y.__shl then return mt_y.__shl(x, y)
    return bitlib.lshift(x, y)
ret.rshift = (x,y)->
    if mt_x = getmetatable(x)
        if mt_x.__shr then return mt_x.__shr(x, y)
    if mt_y = getmetatable(x)
        if mt_y.__shr then return mt_y.__shr(x, y)
    return bitlib.rshift(x, y)

return ret
