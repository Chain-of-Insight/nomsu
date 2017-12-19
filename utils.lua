
local function is_list(t)
    if type(t) ~= 'table' then
        return false
    end
    local i = 1
    for _ in pairs(t) do
        if t[i] == nil then
            return false
        end
        i = i + 1
    end
    return true
end

local function size(t)
    local n = 0
    for _ in pairs(t) do
        n = n + 1
    end
    return n
end

local function repr(x)
    local x_type = type(x)
    if x_type == 'table' then
        local mt = getmetatable(x)
        if mt and mt.__tostring then
            return mt.__tostring(x)
        elseif is_list(x) then
            local ret = {}
            for i=1,#x do
                ret[i] = repr(x[i])
            end
            return "{"..table.concat(ret, ", ").."}"
        else
            local ret = {}
            for k, v in pairs(x) do
                ret[#ret+1] = "["..repr(k).."]= "..repr(v)
            end
            return "{"..table.concat(ret, ", ").."}"
        end
    elseif x_type == 'string' then
        if x == "\n" then
            return "'\\n'"
        elseif not x:find([["]]) and not x:find("\n") and not x:find("\\") then
            return "\"" .. x .. "\""
        elseif not x:find([[']]) and not x:find("\n") and not x:find("\\") then
            return "\'" .. x .. "\'"
        else
            for i = 0, math.huge do
                local eq = ("="):rep(i)
                if not x:find("%]"..eq.."%]") and x:sub(-#eq-1, -1) ~= "]"..eq then
                    if x:sub(1, 1) == "\n" then
                        return "["..eq.."[\n"..x.."]"..eq.."]"
                    else
                        return "["..eq.."["..x.."]"..eq.."]"
                    end
                end
            end
        end
    else
        return tostring(x)
    end
end

local function stringify(x)
    if type(x) == 'string' then
        return x
    else
        return repr(x)
    end
end

local function split(str, sep)
    if sep == nil then
        sep = "%s"
    end
    local ret = {}
    for chunk in str:gmatch("[^"..sep.."]+") do
        ret[#ret+1] = chunk
    end
    return ret
end

local function remove_from_list(list, item)
    for i=1,#list do
        if list[i] == item then
            table.remove(list, i)
            return 
        end
    end
end

local function accumulate(glue, co)
    if co == nil then
        glue, co = "", glue
    end
    local bits = { }
    for bit in coroutine.wrap(co) do
        bits[#bits+1] = bit
    end
    return table.concat(bits, glue)
end

local function nth_to_last(list, n)
    return list[#list - n + 1]
end

local function keys(t)
    local ret = {}
    for k in pairs(t) do
        ret[#ret+1] = k
    end
    return ret
end

local function values(t)
    local ret = {}
    for _,v in pairs(t) do
        ret[#ret+1] = v
    end
    return ret
end

local function set(list)
    local ret = {}
    for i=1,#list do
        ret[list[i]] = true
    end
    return ret
end

local function sum(t)
    local tot = 0
    for i=1,#t do
        tot = tot + t[i]
    end
    return tot
end

local function product(t)
    if #t > 5 and 0 < t[1] and t[1] < 1 then
        local log, log_prod = math.log, 0
        for i=1,#t do
            log_prod = log_prod + log(t[i])
        end
        return math.exp(log_prod)
    else
        local prod = 1
        for i=1,#t do
            prod = prod * t[i]
        end
        return prod
    end
end

local function all(t)
    for i=1,#t do
        if not t[i] then return false end
    end
    return true
end

local function any(t)
    for i=1,#t do
        if t[i] then return true end
    end
    return false
end

local function min(list, keyFn)
    if keyFn == nil then
        keyFn = (function(x)
            return x
        end)
    end
    if type(keyFn) == 'table' then
        local keyTable = keyFn
        keyFn = function(k)
            return keyTable[k]
        end
    end
    local best, bestKey = list[1], keyFn(best)
    for i = 2, #list do
        local key = keyFn(list[i])
        if key < bestKey then
            best, bestKey = list[i], key
        end
    end
    return best
end

local function max(list, keyFn)
    if keyFn == nil then
        keyFn = (function(x)
            return x
        end)
    end
    if type(keyFn) == 'table' then
        local keyTable = keyFn
        keyFn = function(k)
            return keyTable[k]
        end
    end
    local best, bestKey = list[1], keyFn(best)
    for i = 2, #list do
        local key = keyFn(list[i])
        if key > bestKey then
            best, bestKey = list[i], key
        end
    end
    return best
end

local function sort(list, keyFn, reverse)
    if keyFn == nil then
        keyFn = (function(x)
            return x
        end)
    end
    if reverse == nil then
        reverse = false
    end
    if type(keyFn) == 'table' then
        local keyTable = keyFn
        keyFn = function(k)
            return keyTable[k]
        end
    end
    table.sort(list, reverse
        and (function(x,y) return keyFn(x) > keyFn(y) end)
        or  (function(x,y) return keyFn(x) < keyFn(y) end))
    return list
end

local function equivalent(x, y, depth)
    depth = depth or 1
    if x == y then
        return true
    end
    if type(x) ~= type(y) then
        return false
    end
    if type(x) ~= 'table' then
        return false
    end
    if depth == 0 then
        return false
    end
    local checked = {}
    for k, v in pairs(x) do
        if not equivalent(y[k], v, depth - 1) then
            return false
        end
        checked[k] = true
    end
    for k, v in pairs(y) do
        if not checked[k] and not equivalent(x[k], v, depth - 1) then
            return false
        end
    end
    return true
end

local function key_for(t, value)
    for k, v in pairs(t) do
        if v == value then
            return k
        end
    end
    return nil
end

local function clamp(x, min, max)
    if x < min then
        return min
    elseif x > max then
        return max
    else
        return x
    end
end

local function mix(min, max, amount)
    return (1 - amount) * min + amount * max
end

local function sign(x)
    if x == 0 then
        return 0
    elseif x < 0 then
        return -1
    else
        return 1
    end
end

local function round(x, increment)
    if increment == nil then
        increment = 1
    end
    if x >= 0 then
        return math.floor(x / increment + .5) * increment
    else
        return math.ceil(x / increment - .5) * increment
    end
end

return {is_list=is_list, size=size, repr=repr, stringify=stringify, split=split,
    remove_from_list=remove_from_list, accumulate=accumulate, nth_to_last=nth_to_last,
    keys=keys, values=values, set=set, sum=sum, product=product, all=all, any=any,
    min=min, max=max, sort=sort, equivalent=equivalent, key_for=key_for, clamp=clamp,
    mix=mix, round=round}
