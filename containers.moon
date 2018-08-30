-- This file contains container classes, i.e. Lists, Dicts, and Sets
{:insert,:remove,:concat} = table
{:repr, :stringify, :equivalent, :nth_to_last, :size} = require 'utils'

local list, dict

-- List and Dict classes to provide basic equality/tostring functionality for the tables
-- used in Nomsu. This way, they retain a notion of whether they were originally lists or dicts.
_list_mt =
    __eq:equivalent
    -- Could consider adding a __newindex to enforce list-ness, but would hurt performance
    __tostring: =>
        "["..concat([repr(b) for b in *@], ", ").."]"
    __lt: (other)=>
        assert type(@) == 'table' and type(other) == 'table', "Incompatible types for comparison"
        for i=1,math.max(#@, #other)
            if not @[i] and other[i] then return true
            elseif @[i] and not other[i] then return false
            elseif @[i] < other[i] then return true
            elseif @[i] > other[i] then return false
        return false
    __le: (other)=>
        assert type(@) == 'table' and type(other) == 'table', "Incompatible types for comparison"
        for i=1,math.max(#@, #other)
            if not @[i] and other[i] then return true
            elseif @[i] and not other[i] then return false
            elseif @[i] < other[i] then return true
            elseif @[i] > other[i] then return false
        return true
    __add: (other)=>
        ret = list[x for x in *@]
        for x in *other
            insert(ret, x)
        return ret
    __index:
        add_1: insert, append_1: insert
        add_1_at_index_2: (t,x,i)-> insert(t,i,x)
        at_index_1_add_2: insert
        pop: remove, remove_last: remove, remove_index_1: remove
        last: (=> @[#@]), first: (=> @[1])
        _1_st_to_last:nth_to_last, _1_nd_to_last:nth_to_last
        _1_rd_to_last:nth_to_last, _1_th_to_last:nth_to_last
        has_1: (item)=>
            for x in *@
                if x == item
                    return true
            return false
        index_of_1: (item)=>
            for i,x in ipairs @
                if x == item
                    return i
            return nil

list = (t)-> setmetatable(t, _list_mt)

walk_items = (i)=>
    i = i + 1
    k, v = next(@table, @key)
    if k != nil
        @key = k
        return i, dict{key:k, value:v}

_dict_mt =
    __eq:equivalent
    __len:size
    __tostring: =>
        "{"..concat(["#{repr(k)}: #{repr(v)}" for k,v in pairs @], ", ").."}"
    __ipairs: => walk_items, {table:@, key:nil}, 0
    __band: (other)=>
        dict{k,v for k,v in pairs(@) when other[k] != nil}
    __bor: (other)=>
        ret = {k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
        return dict(ret)
    __bxor: (other)=>
        ret = {k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
            else ret[k] = nil
        return dict(ret)
    __add: (other)=>
        ret = {k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
            else ret[k] += v
        return dict(ret)
    __sub: (other)=>
        ret = {k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = -v
            else ret[k] -= v
        return dict(ret)
dict = (t)-> setmetatable(t, _dict_mt)

for i,entry in ipairs(dict({x:99}))
    assert(i == 1 and entry.key == "x" and entry.value == 99, "ipairs compatibility issue")

return {:list, :dict}
