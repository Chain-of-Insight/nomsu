-- This file contains container classes, i.e. Lists, Dicts, and Sets

{:insert,:remove,:concat} = table
{:repr, :stringify, :equivalent, :nth_to_last, :size} = require 'utils'
lpeg = require 'lpeg'
re = require 're'

local List, Dict

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
        ret = List[x for x in *@]
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
        -- TODO: use stringify() to allow joining misc. objects?
        joined: => table.concat([tostring(x) for x in *@]),
        joined_with_1: (glue)=> table.concat([tostring(x) for x in *@], glue),
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
    -- TODO: remove this safety check to get better performance?
    __newindex: (k,v)=>
        assert type(k) == 'number', "List indices must be numbers"
        rawset(@, k, v)

List = (t)-> setmetatable(t, _list_mt)

walk_items = (i)=>
    i = i + 1
    k, v = next(@table, @key)
    if k != nil
        @key = k
        return i, Dict{key:k, value:v}

_dict_mt =
    __eq:equivalent
    __len:size
    __tostring: =>
        "{"..concat(["#{repr(k)}: #{repr(v)}" for k,v in pairs @], ", ").."}"
    __ipairs: => walk_items, {table:@, key:nil}, 0
    __band: (other)=>
        Dict{k,v for k,v in pairs(@) when other[k] != nil}
    __bor: (other)=>
        ret = {k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
        return Dict(ret)
    __bxor: (other)=>
        ret = {k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
            else ret[k] = nil
        return Dict(ret)
    __add: (other)=>
        ret = {k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
            else ret[k] += v
        return Dict(ret)
    __sub: (other)=>
        ret = {k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = -v
            else ret[k] -= v
        return Dict(ret)
Dict = (t)-> setmetatable(t, _dict_mt)

for i,entry in ipairs(Dict({x:99}))
    assert(i == 1 and entry.key == "x" and entry.value == 99, "ipairs compatibility issue")

do
    {:reverse, :upper, :lower, :find, :byte, :match, :gmatch, :gsub, :sub, :format, :rep} = string
    string2 = require 'string2'
    {:lines, :line, :line_at, :as_lua_id} = string2
    text_methods =
        formatted_with_1:format, byte_1:byte, position_of_1:find, position_of_1_after_2:find,
        bytes_1_to_2: (start, stop)=> List{byte(tostring(@), start, stop)}
        [as_lua_id "with 1 -> 2"]: gsub
        bytes: => List{byte(tostring(@), 1, -1)},
        lines: => List(lines(@))
        line_1: line
        wrap_to_1: (maxlen)=>
            _lines = {}
            for line in *@lines!
                while #line > maxlen
                    chunk = line\sub(1, maxlen)
                    split = chunk\find(' ', maxlen-8) or maxlen
                    chunk = line\sub(1, split)
                    line = line\sub(split+1, -1)
                    _lines[#_lines+1] = chunk
                _lines[#_lines+1] = line
            return table.concat(_lines, "\n")
                                
        line_at_1: (i)=> (line_at(@, i))
        line_number_of_1: (i)=> select(2, line_at(@, i))
        line_position_of_1: (i)=> select(3, line_at(@, i))
        matches_1: (patt)=> match(@, patt) and true or false
        [as_lua_id "* 1"]: (n)=> rep(@, n)
        matching_1: (patt)=>
            result = {}
            stepper,x,i = gmatch(@, patt)
            while true
                tmp = List{stepper(x,i)}
                break if #tmp == 0
                i = tmp[1]
                result[#result+1] = tmp
            return List(result)
    
    setmetatable(text_methods, {__index:string2})

    getmetatable("").__index = (i)=>
        -- Use [] for accessing text characters, or s[{3,4}] for s:sub(3,4)
        if type(i) == 'number' then return sub(@, i, i)
        elseif type(i) == 'table' then return sub(@, i[1], i[2])
        else return text_methods[i]

return {:List, :Dict}