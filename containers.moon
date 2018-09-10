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

local Text
do
    {:reverse, :upper, :lower, :find, :byte, :match, :gmatch, :gsub, :sub, :format, :rep} = string

    -- Convert an arbitrary text into a valid Lua identifier. This function is injective,
    -- but not idempotent, i.e. if (x != y) then (as_lua_id(x) != as_lua_id(y)),
    -- but as_lua_id(x) is not necessarily equal to as_lua_id(as_lua_id(x))
    as_lua_id = (str)->
        -- Empty strings are not valid lua identifiers, so treat them like "\3",
        -- and treat "\3" as "\3\3", etc. to preserve injectivity.
        str = gsub str, "^\3*$", "%1\3"
        -- Escape 'x' when it precedes something that looks like an uppercase hex sequence.
        -- This way, all Lua IDs can be unambiguously reverse-engineered, but normal usage
        -- of 'x' won't produce ugly Lua IDs.
        -- i.e. "x" -> "x", "oxen" -> "oxen", but "Hex2Dec" -> "Hex782Dec" and "He-ec" -> "Hex2Dec"
        str = gsub str, "x([0-9A-F][0-9A-F])", "x78%1"
        -- Map spaces to underscores, and everything else non-alphanumeric to hex escape sequences
        str = gsub str, "%W", (c)->
            if c == ' ' then '_'
            else format("x%02X", byte(c))
        -- Lua IDs can't start with numbers, so map "1" -> "_1", "_1" -> "__1", etc.
        str = gsub str, "^_*%d", "_%1"
        return str

    line_matcher = re.compile([[ 
        lines <- {| line (%nl line)* |}
        line <- {(!%nl .)*}
    ]], nl:lpeg.P("\r")^-1 * lpeg.P("\n"))

    text_methods =
        reversed:=>reverse(tostring @)
        uppercase:=>upper(tostring @)
        lowercase:=>lower(tostring @)
        as_lua_id:=>as_lua_id(tostring @)
        formatted_with_1:(args)=>format(tostring(@), unpack(args))
        byte_1:(i)=>byte(tostring(@), i)
        position_of_1:=>find(tostring @),
        position_of_1_after_2:(i)=> find(tostring(@), i)
        bytes_1_to_2: (start, stop)=> List{byte(tostring(@), start, stop)}
        bytes: => List{byte(tostring(@), 1, #@)},
        capitalized: => gsub(tostring(@), '%l', upper, 1)
        lines: => List(line_matcher\match(@))
        matches_1: (patt)=> match(tostring(@), patt) and true or false
        [as_lua_id "* 1"]: (n)=> rep(@, n)
        matching_1: (patt)=>
            result = {}
            stepper,x,i = gmatch(tostring(@), patt)
            while true
                tmp = List{stepper(x,i)}
                break if #tmp == 0
                i = tmp[1]
                result[#result+1] = tmp
            return List(result)
        [as_lua_id "with 1 -> 2"]: (patt, sub)=> gsub(tostring(@), patt, sub)
        _coalesce: =>
            if rawlen(@) > 1
                s = table.concat(@)
                for i=rawlen(@), 2, -1 do @[i] = nil
                @[1] = s
            return @
    
    setmetatable(text_methods, {__index:string})

    getmetatable("").__index = (i)=>
        -- Use [] for accessing text characters, or s[{3,4}] for s:sub(3,4)
        if type(i) == 'number' then return sub(@, i, i)
        elseif type(i) == 'table' then return sub(@, i[1], i[2])
        else return text_methods[i]
    
    assert(("abc")\matches_1("ab"))

    [==[
    text_metatable =
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

return {:List, :Dict, :Text}
