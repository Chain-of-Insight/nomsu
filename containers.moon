-- This file contains container classes, i.e. Lists and Dicts, plus some extended string functionality
local List, Dict
{:insert,:remove,:concat} = table

as_nomsu = =>
    if type(@) == 'number'
        return tostring(@)
    if mt = getmetatable(@)
        if _as_nomsu = mt.as_nomsu
            return _as_nomsu(@)
    return tostring(@)

as_lua = =>
    if type(@) == 'number'
        return tostring(@)
    if mt = getmetatable(@)
        if _as_lua = mt.as_lua
            return _as_lua(@)
    return tostring(@)

nth_to_last = (n)=> @[#@-n+1]

-- List and Dict classes to provide basic equality/tostring functionality for the tables
-- used in Nomsu. This way, they retain a notion of whether they were originally lists or dicts.

_list_mt =
    __type: "a List"
    __eq: (other)=>
        unless type(other) == 'table' and getmetatable(other) == getmetatable(@) and #other == #@
            return false
        for i,x in ipairs(@)
            return false unless x == other[i]
        return true
    -- Could consider adding a __newindex to enforce list-ness, but would hurt performance
    __tostring: =>
        "["..concat([as_nomsu(b) for b in *@], ", ").."]"
    as_nomsu: =>
        "["..concat([as_nomsu(b) for b in *@], ", ").."]"
    as_lua: =>
        "List{"..concat([as_lua(b) for b in *@], ", ").."}"
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
        add: insert, append: insert
        add_1_at_index: (t,x,i)-> insert(t,i,x)
        at_index_1_add: insert
        pop: remove, remove_last: remove, remove_index: remove
        last: (=> @[#@]), first: (=> @[1])
        _1_st_to_last:nth_to_last, _1_nd_to_last:nth_to_last
        _1_rd_to_last:nth_to_last, _1_th_to_last:nth_to_last
        joined: => table.concat([tostring(x) for x in *@]),
        joined_with: (glue)=> table.concat([tostring(x) for x in *@], glue),
        has: (item)=>
            for x in *@
                if x == item
                    return true
            return false
        remove: (item)=>
            for i,x in ipairs @
                if x == item
                    remove(@, i)
        index_of: (item)=>
            for i,x in ipairs @
                if x == item
                    return i
            return nil
        from_1_to: (start, stop)=>
            n = #@
            start = (n+1-start) if start < 0
            stop = (n+1-stop) if stop < 0
            return List[@[i] for i=start,stop]
    -- TODO: remove this safety check to get better performance?
    __newindex: (k,v)=>
        assert type(k) == 'number', "List indices must be numbers"
        rawset(@, k, v)
_list_mt.__index.as_lua = _list_mt.as_lua
_list_mt.__index.as_nomsu = _list_mt.as_nomsu

List = (t)->
    if type(t) == 'table'
        return setmetatable(t, _list_mt)
    elseif type(t) == 'function'
        l = setmetatable({}, _list_mt)
        add = (...)->
            for i=1,select('#',...) do l[#l+1] = select(i,...)
        t(add)
        return l
    else error("Unsupported List type: "..type(t))

_dict_mt =
    __type: "a Dict"
    __eq: (other)=>
        unless type(other) == 'table' and getmetatable(other) == getmetatable(@)
            return false
        for k,v in pairs(@)
            return false unless v == other[k]
        for k,v in pairs(other)
            return false unless v == @[k]
        return true
    __len: =>
        n = 0
        for _ in pairs(@) do n += 1
        return n
    __tostring: =>
        "{"..concat([".#{k} = #{v}" for k,v in pairs @], ", ").."}"
    as_nomsu: =>
        "{"..concat([".#{as_nomsu(k)} = #{as_nomsu(v)}" for k,v in pairs @], ", ").."}"
    as_lua: =>
        "Dict{"..concat(["[ #{as_lua(k)}]= #{as_lua(v)}" for k,v in pairs @], ", ").."}"
    __band: (other)=>
        Dict{k,v for k,v in pairs(@) when other[k] != nil}
    __bor: (other)=>
        ret = Dict{k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
        return ret
    __bxor: (other)=>
        ret = Dict{k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
            else ret[k] = nil
        return ret
    __add: (other)=>
        ret = Dict{k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
            else ret[k] += v
        return ret
    __sub: (other)=>
        ret = Dict{k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = -v
            else ret[k] -= v
        return ret
Dict = (t)->
    if type(t) == 'table'
        return setmetatable(t, _dict_mt)
    elseif type(t) == 'function'
        d = setmetatable({}, _dict_mt)
        add = (...)->
            for i=1,select('#',...) do d[select(i,...)] = true
        add_1_eq_2 = (k, v)-> d[k] = v
        t(add, add_1_eq_2)
        return d
    else error("Unsupported Dict type: "..type(t))


do
    {:reverse, :upper, :lower, :find, :byte, :match, :gmatch, :gsub, :sub, :format, :rep} = string
    string2 = require 'string2'
    {:lines, :line, :line_at, :as_lua_id, :is_lua_id} = string2
    text_methods =
        formatted_with:format, byte:byte,
        position_of:((...)->(find(...))), position_of_1_after:((...)->(find(...))),
        as_a_lua_identifier: as_lua_id, is_a_lua_identifier: is_lua_id,
        as_a_lua_id: as_lua_id, is_a_lua_id: is_lua_id,
        bytes_1_to: (start, stop)=> List{byte(tostring(@), start, stop)}
        [as_lua_id "with 1 ->"]: (...)-> (gsub(...))
        bytes: => List{byte(tostring(@), 1, -1)},
        lines: => List(lines(@))
        line: line
        wrapped_to: (maxlen)=>
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
                                
        line_at: (i)=> (line_at(@, i))
        line_number_at: (i)=> select(2, line_at(@, i))
        line_position_at: (i)=> select(3, line_at(@, i))
        matches: (patt)=> match(@, patt) and true or false
        matching: (patt)=> (match(@, patt))
        matching_groups: (patt)=> List{match(@, patt)}
        [as_lua_id "* 1"]: (n)=> rep(@, n)
        all_matches_of: (patt)=>
            result = {}
            stepper,x,i = gmatch(@, patt)
            while true
                tmp = List{stepper(x,i)}
                break if #tmp == 0
                i = tmp[1]
                result[#result+1] = (#tmp == 1) and tmp[1] or tmp
            return List(result)
        from_1_to: sub, from: sub,
        character: (i)=> sub(@, i, i)
    
    setmetatable(text_methods, {__index:string2})
    getmetatable("").__methods = text_methods
    getmetatable("").__index = text_methods
    getmetatable("").__add = (x)=> tostring(@)..tostring(x)

return {:List, :Dict}
