-- This file contains container classes, i.e. Lists and Dicts, plus some extended string functionality
local List, Dict, Undict, _undict_mt, _dict_mt
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
        "a_List{"..concat([as_lua(b) for b in *@], ", ").."}"
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
        copy: => List[@[i] for i=1,#@]
        reverse: =>
            n = #@
            for i=1,math.floor(n/2)
                @[i], @[n-i+1] = @[n-i+1], @[i]
        reversed: => List[@[i] for i=#@,1,-1]
        sort: => table.sort(@)
        sort_by: (fn)=>
            keys = setmetatable {},
                __index: (k)=>
                    key = fn(k)
                    @[k] = key
                    return key
            table.sort(@, (a,b)->keys[a] <= keys[b])
        sorted: =>
            c = @copy!
            c\sort!
            return c
        sorted_by: (fn)=>
            c = @copy!
            c\sort_by(fn)
            return c
        filter_by: (keep)=>
            deleted = 0
            for i=1,#@
                unless keep(@[i])
                    deleted += 1
                elseif deleted > 0
                    @[i-deleted] = @[i]
            for i=#@-deleted+1,#@
                @[i] = nil

        filtered_by: (keep)=>
            c = @copy!
            c\filter_by(keep)
            return c

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


compliments = setmetatable({}, {__mode:'k'})
_undict_mt =
    __type: "an Inverse Dict"
    __index: (k)=> not compliments[@][k] and true or nil
    __newindex: (k,v)=>
        if k
            compliments[@][k] = nil
        else
            compliments[@][k] = true
    __eq: (other)=>
        unless type(other) == 'table' and getmetatable(other) == getmetatable(@)
            return false
        return compliments[@] == compliments[other]
    __len: => math.huge
    __tostring: => "~".._dict_mt.__tostring(compliments[@])
    as_nomsu: => "~".._dict_mt.as_nomsu(compliments[@])
    as_lua: => "~"..__dict_mt.as_lua(compliments[@])
    __band: (other)=>
        if getmetatable(other) == _undict_mt
            -- ~{x,y} & ~{y,z} == ~{x,y,z} == ~({x,y} | {y,z})
            Undict(_dict_mt.__bor(compliments[@], compliments[other]))
        else
            -- ~{x,y} & {y,z} == {z} == {y,z} & ~{x,y}
            _dict_mt.__band(other, @)
    __bor: (other)=>
        if getmetatable(other) == _undict_mt
            -- ~{x,y} | ~{y,z} == ~{y} = ~({x,y} & {y,z})
            Undict(_dict_mt.__band(compliments[@], compliments[other]))
        else
            -- ~{x,y} | {y,z} == ~{z} = ~({y,z} & ~{x,y})
            Undict{k,v for k,v in pairs(compliments[@]) when not other[k]}
    __bxor: (other)=>
        if getmetatable(other) == _undict_mt
            -- ~{x,y} ^ ~{y,z} == {x,z} = {x,y} ^ {y,z}
            _dict_mt.__bxor(compliments[@], compliments[other])
        else
            -- ~{x,y} ^ {y,z} == ~{x} = ~({x,y} & ~{y,z})
            Undict(_dict_mt.__band(other, @))
    __bnot: => Dict{k,v for k,v in pairs(compliments[@])}

Undict = (d)->
    u = setmetatable({}, _undict_mt)
    compliments[u] = Dict{k,true for k,v in pairs(d) when v}
    return u

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
        "{"..concat([v == true and "."..as_nomsu(k) or ".#{k} = #{v}" for k,v in pairs @], ", ").."}"
    as_nomsu: =>
        "{"..concat([v == true and "."..as_nomsu(k) or ".#{as_nomsu(k)} = #{as_nomsu(v)}" for k,v in pairs @], ", ").."}"
    as_lua: =>
        "a_Dict{"..concat(["[ #{as_lua(k)}]= #{as_lua(v)}" for k,v in pairs @], ", ").."}"
    as_list: => List[k for k,v in pairs(@)]
    __band: (other)=>
        Dict{k,v for k,v in pairs(@) when other[k]}
    __bor: (other)=>
        if getmetatable(other) == _undict_mt
            return _undict_mt.__bor(other, @)
        ret = Dict{k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
        return ret
    __bxor: (other)=>
        if getmetatable(other) == _undict_mt
            return _undict_mt.__bxor(other, @)
        ret = Dict{k,v for k,v in pairs(@)}
        for k,v in pairs(other)
            if ret[k] == nil then ret[k] = v
            else ret[k] = nil
        return ret
    __bnot: Undict

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

return {:List, :Dict}
