local utils
utils = {
    is_list: (t)->
        if type(t) != 'table' then return false
        i = 1
        for _ in pairs(t)
            if t[i] == nil then return false
            i += 1
        return true

    size: (t)->
        with n = 0
            for _ in pairs(t) do n += 1

    repr: (x)->
        switch type(x)
            when 'table'
                mt = getmetatable(x)
                if mt and mt.__tostring
                    mt.__tostring(x)
                elseif utils.is_list x
                    "{#{table.concat([utils.repr(i) for i in *x], ", ")}}"
                else
                    "{#{table.concat(["[#{utils.repr(k)}]= #{utils.repr(v)}" for k,v in pairs x], ", ")}}"
            when 'string'
                if x == "\n"
                    return "'\\n'"
                elseif not x\find[["]] and not x\find"\n" and not x\find"\\"
                    "\""..x.."\""
                elseif not x\find[[']] and not x\find"\n" and not x\find"\\"
                    "\'"..x.."\'"
                else
                    for i=0,math.huge
                        eq = ("=")\rep(i)
                        if not x\find"%]#{eq}%]" and not x\match(".*]#{eq}$")
                            -- Stupid bullshit add an extra newline because lua discards first one if it exists
                            if x\sub(1,1) == "\n"
                                return "[#{eq}[\n"..x.."]#{eq}]"
                            else
                                return "[#{eq}["..x.."]#{eq}]"
            else
                tostring(x)
    
    stringify: (x)->
        if type(x) == 'string' then x
        else utils.repr(x)
    
    split: (str, sep="%s")->
        [chunk for chunk in str\gmatch("[^#{sep}]+")]
    
    remove_from_list: (list, item)->
        for i,list_item in ipairs(list)
            if list_item == item
                table.remove list, i
                return
    
    accumulate: (glue, co)->
        if co == nil then glue, co = "", glue
        bits = {}
        for bit in coroutine.wrap(co)
            table.insert(bits, bit)
        return table.concat(bits, glue)

    range: (start,stop,step)->
        if stop == nil
            start,stop,step = 1,start,1
        elseif step == nil
            step = 1
        elseif step == 0
            error("Range step cannot be zero.")
        return setmetatable({:start,:stop,:step}, {
            __ipairs: =>
                iter = (i)=>
                    if i <= (@stop-@start)/@step
                        return i+1, @start+i*@step
                return iter, @, 0
            __index: (i)=>
                if type(i) != "Number" then return nil
                if i % 1 != 0 then return nil
                if i <= 0 or i-1 > (@stop-@start)/@step then return nil
                return @start + (i-1)*@step
            __len: =>
                len = (@stop-@start)/@step
                if len < 0 then len = 0
                return len

        })
    
    nth_to_last: (list, n) -> list[#list-n+1]

    keys: (t)-> [k for k in pairs(t)]
    values: (t)-> [v for _,v in pairs(t)]
    set: (list)-> {i,true for i in *list}

    sum: (t)->
        with tot = 0
            for _,x in pairs(t) do tot += x

    product: (t)->
        with prod = 1
            for _,x in pairs(t) do prod *= x

    all: (t)->
        for _,x in pairs t
            if not x then return false
        return true

    any: (t)->
        for _,x in pairs t
            if x then return true
        return false

    min: (list, keyFn=((x)->x))->
        assert utils.is_list(list), "min() expects to be operating on a list"
        with best = list[1]
            if type(keyFn) == 'table'
                keyTable = keyFn
                keyFn = (k)->keyTable[k]
            for i=2,#list
                if keyFn(list[i]) < keyFn(best)
                    best = list[i]
    
    max: (list, keyFn=((x)->x))->
        assert utils.is_list(list), "min() expects to be operating on a list"
        with best = list[1]
            if type(keyFn) == 'table'
                keyTable = keyFn
                keyFn = (k)->keyTable[k]
            for i=2,#list
                if keyFn(list[i]) > keyFn(best)
                    best = list[i]
    
    sort: (list, keyFn=((x)->x), reverse=false)->
        assert utils.is_list(list), "min() expects to be operating on a list"
        if type(keyFn) == 'table'
            keyTable = keyFn
            keyFn = (k)->keyTable[k]
        comparison = if reverse then ((x,y)->(keyFn(x)>keyFn(y))) else ((x,y)->(keyFn(x)<keyFn(y)))
        table.sort list, comparison

    equivalent: (x,y)->
        if x == y then return true
        if type(x) != type(y) then return false
        if type(x) != 'table' then return false
        for k,v in pairs(x)
            if y[k] != v
                return false
        for k,v in pairs(y)
            if x[k] != v
                return false
        return true

    key_for: (t, value)->
        for k,v in pairs(t)
            if v == value
                return k
        return nil

    clamp: (x, min,max)->
        if x < min then min
        elseif x > max then max
        else x

    mix: (min,max, amount)->
        (1-amount)*min + amount*max

    sign: (x)->
        if x == 0 then 0
        elseif x < 0 then -1
        else 1

    round: (x, increment=1)->
        if x >= 0 then math.floor(x/increment + .5)*increment
        else math.ceil(x/increment - .5)*increment

}
return utils
