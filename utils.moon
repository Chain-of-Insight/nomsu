local utils
utils = {
    is_list: (t)->
        i = 1
        for _ in pairs(t)
            if t[i] == nil then return false
            i += 1
        return true

    repr: (x, add_quotes=false)->
        switch type(x)
            when 'table'
                if utils.is_list x
                    "{#{table.concat([utils.repr(i, true) for i in *x], ", ")}}"
                else
                    "{#{table.concat(["[#{utils.repr(k, true)}]: #{utils.repr(v, true)}" for k,v in pairs x], ", ")}}"
            when 'string'
                if not add_quotes
                    x
                elseif not x\find[["]] and not x\find"\n"
                    "\"#{x}\""
                elseif not x\find[[']] and not x\find"\n"
                    "\'#{x}\'"
                else
                    for i=0,math.huge
                        eq = ("=")\rep(i)
                        if not x\find"%[#{eq}%[" and not x\find"%]#{eq}%]"
                            return "[#{eq}[#{x}]#{eq}]"
            else
                tostring(x)
    
    split: (str, sep="%s")->
        [chunk for chunk in str\gmatch("[^#{sep}]+")]
    
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
        return setmetatable({:start,:stop,:step}, {
            __ipairs: =>
                iter = (i)=>
                    if i < (@stop-@start)/@step
                        return i+1, @start+i*@step
                return iter, @, 0
        })

    keys: (t)-> [k for k in pairs(t)]
    values: (t)-> [v for _,v in pairs(t)]
    set: (list)-> {i,true for i in *list}

    sum: (t)->
        with tot = 0
            for _,x in pairs(t) do tot += x

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
