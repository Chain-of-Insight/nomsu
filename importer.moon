-- This file defines Importer, which is a type of table that can import from other tables

import_to_1_from = (host, to_import, prefix=nil)->
    if host_mt = getmetatable(host)
        if host_mt.__import
            host_mt.__import(host, to_import, prefix)
            return
    for k,v in pairs(to_import)
        if k == to_import then k = host
        if v == to_import then v = host
        if prefix and type(k) == 'string'
            --print "PREFIXING #{k} -> #{prefix..k}"
            k = prefix..k
        --print("IMPORTED (#{k})")
        host[k] = v
_imports = setmetatable({}, {__mode:"k"})
Importer = setmetatable({
    __index: (key)=> _imports[@][key]
    __import: (to_import, prefix=nil)=>
        imports = assert _imports[@]
        for k,v in pairs(to_import)
            if prefix and type(k) == 'string'
                k = prefix..k
            --print("IMPORTED (#{k})")
            imports[k] = v
            continue if v == to_import
            conflict = @[k]
            if conflict_mt = getmetatable(host)
                if conflict_mt.__import
                    conflict_mt.__import(conflict, v, prefix)
    --__newindex: (k,v)=>
    --    print("DEFINED (#{k})")
    --    rawset(@, k, v)
}, {
    __call: (t)=>
        _imports[t] = {}
        setmetatable(t, @)
        return t
})

_1_forked = (t)=>
    f = Importer(t or {})
    _imports[f] = assert _imports[@]
    import_to_1_from(f, @)
    return f

return {:Importer, :import_to_1_from, :_1_forked}
