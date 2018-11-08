-- This file defines Importer, which is a type of table that can import from other tables

import_to_1_from = (host, to_import)->
    if host_mt = getmetatable(host)
        if host_mt.__import
            host_mt.__import(host, to_import)
            return
    for k,v in pairs(to_import)
        host[k] = v
_imports = setmetatable({}, {__mode:"k"})
Importer = setmetatable({
    __index: (key)=> _imports[@][key]
    __import: (to_import)=>
        imports = assert _imports[@]
        for k,v in pairs(to_import)
            imports[k] = v
            continue if v == to_import
            conflict = @[k]
            import_to_1_from(conflict, v) if type(conflict) == 'table'
}, {
    __call: (t)=>
        _imports[t] = {}
        setmetatable(t, @)
        return t
})

_1_forked = =>
    f = Importer{}
    _imports[f] = assert _imports[@]
    import_to_1_from(f, @)
    return f

return {:Importer, :import_to_1_from, :_1_forked}
