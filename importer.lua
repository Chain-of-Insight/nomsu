local import_to_1_from
import_to_1_from = function(host, to_import)
  do
    local host_mt = getmetatable(host)
    if host_mt then
      if host_mt.__import then
        host_mt.__import(host, to_import)
        return 
      end
    end
  end
  for k, v in pairs(to_import) do
    host[k] = v
  end
end
local _imports = setmetatable({ }, {
  __mode = "k"
})
local Importer = setmetatable({
  __index = function(self, key)
    return _imports[self][key]
  end,
  __import = function(self, to_import)
    local imports = assert(_imports[self])
    for k, v in pairs(to_import) do
      local _continue_0 = false
      repeat
        imports[k] = v
        if v == to_import then
          _continue_0 = true
          break
        end
        local conflict = self[k]
        if type(conflict) == 'table' then
          import_to_1_from(conflict, v)
        end
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
  end
}, {
  __call = function(self, t)
    _imports[t] = { }
    setmetatable(t, self)
    return t
  end
})
local _1_forked
_1_forked = function(self)
  local f = Importer({ })
  _imports[f] = assert(_imports[self])
  import_to_1_from(f, self)
  return f
end
return {
  Importer = Importer,
  import_to_1_from = import_to_1_from,
  _1_forked = _1_forked
}
