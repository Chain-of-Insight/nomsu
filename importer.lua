local import_to_1_from
import_to_1_from = function(host, to_import, prefix)
  if prefix == nil then
    prefix = nil
  end
  do
    local host_mt = getmetatable(host)
    if host_mt then
      if host_mt.__import then
        host_mt.__import(host, to_import, prefix)
        return 
      end
    end
  end
  for k, v in pairs(to_import) do
    if k == to_import then
      k = host
    end
    if v == to_import then
      v = host
    end
    if prefix and type(k) == 'string' then
      k = prefix .. k
    end
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
  __import = function(self, to_import, prefix)
    if prefix == nil then
      prefix = nil
    end
    local imports = assert(_imports[self])
    for k, v in pairs(to_import) do
      local _continue_0 = false
      repeat
        if prefix and type(k) == 'string' then
          k = prefix .. k
        end
        imports[k] = v
        if v == to_import then
          _continue_0 = true
          break
        end
        local conflict = self[k]
        do
          local conflict_mt = getmetatable(host)
          if conflict_mt then
            if conflict_mt.__import then
              conflict_mt.__import(conflict, v, prefix)
            end
          end
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
_1_forked = function(self, t)
  local f = Importer(t or { })
  _imports[f] = assert(_imports[self])
  import_to_1_from(f, self)
  return f
end
return {
  Importer = Importer,
  import_to_1_from = import_to_1_from,
  _1_forked = _1_forked
}
