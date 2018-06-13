local repr
repr = require('utils').repr
local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local Source
Source = require("code_obj").Source
local AST = { }
AST.is_syntax_tree = function(n)
  return type(n) == 'table' and getmetatable(n) and AST[n.type] == getmetatable(n)
end
local Tree
Tree = function(name, methods)
  local cls = methods or { }
  do
    cls.type = name
    cls.__class = cls
    cls.__name = name
    cls.is_instance = function(self, x)
      return getmetatable(x) == self
    end
    cls.__index = cls
    cls.__tostring = function(self)
      return tostring(self.name) .. "(#{table.concat([repr(v) for v in *@]), ', '})"
    end
    cls.map = function(self, fn)
      do
        local replacement = fn(self)
        if replacement then
          return replacement
        end
      end
      local made_changes, new_vals = false, { }
      for i, v in ipairs(self) do
        if AST.is_syntax_tree(v) then
          do
            local replacement = v:map(fn)
            if replacement then
              if replacement ~= v then
                made_changes = true
                v = replacement
              end
            end
          end
        end
        new_vals[i] = v
      end
      if not (made_changes) then
        return self
      end
      local replacement = getmetatable(self)(self.source, unpack(new_vals))
      return replacement
    end
  end
  AST[name] = setmetatable(cls, {
    __tostring = function(self)
      return self.name
    end,
    __call = function(self, source, ...)
      if type(source) == 'string' then
        source = Source:from_string(source)
      end
      for i = 1, select('#', ...) do
        assert(select(i, ...))
      end
      assert(Source:is_instance(source))
      local inst = {
        source = source,
        ...
      }
      setmetatable(inst, self)
      if inst.__init then
        inst:__init()
      end
      return inst
    end
  })
end
Tree("Number")
Tree("Var")
Tree("Block")
Tree("EscapedNomsu")
Tree("Text")
Tree("List")
Tree("Dict")
Tree("DictEntry")
Tree("IndexChain")
Tree("Action", {
  __init = function(self)
    local stub_bits
    do
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #self do
        local a = self[_index_0]
        _accum_0[_len_0] = type(a) == 'string' and a or '%'
        _len_0 = _len_0 + 1
      end
      stub_bits = _accum_0
    end
    self.stub = concat(stub_bits, " ")
  end,
  get_spec = function(self)
    return concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #self do
        local a = self[_index_0]
        _accum_0[_len_0] = type(a) == "string" and a or "%" .. tostring(a[1])
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), " ")
  end
})
return AST
