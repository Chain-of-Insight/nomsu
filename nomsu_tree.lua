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
Tree = function(name, leaf_or_branch, methods)
  local cls = methods or { }
  local is_multi = leaf_or_branch == 'branch'
  do
    cls.type = name
    cls.is_instance = function(self, x)
      return getmetatable(x) == self
    end
    cls.__index = cls
    cls.__tostring = function(self)
      return tostring(self.name) .. "(#{@value and repr(@value) or table.concat([repr(v) for v in *@]), ', '})"
    end
    cls.map = function(self, fn)
      do
        local replacement = fn(self)
        if replacement then
          return replacement
        end
      end
      if self.value then
        return self
      end
      local new_vals
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #self do
          local v = self[_index_0]
          _accum_0[_len_0] = v.map and v:map(fn) or v
          _len_0 = _len_0 + 1
        end
        new_vals = _accum_0
      end
      return getmetatable(self)(self.source, unpack(new_vals))
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
      assert(Source:is_instance(source))
      local inst
      if is_multi then
        inst = {
          source = source,
          ...
        }
      else
        inst = {
          source = source,
          value = ...
        }
      end
      setmetatable(inst, self)
      if inst.__init then
        inst:__init()
      end
      return inst
    end
  })
end
Tree("Number", 'leaf')
Tree("Var", 'leaf')
Tree("Block", 'branch')
Tree("EscapedNomsu", 'branch')
Tree("Text", 'branch')
Tree("List", 'branch')
Tree("Dict", 'branch')
Tree("DictEntry", 'branch')
Tree("IndexChain", 'branch')
Tree("Action", 'branch', {
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
        _accum_0[_len_0] = type(a) == "string" and a or "%" .. tostring(a.value)
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), " ")
  end
})
return AST
