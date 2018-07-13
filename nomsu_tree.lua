local repr
repr = require('utils').repr
local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local Source
Source = require("code_obj").Source
local unpack = unpack or table.unpack
local AST = { }
AST.is_syntax_tree = function(n, t)
  if t == nil then
    t = nil
  end
  return type(n) == 'table' and getmetatable(n) and AST[n.type] == getmetatable(n) and (t == nil or n.type == t)
end
local types = {
  "Number",
  "Var",
  "Block",
  "EscapedNomsu",
  "Text",
  "List",
  "Dict",
  "DictEntry",
  "IndexChain",
  "Action",
  "File",
  "Comment"
}
for _index_0 = 1, #types do
  local name = types[_index_0]
  local cls = { }
  do
    cls.__class = cls
    cls.__index = cls
    cls.__name = name
    cls.type = name
    cls.is_instance = function(self, x)
      return getmetatable(x) == self
    end
    cls.__tostring = function(self)
      return tostring(self.type) .. "(" .. tostring(repr(tostring(self.source))) .. ", " .. tostring(concat((function()
        local _accum_0 = { }
        local _len_0 = 1
        for _index_1 = 1, #self do
          local v = self[_index_1]
          _accum_0[_len_0] = repr(v)
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), ', ')) .. ")"
    end
    cls.map = function(self, fn)
      do
        local replacement = fn(self)
        if replacement then
          return replacement
        end
      end
      local replacements
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_1 = 1, #self do
          local v = self[_index_1]
          _accum_0[_len_0] = AST.is_syntax_tree(v) and v:map(fn) or nil
          _len_0 = _len_0 + 1
        end
        replacements = _accum_0
      end
      if not (next(replacements)) then
        return self
      end
      return (self.__class)(self.source, unpack((function()
        local _accum_0 = { }
        local _len_0 = 1
        for i, v in ipairs(self) do
          _accum_0[_len_0] = replacements[i] or v
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)()))
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
AST.Action.__init = function(self)
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
end
AST.Action.get_args = function(self)
  local _accum_0 = { }
  local _len_0 = 1
  for _index_0 = 1, #self do
    local tok = self[_index_0]
    if type(tok) ~= 'string' then
      _accum_0[_len_0] = tok
      _len_0 = _len_0 + 1
    end
  end
  return _accum_0
end
return AST
