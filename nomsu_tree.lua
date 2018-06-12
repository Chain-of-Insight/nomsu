local utils = require('utils')
local repr, stringify, min, max, equivalent, set, is_list, sum
repr, stringify, min, max, equivalent, set, is_list, sum = utils.repr, utils.stringify, utils.min, utils.max, utils.equivalent, utils.set, utils.is_list, utils.sum
local immutable = require('immutable')
local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local Lua, Nomsu, Source
do
  local _obj_0 = require("code_obj")
  Lua, Nomsu, Source = _obj_0.Lua, _obj_0.Nomsu, _obj_0.Source
end
local MAX_LINE = 80
local Types = { }
Types.is_node = function(n)
  return type(n) == 'userdata' and getmetatable(n) and Types[n.type] == getmetatable(n)
end
local Tree
Tree = function(name, fields, methods)
  methods = methods or { }
  local is_multi = true
  for _index_0 = 1, #fields do
    local f = fields[_index_0]
    is_multi = is_multi and (f ~= "value")
  end
  do
    methods.type = name
    methods.name = name
    methods.__new = methods.__new or function(self, source, ...)
      assert(source)
      if type(source) == 'string' then
        source = Source:from_string(source)
      end
      return source, ...
    end
    methods.is_multi = is_multi
    if is_multi then
      methods.__tostring = function(self)
        return tostring(self.name) .. "(" .. tostring(table.concat((function()
          local _accum_0 = { }
          local _len_0 = 1
          for _index_0 = 1, #self do
            local v = self[_index_0]
            _accum_0[_len_0] = repr(v)
            _len_0 = _len_0 + 1
          end
          return _accum_0
        end)(), ', ')) .. ")"
      end
      methods.map = function(self, fn)
        do
          local replacement = fn(self)
          if replacement then
            return replacement
          end
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
    else
      methods.__tostring = function(self)
        return tostring(self.name) .. "(" .. tostring(repr(self.value)) .. ")"
      end
      methods.map = function(self, fn)
        return fn(self) or self
      end
    end
  end
  Types[name] = immutable(fields, methods)
end
Tree("Block", {
  "source"
})
Tree("EscapedNomsu", {
  "source"
})
Tree("Text", {
  "source"
})
Tree("List", {
  "source"
})
Tree("Dict", {
  "source"
})
Tree("DictEntry", {
  "source"
})
Tree("IndexChain", {
  "source"
})
Tree("Number", {
  "source",
  "value"
})
Tree("Var", {
  "source",
  "value"
})
Tree("Action", {
  "source",
  "stub"
}, {
  __new = function(self, source, ...)
    assert(source)
    if type(source) == 'string' then
      source = Source:from_string(source)
    end
    local stub_bits = { }
    for i = 1, select("#", ...) do
      local a = select(i, ...)
      stub_bits[i] = type(a) == 'string' and a or "%"
    end
    local stub = concat(stub_bits, " ")
    return source, stub, ...
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
return Types
