local utils = require('utils')
local repr, stringify, min, max, equivalent, set, is_list, sum
repr, stringify, min, max, equivalent, set, is_list, sum = utils.repr, utils.stringify, utils.min, utils.max, utils.equivalent, utils.set, utils.is_list, utils.sum
local immutable = require('immutable')
local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local Lua, Nomsu, Location
do
  local _obj_0 = require("code_obj")
  Lua, Nomsu, Location = _obj_0.Lua, _obj_0.Nomsu, _obj_0.Location
end
local MAX_LINE = 80
local Types = { }
Types.is_node = function(n)
  return type(n) == 'userdata' and getmetatable(n) and Types[n.type] == getmetatable(n)
end
local Tree
Tree = function(name, kind, methods)
  methods = methods or { }
  assert((kind == 'single') or (kind == 'multi'))
  local is_multi = (kind == 'multi')
  do
    methods.with_value = function(self, value)
      return getmetatable(self)(value)
    end
    methods.type = name
    methods.name = name
    methods.is_multi = is_multi
    methods.map = function(self, fn)
      if type(fn) == 'table' then
        if not (next(fn)) then
          return self
        end
        if type(next(fn)) == 'string' then
          error("SHIT")
        end
        local _replacements = fn
        fn = function(k)
          return _replacements[k]
        end
      end
      return self:_map(fn)
    end
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
          local ret = fn(self)
          if ret then
            return ret
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
        local ret = getmetatable(self)(unpack(new_vals))
        return ret
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
  if is_multi then
    Types[name] = immutable(nil, methods)
  else
    Types[name] = immutable({
      "value"
    }, methods)
  end
end
Tree("Block", 'multi')
Tree("Text", 'multi')
Tree("List", 'multi')
Tree("Dict", 'multi')
Tree("DictEntry", 'multi')
Tree("IndexChain", 'multi')
Tree("Number", 'single')
Tree("Word", 'single')
Tree("Comment", 'single')
Tree("EscapedNomsu", 'single', {
  map = function(self, fn)
    return fn(self) or self:map(fn)
  end
})
Tree("Var", 'single', {
  as_lua_id = function(self)
    return "_" .. (self.value:gsub("%W", function(c)
      if c == "_" then
        return "__"
      else
        return ("_%x"):format(c:byte())
      end
    end))
  end
})
Tree("Action", 'multi', {
  get_stub = function(self, include_names)
    if include_names == nil then
      include_names = false
    end
    local bits
    if include_names then
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #self do
          local t = self[_index_0]
          _accum_0[_len_0] = (t.type == "Word" and t.value or "%" .. tostring(t.value))
          _len_0 = _len_0 + 1
        end
        bits = _accum_0
      end
    else
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #self do
          local t = self[_index_0]
          _accum_0[_len_0] = (t.type == "Word" and t.value or "%")
          _len_0 = _len_0 + 1
        end
        bits = _accum_0
      end
    end
    return concat(bits, " ")
  end
})
return Types
