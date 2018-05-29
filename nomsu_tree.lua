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
Tree = function(name, kind, methods)
  methods = methods or { }
  assert((kind == 'single') or (kind == 'multi'))
  local is_multi = (kind == 'multi')
  do
    methods.type = name
    methods.name = name
    methods.__new = function(self, value, source)
      assert(source)
      if type(source) == 'string' then
        source = Source:from_string(source)
      end
      return value, source
    end
    methods.is_multi = is_multi
    methods.map = function(self, fn)
      if type(fn) == 'table' then
        if not (next(fn)) then
          return self
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
          local _list_0 = self.value
          for _index_0 = 1, #_list_0 do
            local v = _list_0[_index_0]
            _accum_0[_len_0] = repr(v)
            _len_0 = _len_0 + 1
          end
          return _accum_0
        end)(), ', ')) .. ")"
      end
      methods._map = function(self, fn)
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
          local _list_0 = self.value
          for _index_0 = 1, #_list_0 do
            local v = _list_0[_index_0]
            _accum_0[_len_0] = v._map and v:_map(fn) or v
            _len_0 = _len_0 + 1
          end
          new_vals = _accum_0
        end
        local ret = getmetatable(self)(Tuple(unpack(new_vals)), self.source)
        return ret
      end
      methods.__ipairs = function(self)
        return error()
      end
    else
      methods.__tostring = function(self)
        return tostring(self.name) .. "(" .. tostring(repr(self.value)) .. ")"
      end
      methods._map = function(self, fn)
        return fn(self) or self
      end
    end
  end
  Types[name] = immutable({
    "value",
    "source"
  }, methods)
end
Tree("Block", 'multi')
Tree("EscapedNomsu", 'multi')
Tree("Text", 'multi')
Tree("List", 'multi')
Tree("Dict", 'multi')
Tree("DictEntry", 'multi')
Tree("IndexChain", 'multi')
Tree("Number", 'single')
Tree("Comment", 'single')
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
    if include_names then
      return concat((function()
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = self.value
        for _index_0 = 1, #_list_0 do
          local a = _list_0[_index_0]
          _accum_0[_len_0] = type(a) == "string" and a or "%" .. tostring(a.value)
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), " ")
    else
      return concat((function()
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = self.value
        for _index_0 = 1, #_list_0 do
          local a = _list_0[_index_0]
          _accum_0[_len_0] = type(a) == "string" and a or "%"
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), " ")
    end
  end
})
return Types
