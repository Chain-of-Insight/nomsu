local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local Source
Source = require("code_obj").Source
local unpack = unpack or table.unpack
local as_lua
as_lua = function(self)
  if type(self) == 'number' then
    return tostring(self)
  end
  do
    local mt = getmetatable(self)
    if mt then
      do
        local _as_lua = mt.as_lua
        if _as_lua then
          return _as_lua(self)
        end
      end
    end
  end
  if self.as_lua then
    return self:as_lua()
  end
  return error("Not supported: " .. tostring(self))
end
local SyntaxTree
do
  local _class_0
  local _base_0 = {
    __tostring = function(self)
      local bits
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #self do
          local b = self[_index_0]
          _accum_0[_len_0] = type(b) == 'string' and b:as_lua() or tostring(b)
          _len_0 = _len_0 + 1
        end
        bits = _accum_0
      end
      for k, v in pairs(self) do
        if not (bits[k] or k == 'type' or k == 'source') then
          table.insert(bits, tostring(k) .. "=" .. tostring(type(v) == 'string' and v:as_lua() or v))
        end
      end
      return tostring(self.type) .. "{" .. tostring(table.concat(bits, ", ")) .. "}"
    end,
    __eq = function(self, other)
      if type(self) ~= type(other) or #self ~= #other or getmetatable(self) ~= getmetatable(other) then
        return false
      end
      for i = 1, #self do
        if self[i] ~= other[i] then
          return false
        end
      end
      return true
    end,
    as_lua = function(self)
      local bits
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #self do
          local b = self[_index_0]
          _accum_0[_len_0] = as_lua(b)
          _len_0 = _len_0 + 1
        end
        bits = _accum_0
      end
      for k, v in pairs(self) do
        if not (bits[k]) then
          table.insert(bits, "[ " .. tostring(as_lua(k)) .. "]=" .. tostring(as_lua(v)))
        end
      end
      return "SyntaxTree{" .. tostring(table.concat(bits, ", ")) .. "}"
    end,
    get_source_file = function(self)
      return self.__class.source_code_for_tree[self]
    end,
    get_source_code = function(self)
      return self.__class.source_code_for_tree[self]:sub(self.source.start, self.source.stop)
    end,
    map = function(self, fn)
      local replacement = fn(self)
      if replacement == false then
        return nil
      end
      if replacement then
        if SyntaxTree:is_instance(replacement) then
          do
            local _tbl_0 = { }
            for k, v in pairs(replacement) do
              _tbl_0[k] = v
            end
            replacement = _tbl_0
          end
          replacement.source = self.source
          if self.comments then
            replacement.comments = {
              unpack(self.comments)
            }
          end
          replacement = SyntaxTree(replacement)
        end
      else
        replacement = {
          source = self.source,
          comments = self.comments and {
            unpack(self.comments)
          }
        }
        local changes = false
        for k, v in pairs(self) do
          local _continue_0 = false
          repeat
            replacement[k] = v
            if SyntaxTree:is_instance(v) then
              local r = v:map(fn)
              if r == v or r == nil then
                _continue_0 = true
                break
              end
              changes = true
              replacement[k] = r
            end
            _continue_0 = true
          until true
          if not _continue_0 then
            break
          end
        end
        if not (changes) then
          return self
        end
        replacement = SyntaxTree(replacement)
      end
      return replacement
    end,
    get_args = function(self)
      assert(self.type == "Action" or self.type == "MethodCall", "Only actions and method calls have arguments")
      local args = { }
      if self.type == "MethodCall" then
        assert(#self == 2, "Can't get arguments for multiple method calls at once.")
        args[1] = self[1]
        local _list_0 = self[2]
        for _index_0 = 1, #_list_0 do
          local tok = _list_0[_index_0]
          if type(tok) ~= 'string' then
            args[#args + 1] = tok
          end
        end
      else
        for _index_0 = 1, #self do
          local tok = self[_index_0]
          if type(tok) ~= 'string' then
            args[#args + 1] = tok
          end
        end
      end
      return args
    end,
    get_stub = function(self)
      if self.type == "MethodCall" then
        assert(#self == 2, "Can't get the stubs of multiple method calls at once.")
        return self[2]:get_stub()
      end
      local stub_bits = { }
      local arg_i = 1
      for _index_0 = 1, #self do
        local a = self[_index_0]
        if type(a) == 'string' then
          stub_bits[#stub_bits + 1] = a
        else
          stub_bits[#stub_bits + 1] = arg_i
          arg_i = arg_i + 1
        end
      end
      while type(stub_bits[#stub_bits]) == 'number' do
        stub_bits[#stub_bits] = nil
      end
      return concat(stub_bits, " ")
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function() end,
    __base = _base_0,
    __name = "SyntaxTree"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  local self = _class_0
  self.source_code_for_tree = setmetatable({ }, {
    __index = function(self, t)
      local s = t.source
      local Files = require('files')
      local f = Files.read(s.filename)
      return f
    end,
    __mode = "k"
  })
  self.is_instance = function(self, t)
    return type(t) == 'table' and getmetatable(t) == self.__base
  end
  SyntaxTree = _class_0
end
SyntaxTree.__base.__type = "Syntax Tree"
getmetatable(SyntaxTree).__call = function(self, t)
  if type(t.source) == 'string' then
    t.source = Source:from_string(t.source)
  end
  setmetatable(t, self.__base)
  if t.type == 'Action' then
    t.stub = t:get_stub()
  end
  return t
end
return SyntaxTree
