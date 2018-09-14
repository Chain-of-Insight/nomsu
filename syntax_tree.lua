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
  "FileChunks",
  "Error",
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
      return tostring(self.type) .. tostring(repr(self, (function(x)
        return Source:is_instance(x) and repr(tostring(x)) or nil
      end)))
    end
    cls.__repr = function(self)
      return tostring(self.type) .. tostring(repr(self, (function(x)
        return Source:is_instance(x) and repr(tostring(x)) or nil
      end)))
    end
    cls.source_code_for_tree = { }
    cls.get_source_code = function(self)
      return self.source_code_for_tree[self]
    end
    cls.map = function(self, fn)
      local replacement = fn(self)
      if replacement == false then
        return nil
      end
      if replacement then
        if AST.is_syntax_tree(replacement) then
          replacement = setmetatable((function()
            local _tbl_0 = { }
            for k, v in pairs(replacement) do
              _tbl_0[k] = v
            end
            return _tbl_0
          end)(), getmetatable(replacement))
          replacement.source = self.source
          if self.comments then
            replacement.comments = {
              unpack(self.comments)
            }
          end
          do
            local init = replacement.__init
            if init then
              init(replacement)
            end
          end
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
            if AST.is_syntax_tree(v) then
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
        replacement = setmetatable(replacement, getmetatable(self))
        do
          local init = replacement.__init
          if init then
            init(replacement)
          end
        end
      end
      return replacement
    end
    cls.__eq = function(self, other)
      if type(self) ~= type(other) or #self ~= #other or getmetatable(self) ~= getmetatable(other) then
        return false
      end
      for i = 1, #self do
        if self[i] ~= other[i] then
          return false
        end
      end
      if self.target ~= other.target then
        return false
      end
      return true
    end
  end
  AST[name] = setmetatable(cls, {
    __tostring = function(self)
      return self.__name
    end,
    __call = function(self, t)
      if type(t.source) == 'string' then
        t.source = Source:from_string(t.source)
      else
        assert(Source:is_instance(t.source))
      end
      setmetatable(t, self)
      do
        local init = t.__init
        if init then
          init(t)
        end
      end
      return t
    end
  })
end
AST.Action.__init = function(self)
  local stub_bits = { }
  local arg_i = 1
  for _index_0 = 1, #self do
    local a = self[_index_0]
    if type(a) == 'string' then
      stub_bits[#stub_bits + 1] = a
    else
      stub_bits[#stub_bits + 1] = tostring(arg_i)
      arg_i = arg_i + 1
    end
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
