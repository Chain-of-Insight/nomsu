local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local Source
Source = require("code_obj").Source
local List, Dict
do
  local _obj_0 = require('containers')
  List, Dict = _obj_0.List, _obj_0.Dict
end
local Files = require('files')
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
      if self.type ~= other.type then
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
      return self.__class.source_code_for_tree[self]:sub(self.source.start, self.source.stop - 1)
    end,
    add = function(self, ...)
      local n = #self
      for i = 1, select('#', ...) do
        self[n + i] = select(i, ...)
      end
      self.stub = nil
    end,
    with = function(self, fn)
      if type(fn) == 'table' then
        local replacements = fn
        fn = function(t)
          if t.type == "Var" then
            do
              local r = replacements[t:as_var()]
              if r then
                return r
              end
            end
          end
        end
      end
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
              local r = v:with(fn)
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
    contains = function(self, subtree)
      if subtree == self then
        return true
      end
      for k, v in pairs(self) do
        if SyntaxTree:is_instance(v) then
          if v:contains(subtree) then
            return true
          end
        end
      end
      return false
    end,
    get_args = function(self)
      assert(self.type == "Action" or self.type == "MethodCall", "Only actions and method calls have arguments")
      local args = { }
      if self.type == "MethodCall" then
        args[1] = self[1]
        for i = 2, #self do
          local _list_0 = self[i]
          for _index_0 = 1, #_list_0 do
            local tok = _list_0[_index_0]
            if type(tok) ~= 'string' then
              args[#args + 1] = tok
            end
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
      local _exp_0 = self.type
      if "Action" == _exp_0 then
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
      elseif "MethodCall" == _exp_0 then
        return "0, " .. table.concat((function()
          local _accum_0 = { }
          local _len_0 = 1
          for i = 2, #self do
            _accum_0[_len_0] = self[i]:get_stub()
            _len_0 = _len_0 + 1
          end
          return _accum_0
        end)(), "; ")
      else
        return error(tostring(self.type) .. "s do not have stubs")
      end
    end,
    as_var = function(self)
      assert(self.type == "Var")
      if type(self[1]) == 'string' then
        return self[1]
      else
        return self[1]:get_stub()
      end
    end,
    matching = function(self, patt)
      if patt.type == "Var" then
        return {
          [patt:as_var()] = self
        }
      end
      if patt.type ~= self.type then
        return nil
      end
      if patt.type == "Action" and patt:get_stub() ~= self:get_stub() then
        return nil
      end
      if #self ~= #patt then
        return nil
      end
      local match = { }
      for i = 1, #self do
        local v = self[i]
        local pv = patt[i]
        if type(v) ~= type(pv) then
          return nil
        end
        if type(v) ~= 'table' then
          if not (v == pv) then
            return nil
          end
        else
          local m = v:matching(pv)
          if not (m) then
            return nil
          end
          for mk, mv in pairs(m) do
            if match[mk] and match[mk] ~= mv then
              return nil
            end
            match[mk] = mv
          end
        end
      end
      return Dict(match)
    end,
    _breadth_first = function(self)
      coroutine.yield(self)
      for _index_0 = 1, #self do
        local child = self[_index_0]
        if getmetatable(child) == SyntaxTree.__base then
          child:_breadth_first()
        end
      end
    end,
    breadth_first = function(self)
      return coroutine.create(function()
        return self:_breadth_first()
      end)
    end,
    _depth_first = function(self)
      coroutine.yield(self)
      for _index_0 = 1, #self do
        local child = self[_index_0]
        if getmetatable(child) == SyntaxTree.__base then
          child:_depth_first()
        end
      end
    end,
    depth_first = function(self)
      return coroutine.create(function()
        return self:_depth_first()
      end)
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
SyntaxTree.__base.__type = "a Syntax Tree"
getmetatable(SyntaxTree).__call = function(self, t, ...)
  if type(t.source) == 'string' then
    t.source = Source:from_string(t.source)
  end
  setmetatable(t, self.__base)
  for i = 1, select("#", ...) do
    t[i] = select(i, ...)
  end
  if t.type == 'Action' then
    t.stub = t:get_stub()
  end
  return t
end
return SyntaxTree
