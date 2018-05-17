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
  assert((kind == 'single') or (kind == 'multi'))
  local is_multi = (kind == 'multi')
  do
    methods.with_value = function(self, value)
      return getmetatable(self)(value)
    end
    methods.type = name
    methods.name = name
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
Tree("EscapedNomsu", 'single', {
  as_lua = function(self, nomsu)
    local make_tree
    make_tree = function(t)
      if type(t) ~= 'userdata' then
        return repr(t)
      end
      if t.is_multi then
        local bits
        do
          local _accum_0 = { }
          local _len_0 = 1
          for _index_0 = 1, #t do
            local bit = t[_index_0]
            _accum_0[_len_0] = make_tree(bit)
            _len_0 = _len_0 + 1
          end
          bits = _accum_0
        end
        return t.type .. "(" .. table.concat(bits, ", ") .. ")"
      else
        return t.type .. "(" .. make_tree(t.value) .. ")"
      end
    end
    return Lua.Value(nil, make_tree(self.value))
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    local nomsu = self.value:as_nomsu(true)
    if nomsu == nil and not inline then
      nomsu = self.value:as_nomsu()
      return nomsu and Nomsu(nil, "\\:\n    ", nomsu)
    end
    return nomsu and Nomsu(nil, "\\(", nomsu, ")")
  end,
  map = function(self, fn)
    return fn(self) or self:map(fn)
  end
})
Tree("Block", 'multi', {
  as_lua = function(self, nomsu)
    local lua = Lua()
    for i, line in ipairs(self) do
      local line_lua = line:as_lua(nomsu)
      if i > 1 then
        lua:append("\n")
      end
      lua:append(line_lua:as_statements())
    end
    return lua
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    if inline then
      local nomsu = Nomsu()
      for i, line in ipairs(self) do
        if i > 1 then
          nomsu:append("; ")
        end
        local line_nomsu = line:as_nomsu(true)
        if not (line_nomsu) then
          return nil
        end
        nomsu:append(line_nomsu)
      end
      return nomsu
    end
    local nomsu = Nomsu()
    for i, line in ipairs(self) do
      line = assert(line:as_nomsu(nil, true), "Could not convert line to nomsu")
      nomsu:append(line)
      if i < #self then
        nomsu:append("\n")
        if tostring(line):match("\n") then
          nomsu:append("\n")
        end
      end
    end
    return nomsu
  end
})
local math_expression = re.compile([[ ([+-] " ")* "%" (" " [*/^+-] (" " [+-])* " %")+ !. ]])
Tree("Action", 'multi', {
  as_lua = function(self, nomsu)
    local stub = self:get_stub()
    local compile_action = nomsu.environment.COMPILE_ACTIONS[stub]
    if compile_action then
      local args
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #self do
          local arg = self[_index_0]
          if arg.type ~= "Word" then
            _accum_0[_len_0] = arg
            _len_0 = _len_0 + 1
          end
        end
        args = _accum_0
      end
      do
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = nomsu.environment.ARG_ORDERS[compile_action][stub]
        for _index_0 = 1, #_list_0 do
          local p = _list_0[_index_0]
          _accum_0[_len_0] = args[p - 1]
          _len_0 = _len_0 + 1
        end
        args = _accum_0
      end
      local ret = compile_action(self, unpack(args))
      if not ret then
        error("Failed to produce any Lua")
      end
      return ret
    end
    local action = rawget(nomsu.environment.ACTIONS, stub)
    local lua = Lua.Value()
    if not action and math_expression:match(stub) then
      for i, tok in ipairs(self) do
        if tok.type == "Word" then
          lua:append(tok.value)
        else
          local tok_lua = tok:as_lua(nomsu)
          if not (tok_lua.is_value) then
            error("non-expression value inside math expression: " .. tostring(colored.yellow(repr(tok))))
          end
          if tok.type == "Action" then
            tok_lua:parenthesize()
          end
          lua:append(tok_lua)
        end
        if i < #self then
          lua:append(" ")
        end
      end
      return lua
    end
    local args = { }
    for i, tok in ipairs(self) do
      local _continue_0 = false
      repeat
        if tok.type == "Word" then
          _continue_0 = true
          break
        end
        local arg_lua = tok:as_lua(nomsu)
        if not (arg_lua.is_value) then
          error("Cannot use:\n" .. tostring(colored.yellow(repr(tok))) .. "\nas an argument to " .. tostring(stub) .. ", since it's not an expression, it produces: " .. tostring(repr(arg_lua)), 0)
        end
        insert(args, arg_lua)
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
    if action then
      do
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = nomsu.environment.ARG_ORDERS[action][stub]
        for _index_0 = 1, #_list_0 do
          local p = _list_0[_index_0]
          _accum_0[_len_0] = args[p]
          _len_0 = _len_0 + 1
        end
        args = _accum_0
      end
    end
    lua:append("ACTIONS[", repr(stub), "](")
    for i, arg in ipairs(args) do
      lua:append(arg)
      if i < #args then
        lua:append(", ")
      end
    end
    lua:append(")")
    return lua
  end,
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
  end,
  as_nomsu = function(self, inline, can_use_colon)
    if inline == nil then
      inline = false
    end
    if can_use_colon == nil then
      can_use_colon = false
    end
    if inline then
      local nomsu = Nomsu()
      for i, bit in ipairs(self) do
        if bit.type == "Word" then
          if i > 1 then
            nomsu:append(" ")
          end
          nomsu:append(bit.value)
        else
          local arg_nomsu = bit:as_nomsu(true)
          if not (arg_nomsu) then
            return nil
          end
          if not (i == 1) then
            nomsu:append(" ")
          end
          if bit.type == "Action" or bit.type == "Block" then
            arg_nomsu:parenthesize()
          end
          nomsu:append(arg_nomsu)
        end
      end
      return nomsu
    else
      local nomsu = Nomsu()
      local next_space = ""
      local last_colon = nil
      for i, bit in ipairs(self) do
        if bit.type == "Word" then
          nomsu:append(next_space, bit.value)
          next_space = " "
        else
          local arg_nomsu
          if last_colon == i - 1 and bit.type == "Action" then
            arg_nomsu = nil
          elseif bit.type == "Block" then
            arg_nomsu = nil
          else
            arg_nomsu = bit:as_nomsu(true)
          end
          if arg_nomsu and #arg_nomsu < MAX_LINE then
            if bit.type == "Action" then
              if can_use_colon and i > 1 then
                nomsu:append(next_space:match("[^ ]*"), ": ", arg_nomsu)
                next_space = "\n.."
                last_colon = i
              else
                nomsu:append(next_space, "(", arg_nomsu, ")")
                next_space = " "
              end
            else
              nomsu:append(next_space, arg_nomsu)
              next_space = " "
            end
          else
            arg_nomsu = bit:as_nomsu(nil, true)
            if not (nomsu) then
              return nil
            end
            if bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
              if i == 1 then
                arg_nomsu = Nomsu(nil, "(..)\n    ", arg_nomsu)
              else
                arg_nomsu = Nomsu(nil, "\n    ", arg_nomsu)
              end
            end
            if last_colon == i - 1 and (bit.type == "Action" or bit.type == "Block") then
              next_space = ""
            end
            nomsu:append(next_space, arg_nomsu)
            next_space = "\n.."
          end
          if next_space == " " and #(tostring(nomsu):match("[^\n]*$")) > MAX_LINE then
            next_space = "\n.."
          end
        end
      end
      return nomsu
    end
  end
})
Tree("Text", 'multi', {
  as_lua = function(self, nomsu)
    local lua = Lua.Value()
    local string_buffer = ""
    for _index_0 = 1, #self do
      local _continue_0 = false
      repeat
        local bit = self[_index_0]
        if type(bit) == "string" then
          string_buffer = string_buffer .. bit
          _continue_0 = true
          break
        end
        if string_buffer ~= "" then
          if #lua.bits > 0 then
            lua:append("..")
          end
          lua:append(repr(string_buffer))
          string_buffer = ""
        end
        local bit_lua = bit:as_lua(nomsu)
        if not (bit_lua.is_value) then
          error("Cannot use " .. tostring(colored.yellow(repr(bit))) .. " as a string interpolation value, since it's not an expression.", 0)
        end
        if #lua.bits > 0 then
          lua:append("..")
        end
        if bit.type ~= "Text" then
          bit_lua = Lua.Value(nil, "stringify(", bit_lua, ")")
        end
        lua:append(bit_lua)
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
    if string_buffer ~= "" or #lua.bits == 0 then
      if #lua.bits > 0 then
        lua:append("..")
      end
      lua:append(repr(string_buffer))
    end
    if #lua.bits > 1 then
      lua:parenthesize()
    end
    return lua
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    if inline then
      local nomsu = Nomsu(nil, '"')
      for _index_0 = 1, #self do
        local bit = self[_index_0]
        if type(bit) == 'string' then
          nomsu:append((bit:gsub("\\", "\\\\"):gsub("\n", "\\n")))
        else
          local interp_nomsu = bit:as_nomsu(true)
          if interp_nomsu then
            if bit.type ~= "Word" and bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
              interp_nomsu:parenthesize()
            end
            nomsu:append("\\", interp_nomsu)
          else
            return nil
          end
        end
      end
      nomsu:append('"')
      return nomsu
    else
      local inline_version = self:as_nomsu(true)
      if inline_version and #inline_version <= MAX_LINE then
        return inline_version
      end
      local nomsu = Nomsu(nil, '".."\n    ')
      for i, bit in ipairs(self) do
        if type(bit) == 'string' then
          nomsu:append((bit:gsub("\\", "\\\\"):gsub("\n", "\n    ")))
        else
          local interp_nomsu = bit:as_nomsu(true)
          if interp_nomsu then
            if bit.type ~= "Word" and bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
              interp_nomsu:parenthesize()
            end
            nomsu:append("\\", interp_nomsu)
          else
            interp_nomsu = bit:as_nomsu()
            if not (interp_nomsu) then
              return nil
            end
            nomsu:append("\\\n        ", interp_nomsu)
            if i < #self then
              nomsu:append("\n    ..")
            end
          end
        end
      end
      return nomsu
    end
  end
})
Tree("List", 'multi', {
  as_lua = function(self, nomsu)
    local lua = Lua.Value(nil, "{")
    local line_length = 0
    for i, item in ipairs(self) do
      local item_lua = item:as_lua(nomsu)
      if not (item_lua.is_value) then
        error("Cannot use " .. tostring(colored.yellow(repr(item))) .. " as a list item, since it's not an expression.", 0)
      end
      lua:append(item_lua)
      local item_string = tostring(item_lua)
      local last_line = item_string:match("[^\n]*$")
      if item_string:match("\n") then
        line_length = #last_line
      else
        line_length = line_length + #last_line
      end
      if i < #self then
        if line_length >= MAX_LINE then
          lua:append(",\n  ")
          line_length = 0
        else
          lua:append(", ")
          line_length = line_length + 2
        end
      end
    end
    lua:append("}")
    return lua
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    if inline then
      local nomsu = Nomsu(nil, "[")
      for i, item in ipairs(self) do
        local item_nomsu = item:as_nomsu(true)
        if not (item_nomsu) then
          return nil
        end
        if i > 1 then
          nomsu:append(", ")
        end
        nomsu:append(item_nomsu)
      end
      nomsu:append("]")
      return nomsu
    else
      local inline_version = self:as_nomsu(true)
      if inline_version and #inline_version <= MAX_LINE then
        return inline_version
      end
      local nomsu = Nomsu(nil, "[..]")
      local line = Nomsu(nil, "\n    ")
      for _index_0 = 1, #self do
        local item = self[_index_0]
        local item_nomsu = item:as_nomsu(true)
        if item_nomsu and #line + #", " + #item_nomsu <= MAX_LINE then
          if #line.bits > 1 then
            line:append(", ")
          end
          line:append(item_nomsu)
        else
          if not (item_nomsu) then
            item_nomsu = item:as_nomsu()
            if not (item_nomsu) then
              return nil
            end
          end
          if #line.bits > 1 then
            nomsu:append(line)
            line = Nomsu(nil, "\n    ")
          end
          line:append(item_nomsu)
        end
      end
      if #line.bits > 1 then
        nomsu:append(line)
      end
      return nomsu
    end
  end
})
Tree("Dict", 'multi', {
  as_lua = function(self, nomsu)
    local lua = Lua.Value(nil, "{")
    local line_length = 0
    for i, entry in ipairs(self) do
      local entry_lua = entry:as_lua(nomsu)
      lua:append(entry_lua)
      local entry_lua_str = tostring(entry_lua)
      local last_line = entry_lua_str:match("\n([^\n]*)$")
      if last_line then
        line_length = #last_line
      else
        line_length = line_length + #entry_lua_str
      end
      if i < #self then
        if line_length >= MAX_LINE then
          lua:append(",\n  ")
          line_length = 0
        else
          lua:append(", ")
          line_length = line_length + 2
        end
      end
    end
    lua:append("}")
    return lua
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    if inline then
      local nomsu = Nomsu(nil, "{")
      for i, entry in ipairs(self) do
        local entry_nomsu = entry:as_nomsu(true)
        if not (entry_nomsu) then
          return nil
        end
        if i > 1 then
          nomsu:append(", ")
        end
        nomsu:append(entry_nomsu)
      end
      nomsu:append("}")
      return nomsu
    else
      local inline_version = self:as_nomsu(true)
      if inline_version then
        return inline_version
      end
      local nomsu = Nomsu(nil, "{..}")
      local line = Nomsu(nil, "\n    ")
      for _index_0 = 1, #self do
        local entry = self[_index_0]
        local entry_nomsu = entry:as_nomsu()
        if not (entry_nomsu) then
          return nil
        end
        if #line + #tostring(entry_nomsu) <= MAX_LINE then
          if #line.bits > 1 then
            line:append(", ")
          end
          line:append(entry_nomsu)
        else
          if #line.bits > 1 then
            nomsu:append(line)
            line = Nomsu(nil, "\n    ")
          end
          line:append(entry_nomsu)
        end
      end
      if #line.bits > 1 then
        nomsu:append(line)
      end
      return nomsu
    end
  end
})
Tree("DictEntry", 'multi', {
  as_lua = function(self, nomsu)
    local key, value = self[1], self[2]
    local key_lua = key:as_lua(nomsu)
    if not (key_lua.is_value) then
      error("Cannot use " .. tostring(colored.yellow(repr(key))) .. " as a dict key, since it's not an expression.", 0)
    end
    local value_lua = value and value:as_lua(nomsu) or Lua.Value(nil, "true")
    if not (value_lua.is_value) then
      error("Cannot use " .. tostring(colored.yellow(repr(value))) .. " as a dict value, since it's not an expression.", 0)
    end
    local key_str = tostring(key_lua):match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
    if key_str then
      return Lua(nil, key_str, "=", value_lua)
    elseif tostring(key_lua):sub(1, 1) == "[" then
      return Lua(nil, "[ ", key_lua, "]=", value_lua)
    else
      return Lua(nil, "[", key_lua, "]=", value_lua)
    end
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = true
    end
    local key, value = self[1], self[2]
    local key_nomsu = key:as_nomsu(true)
    if not (key_nomsu) then
      return nil
    end
    if key.type == "Action" or key.type == "Block" then
      key_nomsu:parenthesize()
    end
    local value_nomsu
    if value then
      value_nomsu = value:as_nomsu(true)
    else
      value_nomsu = Nomsu(nil, "")
    end
    if inline and not value_nomsu then
      return nil
    end
    if not value_nomsu then
      if inline then
        return nil
      end
      value_nomsu = value:as_nomsu()
      if not (value_nomsu) then
        return nil
      end
    end
    return Nomsu(nil, key_nomsu, ":", value_nomsu)
  end
})
Tree("IndexChain", 'multi', {
  as_lua = function(self, nomsu)
    local lua = self[1]:as_lua(nomsu)
    if not (lua.is_value) then
      error("Cannot index " .. tostring(colored.yellow(repr(self[1]))) .. ", since it's not an expression.", 0)
    end
    local first_char = tostring(lua):sub(1, 1)
    if first_char == "{" or first_char == '"' or first_char == "[" then
      lua:parenthesize()
    end
    for i = 2, #self do
      local key = self[i]
      local key_lua = key:as_lua(nomsu)
      if not (key_lua.is_value) then
        error("Cannot use " .. tostring(colored.yellow(repr(key))) .. " as an index, since it's not an expression.", 0)
      end
      local key_lua_str = tostring(key_lua)
      do
        local lua_id = key_lua_str:match("^['\"]([a-zA-Z_][a-zA-Z0-9_]*)['\"]$")
        if lua_id then
          lua:append("." .. tostring(lua_id))
        elseif key_lua_str:sub(1, 1) == '[' then
          lua:append("[ ", key_lua, " ]")
        else
          lua:append("[", key_lua, "]")
        end
      end
    end
    return lua
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    local nomsu = Nomsu()
    for i, bit in ipairs(self) do
      if i > 1 then
        nomsu:append(".")
      end
      local bit_nomsu = bit:as_nomsu(true)
      if not (bit_nomsu) then
        return nil
      end
      if bit.type == "Action" or bit.type == "Block" then
        bit_nomsu:parenthesize()
      end
      nomsu:append(bit_nomsu)
    end
    return nomsu
  end
})
Tree("Number", 'single', {
  as_lua = function(self, nomsu)
    return Lua.Value(nil, tostring(self.value))
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    return Nomsu(nil, tostring(self.value))
  end
})
Tree("Var", 'single', {
  as_lua_id = function(v)
    return "_" .. (v:gsub("%W", function(c)
      if c == "_" then
        return "__"
      else
        return ("_%x"):format(c:byte())
      end
    end))
  end,
  as_lua = function(self, nomsu)
    return Lua.Value(nil, self.as_lua_id(self.value))
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    return Nomsu(nil, "%", self.value)
  end
})
Tree("Word", 'single', {
  as_lua = function(self, nomsu)
    return error("Attempt to convert Word to lua")
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    return Nomsu(nil, self.value)
  end
})
Tree("Comment", 'single', {
  as_lua = function(self, nomsu)
    return Lua(nil, "--" .. self.value:gsub("\n", "\n--") .. "\n")
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    if inline then
      return nil
    end
    if self.value:match("\n") then
      return Nomsu(nil, "#..", self.value:gsub("\n", "\n    "))
    else
      return Nomsu(nil, "#", self.value)
    end
  end
})
return Types
