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
Types.DictEntry = immutable({
  "key",
  "value"
}, {
  name = "DictEntry"
})
Types.is_node = function(n)
  return type(n) == 'userdata' and getmetatable(n) and Types[n.type] == getmetatable(n)
end
local Tree
Tree = function(name, methods)
  do
    methods.__tostring = function(self)
      return tostring(self.name) .. "(" .. tostring(repr(self.value)) .. ", " .. tostring(repr(self.source)) .. ")"
    end
    methods.with_value = function(self, value)
      return getmetatable(self)(value, self.source)
    end
    methods.type = name
    methods.name = name
    methods.original_nomsu = function(self)
      local leading_space = 0
      local src_file = FILE_CACHE[self.source.filename]
      while src_file:sub(self.source.start - leading_space - 1, self.source.start - leading_space - 1) == " " do
        leading_space = leading_space + 1
      end
      if src_file:sub(self.source.start - leading_space - 1, self.source.start - leading_space - 1) ~= "\n" then
        leading_space = 0
      end
      local ret = tostring(self.source:get_text()):gsub("\n" .. ((" "):rep(leading_space)), "\n")
      return ret
    end
  end
  Types[name] = immutable({
    "value",
    "source"
  }, methods)
end
Tree("Nomsu", {
  as_lua = function(self, nomsu)
    return Lua.Value(self.source, "nomsu:parse(Nomsu(", repr(self.value.source), ", ", repr(tostring(self.value:as_nomsu())), "))")
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    local nomsu = self.value:as_nomsu(true)
    if nomsu == nil and not inline then
      nomsu = self.value:as_nomsu()
      return nomsu and Nomsu(self.source, "\\:\n    ", nomsu)
    end
    return nomsu and Nomsu(self.source, "\\(", nomsu, ")")
  end,
  map = function(self, fn)
    return fn(self) or self:with_value(self.value:map(fn))
  end
})
Tree("Block", {
  as_lua = function(self, nomsu)
    local lua = Lua(self.source)
    for i, line in ipairs(self.value) do
      local line_lua = line:as_lua(nomsu)
      if i > 1 then
        lua:append("\n")
      end
      line_lua:convert_to_statements()
      lua:append(line_lua)
    end
    return lua
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    if inline then
      local nomsu = Nomsu(self.source)
      for i, line in ipairs(self.value) do
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
    local nomsu = Nomsu(self.source)
    for i, line in ipairs(self.value) do
      line = assert(line:as_nomsu(nil, true), "Could not convert line to nomsu")
      nomsu:append(line)
      if i < #self.value then
        nomsu:append("\n")
        if tostring(line):match("\n") then
          nomsu:append("\n")
        end
      end
    end
    return nomsu
  end,
  map = function(self, fn)
    return fn(self) or self:with_value(Tuple(unpack((function()
      local _accum_0 = { }
      local _len_0 = 1
      local _list_0 = self.value
      for _index_0 = 1, #_list_0 do
        local v = _list_0[_index_0]
        _accum_0[_len_0] = v:map(fn)
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())))
  end
})
local math_expression = re.compile([[ ([+-] " ")* "%" (" " [*/^+-] (" " [+-])* " %")+ !. ]])
Tree("Action", {
  as_lua = function(self, nomsu)
    local stub = self:get_stub()
    local compile_action = nomsu.environment.COMPILE_ACTIONS[stub]
    if compile_action then
      local args
      do
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = self.value
        for _index_0 = 1, #_list_0 do
          local arg = _list_0[_index_0]
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
    local lua = Lua.Value(self.source)
    if not action and math_expression:match(stub) then
      for i, tok in ipairs(self.value) do
        if tok.type == "Word" then
          lua:append(tok.value)
        else
          local tok_lua = tok:as_lua(nomsu)
          if not (tok_lua.is_value) then
            local src = tok.source:get_text()
            error("non-expression value inside math expression: " .. tostring(colored.yellow(src)))
          end
          if tok.type == "Action" then
            tok_lua:parenthesize()
          end
          lua:append(tok_lua)
        end
        if i < #self.value then
          lua:append(" ")
        end
      end
      return lua
    end
    local args = { }
    for i, tok in ipairs(self.value) do
      local _continue_0 = false
      repeat
        if tok.type == "Word" then
          _continue_0 = true
          break
        end
        local arg_lua = tok:as_lua(nomsu)
        if not (arg_lua.is_value) then
          local line, src = tok.source:get_line(), tok.source:get_text()
          error(tostring(line) .. ": Cannot use:\n" .. tostring(colored.yellow(src)) .. "\nas an argument to " .. tostring(stub) .. ", since it's not an expression, it produces: " .. tostring(repr(arg_lua)), 0)
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
        local _list_0 = self.value
        for _index_0 = 1, #_list_0 do
          local t = _list_0[_index_0]
          _accum_0[_len_0] = (t.type == "Word" and t.value or "%" .. tostring(t.value))
          _len_0 = _len_0 + 1
        end
        bits = _accum_0
      end
    else
      do
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = self.value
        for _index_0 = 1, #_list_0 do
          local t = _list_0[_index_0]
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
      local nomsu = Nomsu(self.source)
      for i, bit in ipairs(self.value) do
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
      local nomsu = Nomsu(self.source)
      local next_space = ""
      local last_colon = nil
      for i, bit in ipairs(self.value) do
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
                arg_nomsu = Nomsu(bit.source, "(..)\n    ", arg_nomsu)
              else
                arg_nomsu = Nomsu(bit.source, "\n    ", arg_nomsu)
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
  end,
  map = function(self, fn)
    return fn(self) or self:with_value(Tuple(unpack((function()
      local _accum_0 = { }
      local _len_0 = 1
      local _list_0 = self.value
      for _index_0 = 1, #_list_0 do
        local v = _list_0[_index_0]
        _accum_0[_len_0] = v:map(fn)
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())))
  end
})
Tree("Text", {
  as_lua = function(self, nomsu)
    local lua = Lua.Value(self.source)
    local string_buffer = ""
    local _list_0 = self.value
    for _index_0 = 1, #_list_0 do
      local _continue_0 = false
      repeat
        local bit = _list_0[_index_0]
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
          local line, src = bit.source:get_line(), bit.source:get_text()
          error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(bit)) .. " as a string interpolation value, since it's not an expression.", 0)
        end
        if #lua.bits > 0 then
          lua:append("..")
        end
        if bit.type ~= "Text" then
          bit_lua = Lua.Value(bit.source, "stringify(", bit_lua, ")")
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
      local nomsu = Nomsu(self.source, '"')
      local _list_0 = self.value
      for _index_0 = 1, #_list_0 do
        local bit = _list_0[_index_0]
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
      local nomsu = Nomsu(self.source, '".."\n    ')
      for i, bit in ipairs(self.value) do
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
            if i < #self.value then
              nomsu:append("\n    ..")
            end
          end
        end
      end
      return nomsu
    end
  end,
  map = function(self, fn)
    return fn(self) or self:with_value(Tuple(unpack((function()
      local _accum_0 = { }
      local _len_0 = 1
      local _list_0 = self.value
      for _index_0 = 1, #_list_0 do
        local v = _list_0[_index_0]
        _accum_0[_len_0] = type(v) == 'string' and v or v:map(fn)
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())))
  end
})
Tree("List", {
  as_lua = function(self, nomsu)
    local lua = Lua.Value(self.source, "{")
    local line_length = 0
    for i, item in ipairs(self.value) do
      local item_lua = item:as_lua(nomsu)
      if not (item_lua.is_value) then
        local line, src = item.source:get_line(), item.source:get_text()
        error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as a list item, since it's not an expression.", 0)
      end
      lua:append(item_lua)
      local item_string = tostring(item_lua)
      local last_line = item_string:match("[^\n]*$")
      if item_string:match("\n") then
        line_length = #last_line
      else
        line_length = line_length + #last_line
      end
      if i < #self.value then
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
      local nomsu = Nomsu(self.source, "[")
      for i, item in ipairs(self.value) do
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
      local nomsu = Nomsu(self.source, "[..]")
      local line = Nomsu(self.source, "\n    ")
      local _list_0 = self.value
      for _index_0 = 1, #_list_0 do
        local item = _list_0[_index_0]
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
            line = Nomsu(item.source, "\n    ")
          end
          line:append(item_nomsu)
        end
      end
      if #line.bits > 1 then
        nomsu:append(line)
      end
      return nomsu
    end
  end,
  map = function(self, fn)
    return fn(self) or self:with_value(Tuple(unpack((function()
      local _accum_0 = { }
      local _len_0 = 1
      local _list_0 = self.value
      for _index_0 = 1, #_list_0 do
        local v = _list_0[_index_0]
        _accum_0[_len_0] = v:map(fn)
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())))
  end
})
Tree("Dict", {
  as_lua = function(self, nomsu)
    local lua = Lua.Value(self.source, "{")
    local line_length = 0
    for i, entry in ipairs(self.value) do
      local key_lua = entry.key:as_lua(nomsu)
      if not (key_lua.is_value) then
        local line, src = key.source:get_line(), key.source:get_text()
        error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as a dict key, since it's not an expression.", 0)
      end
      local value_lua = entry.value and entry.value:as_lua(nomsu) or Lua.Value(entry.key.source, "true")
      if not (value_lua.is_value) then
        local line, src = value.source:get_line(), value.source:get_text()
        error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as a dict value, since it's not an expression.", 0)
      end
      local key_str = tostring(key_lua):match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
      if key_str then
        lua:append(key_str, "=", value_lua)
      elseif tostring(key_lua):sub(1, 1) == "[" then
        lua:append("[ ", key_lua, "]=", value_lua)
      else
        lua:append("[", key_lua, "]=", value_lua)
      end
      local newlines, last_line = ("[" .. tostring(key_lua) .. "=" .. tostring(value_lua)):match("^(.-)([^\n]*)$")
      if #newlines > 0 then
        line_length = #last_line
      else
        line_length = line_length + #last_line
      end
      if i < #self.value then
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
      local nomsu = Nomsu(self.source, "{")
      for i, entry in ipairs(self.value) do
        local key_nomsu = entry.key:as_nomsu(true)
        if not (key_nomsu) then
          return nil
        end
        if entry.key.type == "Action" or entry.key.type == "Block" then
          key_nomsu:parenthesize()
        end
        local value_nomsu = entry.value and entry.value:as_nomsu(true) or Nomsu(entry.key.source, "")
        if not (value_nomsu) then
          return nil
        end
        if i > 1 then
          nomsu:append(", ")
        end
        nomsu:append(key_nomsu, ":", value_nomsu)
      end
      nomsu:append("}")
      return nomsu
    else
      local inline_version = self:as_nomsu(true)
      if inline_version then
        return inline_version
      end
      local nomsu = Nomsu(self.source, "{..}")
      local line = Nomsu(self.source, "\n    ")
      local _list_0 = self.value
      for _index_0 = 1, #_list_0 do
        local entry = _list_0[_index_0]
        local key_nomsu = entry.key:as_nomsu(true)
        if not (key_nomsu) then
          return nil
        end
        if entry.key.type == "Action" or entry.key.type == "Block" then
          key_nomsu:parenthesize()
        end
        local value_nomsu = entry.value and entry.value:as_nomsu(true) or Nomsu(entry.key.source, "")
        if value_nomsu and #line + #", " + #key_nomsu + #":" + #value_nomsu <= MAX_LINE then
          if #line.bits > 1 then
            line:append(", ")
          end
          line:append(key_nomsu)
          if entry.value then
            line:append(":", value_nomsu)
          end
        else
          if not (value_nomsu) then
            value_nomsu = entry.value:as_nomsu()
            if not (value_nomsu) then
              return nil
            end
          end
          if #line.bits > 1 then
            nomsu:append(line)
            line = Nomsu(bit.source, "\n    ")
          end
          line:append(key_nomsu)
          if entry.value then
            line:append(":", value_nomsu)
          end
        end
      end
      if #line.bits > 1 then
        nomsu:append(line)
      end
      return nomsu
    end
  end,
  map = function(self, fn)
    local DictEntry = Types.DictEntry
    return fn(self) or self:with_value(Tuple(unpack((function()
      local _accum_0 = { }
      local _len_0 = 1
      local _list_0 = self.value
      for _index_0 = 1, #_list_0 do
        local e = _list_0[_index_0]
        _accum_0[_len_0] = DictEntry(e.key:map(fn), e.value:map(fn))
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())))
  end
})
Tree("IndexChain", {
  as_lua = function(self, nomsu)
    local lua = self.value[1]:as_lua(nomsu)
    if not (lua.is_value) then
      local line, src = self.value[1].source:get_line(), self.value[1].source:get_text()
      error(tostring(line) .. ": Cannot index " .. tostring(colored.yellow(src)) .. ", since it's not an expression.", 0)
    end
    local first_char = tostring(lua):sub(1, 1)
    if first_char == "{" or first_char == '"' or first_char == "[" then
      lua:parenthesize()
    end
    for i = 2, #self.value do
      local _continue_0 = false
      repeat
        local key = self.value[i]
        if key.type == 'Text' and #key.value == 1 and type(key.value[1]) == 'string' and key.value[1]:match("^[a-zA-Z_][a-zA-Z0-9_]*$") then
          lua:append("." .. tostring(key.value[1]))
          _continue_0 = true
          break
        end
        local key_lua = key:as_lua(nomsu)
        if not (key_lua.is_value) then
          local line, src = key.source:get_line(), key.source:get_text()
          error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as an index, since it's not an expression.", 0)
        end
        if tostring(key_lua):sub(1, 1) == '[' then
          lua:append("[ ", key_lua, "]")
        else
          lua:append("[", key_lua, "]")
        end
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
    return lua
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    local nomsu = Nomsu(self.source)
    for i, bit in ipairs(self.value) do
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
  end,
  map = function(self, fn)
    return fn(self) or self:with_value(Tuple(unpack((function()
      local _accum_0 = { }
      local _len_0 = 1
      local _list_0 = self.value
      for _index_0 = 1, #_list_0 do
        local v = _list_0[_index_0]
        _accum_0[_len_0] = v:map(fn)
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())))
  end
})
Tree("Number", {
  as_lua = function(self, nomsu)
    return Lua.Value(self.source, tostring(self.value))
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    return Nomsu(self.source, tostring(self.value))
  end,
  map = function(self, fn)
    return fn(self) or self
  end
})
Tree("Var", {
  as_lua = function(self, nomsu)
    local lua_id = "_" .. (self.value:gsub("%W", function(verboten)
      if verboten == "_" then
        return "__"
      else
        return ("_%x"):format(verboten:byte())
      end
    end))
    return Lua.Value(self.source, lua_id)
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    return Nomsu(self.source, "%", self.value)
  end,
  map = function(self, fn)
    return fn(self) or self
  end
})
Tree("Word", {
  as_lua = function(self, nomsu)
    return error("Attempt to convert Word to lua")
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    return Nomsu(self.source, self.value)
  end,
  map = function(self, fn)
    return fn(self) or self
  end
})
Tree("Comment", {
  as_lua = function(self, nomsu)
    return Lua(self.source, "--" .. self.value:gsub("\n", "\n--") .. "\n")
  end,
  as_nomsu = function(self, inline)
    if inline == nil then
      inline = false
    end
    if inline then
      return nil
    end
    if self.value:match("\n") then
      return Nomsu(self.source, "#..", self.value:gsub("\n", "\n    "))
    else
      return Nomsu(self.source, "#", self.value)
    end
  end,
  map = function(self, fn)
    return fn(self) or self
  end
})
return Types
