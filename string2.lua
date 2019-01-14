local reverse, upper, lower, find, byte, match, gmatch, gsub, sub, format, rep, char
do
  local _obj_0 = string
  reverse, upper, lower, find, byte, match, gmatch, gsub, sub, format, rep, char = _obj_0.reverse, _obj_0.upper, _obj_0.lower, _obj_0.find, _obj_0.byte, _obj_0.match, _obj_0.gmatch, _obj_0.gsub, _obj_0.sub, _obj_0.format, _obj_0.rep, _obj_0.char
end
local isplit
isplit = function(self, sep)
  if sep == nil then
    sep = '%s+'
  end
  local step
  step = function(self, i)
    local start = self.pos
    if not (start) then
      return 
    end
    i = i + 1
    local nl = find(self.str, self.sep, start)
    self.pos = nl and (nl + 1) or nil
    local line = sub(self.str, start, nl and (nl - 1) or #self.str)
    return i, line, start, (nl and (nl - 1) or #self.str)
  end
  return step, {
    str = self,
    pos = 1,
    sep = sep
  }, 0
end
local lua_keywords = {
  ["and"] = true,
  ["break"] = true,
  ["do"] = true,
  ["else"] = true,
  ["elseif"] = true,
  ["end"] = true,
  ["false"] = true,
  ["for"] = true,
  ["function"] = true,
  ["goto"] = true,
  ["if"] = true,
  ["in"] = true,
  ["local"] = true,
  ["nil"] = true,
  ["not"] = true,
  ["or"] = true,
  ["repeat"] = true,
  ["return"] = true,
  ["then"] = true,
  ["true"] = true,
  ["until"] = true,
  ["while"] = true
}
local is_lua_id
is_lua_id = function(str)
  return match(str, "^[_a-zA-Z][_a-zA-Z0-9]*$") and not lua_keywords[str]
end
local string2 = {
  isplit = isplit,
  uppercase = upper,
  lowercase = lower,
  reversed = reverse,
  is_lua_id = is_lua_id,
  capitalized = function(self)
    return gsub(self, '%l', upper, 1)
  end,
  byte = byte,
  bytes = function(self, i, j)
    return {
      byte(self, i or 1, j or -1)
    }
  end,
  split = function(self, sep)
    local _accum_0 = { }
    local _len_0 = 1
    for i, chunk in isplit(self, sep) do
      _accum_0[_len_0] = chunk
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end,
  starts_with = function(self, s)
    return sub(self, 1, #s) == s
  end,
  ends_with = function(self, s)
    return #self >= #s and sub(self, #self - #s, -1) == s
  end,
  lines = function(self)
    local _accum_0 = { }
    local _len_0 = 1
    for i, line in isplit(self, '\n') do
      _accum_0[_len_0] = line
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end,
  line = function(self, line_num)
    for i, line, start in isplit(self, '\n') do
      if i == line_num then
        return line
      end
    end
  end,
  line_at = function(self, pos)
    assert(type(pos) == 'number', "Invalid string position")
    for i, line, start, stop in isplit(self, '\n') do
      if stop + 1 >= pos then
        return line, i, (pos - start + 1)
      end
    end
  end,
  wrap = function(self, maxlen, buffer)
    if maxlen == nil then
      maxlen = 80
    end
    if buffer == nil then
      buffer = 8
    end
    local lines = { }
    local _list_0 = self:lines()
    for _index_0 = 1, #_list_0 do
      local line = _list_0[_index_0]
      while #line > maxlen do
        local chunk = sub(line, 1, maxlen)
        local split = find(chunk, ' ', maxlen - buffer, true) or maxlen
        chunk = sub(line, 1, split)
        line = sub(line, split + 1, -1)
        lines[#lines + 1] = chunk
      end
      lines[#lines + 1] = line
    end
    return table.concat(lines, "\n")
  end,
  indented = function(self, indent)
    if indent == nil then
      indent = "    "
    end
    return indent .. (gsub(self, "\n", "\n" .. indent))
  end,
  as_lua = function(self)
    local escaped = gsub(self, "\\", "\\\\")
    escaped = gsub(escaped, "\n", "\\n")
    escaped = gsub(escaped, '"', '\\"')
    escaped = gsub(escaped, "[^ %g]", function(c)
      return format("\\%03d", byte(c, 1))
    end)
    return '"' .. escaped .. '"'
  end,
  as_nomsu = function(self)
    local escaped = gsub(self, "\\", "\\\\")
    escaped = gsub(escaped, "\n", "\\n")
    escaped = gsub(escaped, '"', '\\"')
    escaped = gsub(escaped, "[^ %g]", function(c)
      return format("\\%03d", byte(c, 1))
    end)
    return '"' .. escaped .. '"'
  end,
  as_lua_id = function(str)
    str = gsub(str, "x([0-9A-F][0-9A-F])", "x78%1")
    str = gsub(str, "%W", function(c)
      if c == ' ' then
        return '_'
      else
        return format("x%02X", byte(c))
      end
    end)
    if not (is_lua_id(match(str, "^_*(.*)$"))) then
      str = "_" .. str
    end
    return str
  end,
  from_lua_id = function(str)
    if not (is_lua_id(match(str, "^_*(.*)$"))) then
      str = sub(str, 2, -1)
    end
    str = gsub(str, "_", " ")
    str = gsub(str, "x([0-9A-F][0-9A-F])", function(hex)
      return char(tonumber(hex, 16))
    end)
    return str
  end
}
for k, v in pairs(string) do
  string2[k] = string2[k] or v
end
local _list_0 = {
  "",
  "_",
  " ",
  "return",
  "asdf",
  "one two",
  "one_two",
  "Hex2Dec",
  "He-ec",
  "\3"
}
for _index_0 = 1, #_list_0 do
  local test = _list_0[_index_0]
  local lua_id = string2.as_lua_id(test)
  assert(is_lua_id(lua_id), "failed to convert '" .. tostring(test) .. "' to a valid Lua identifier (got '" .. tostring(lua_id) .. "')")
  local roundtrip = string2.from_lua_id(lua_id)
  assert(roundtrip == test, "Failed lua_id roundtrip: '" .. tostring(test) .. "' -> '" .. tostring(lua_id) .. "' -> '" .. tostring(roundtrip) .. "'")
end
return string2
