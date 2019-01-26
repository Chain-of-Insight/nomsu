local List, Dict
do
  local _obj_0 = require("containers")
  List, Dict = _obj_0.List, _obj_0.Dict
end
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
local as_lua_id
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
end
local from_lua_id
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
local Text = {
  isplit = isplit,
  uppercase = upper,
  lowercase = lower,
  reversed = reverse,
  is_lua_id = is_lua_id,
  capitalized = function(self)
    return (gsub(self, '%l', upper, 1))
  end,
  byte = byte,
  as_a_number = tonumber,
  as_a_base_1_number = tonumber,
  bytes = function(self, i, j)
    return List({
      byte(self, i or 1, j or -1)
    })
  end,
  split = function(self, sep)
    return List((function()
      local _accum_0 = { }
      local _len_0 = 1
      for i, chunk in isplit(self, sep) do
        _accum_0[_len_0] = chunk
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())
  end,
  starts_with = function(self, s)
    return sub(self, 1, #s) == s
  end,
  ends_with = function(self, s)
    return #self >= #s and sub(self, #self - #s, -1) == s
  end,
  lines = function(self)
    return List((function()
      local _accum_0 = { }
      local _len_0 = 1
      for i, line in isplit(self, '\n') do
        _accum_0[_len_0] = line
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)())
  end,
  line = function(self, line_num)
    for i, line, start in isplit(self, '\n') do
      if i == line_num then
        return line
      end
    end
  end,
  line_info_at = function(self, pos)
    assert(type(pos) == 'number', "Invalid string position")
    for i, line, start, stop in isplit(self, '\n') do
      if stop + 1 >= pos then
        return line, i, (pos - start + 1)
      end
    end
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
    return self:as_lua()
  end,
  formatted_with = format,
  byte = byte,
  position_of = (function(...)
    return (find(...))
  end),
  position_of_1_after = (function(...)
    return (find(...))
  end),
  as_a_lua_identifier = as_lua_id,
  is_a_lua_identifier = is_lua_id,
  as_lua_id = as_lua_id,
  as_a_lua_id = as_lua_id,
  is_a_lua_id = is_lua_id,
  is_lua_id = is_lua_id,
  from_lua_id = from_lua_id,
  bytes_1_to = function(self, start, stop)
    return List({
      byte(self, start, stop)
    })
  end,
  [as_lua_id("with 1 ->")] = function(...)
    return (gsub(...))
  end,
  bytes = function(self)
    return List({
      byte(self, 1, -1)
    })
  end,
  wrapped_to = function(self, maxlen, margin)
    if maxlen == nil then
      maxlen = 80
    end
    if margin == nil then
      margin = 8
    end
    local lines = { }
    local _list_0 = self:lines()
    for _index_0 = 1, #_list_0 do
      local line = _list_0[_index_0]
      while #line > maxlen do
        local chunk = sub(line, 1, maxlen)
        local split = find(chunk, ' ', maxlen - margin, true) or maxlen
        chunk = sub(line, 1, split)
        line = sub(line, split + 1, -1)
        lines[#lines + 1] = chunk
      end
      lines[#lines + 1] = line
    end
    return table.concat(lines, "\n")
  end,
  line_at = function(self, i)
    return (self:line_info_at(i))
  end,
  line_number_at = function(self, i)
    return select(2, self:line_info_at(i))
  end,
  line_position_at = function(self, i)
    return select(3, self:line_info_at(i))
  end,
  matches = function(self, patt)
    return match(self, patt) and true or false
  end,
  matching = function(self, patt)
    return (match(self, patt))
  end,
  matching_groups = function(self, patt)
    return List({
      match(self, patt)
    })
  end,
  [as_lua_id("* 1")] = function(self, n)
    return rep(self, n)
  end,
  all_matches_of = function(self, patt)
    local result = { }
    local stepper, x, i = gmatch(self, patt)
    while true do
      local tmp = List({
        stepper(x, i)
      })
      if #tmp == 0 then
        break
      end
      i = tmp[1]
      result[#result + 1] = (#tmp == 1) and tmp[1] or tmp
    end
    return List(result)
  end,
  from_1_to = sub,
  from = sub,
  character = function(self, i)
    return sub(self, i, i)
  end
}
for k, v in pairs(string) do
  Text[k] = Text[k] or v
end
local _1_as_text
_1_as_text = function(x)
  if x == true then
    return "yes"
  end
  if x == false then
    return "no"
  end
  return tostring(x)
end
setmetatable(Text, {
  __call = function(self, ...)
    local ret = {
      ...
    }
    for i = 1, select("#", ...) do
      ret[i] = _1_as_text(ret[i])
    end
    return table.concat(ret)
  end
})
debug.setmetatable("", {
  __type = "Text",
  __index = function(self, k)
    return Text[k] or (type(k) == 'number' and sub(self, k, k) or nil)
  end,
  __add = function(self, x)
    return _1_as_text(self) .. _1_as_text(x)
  end
})
return Text
