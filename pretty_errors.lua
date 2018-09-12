require("containers")
local string2 = require('string2')
local box
box = function(text)
  local max_line = 0
  for line in text:gmatch("[^\n]+") do
    max_line = math.max(max_line, #(line:gsub("\027%[[0-9;]*m", "")))
  end
  local ret = ("\n" .. text):gsub("\n([^\n]*)", function(line)
    local line_len = #(line:gsub("\027%[[0-9;]*m", ""))
    return "\n" .. tostring(line) .. tostring((" "):rep(max_line - line_len)) .. " \027[0m"
  end)
  return ret:sub(2, -1), max_line
end
local format_error
format_error = function(err)
  local context = err.context or 2
  local err_line, err_linenum, err_linepos = string2.line_at(err.source, err.start)
  local err_size = math.min((err.stop - err.start), (#err_line - err_linepos) + 1)
  local nl_indicator = (err_linepos > #err_line) and " " or ""
  local fmt_str = " %" .. tostring(#tostring(err_linenum + context)) .. "d|"
  local pointer
  if err_size >= 2 then
    pointer = (" "):rep(err_linepos + #fmt_str:format(0) - 1) .. "╚" .. tostring(("═"):rep(err_size - 2)) .. "╝"
  else
    pointer = (" "):rep(err_linepos + #fmt_str:format(0) - 1) .. "⬆"
  end
  local err_msg = "\027[33;41;1mParse error at " .. tostring(err.filename) .. ":" .. tostring(err_linenum) .. "\027[0m"
  for i = err_linenum - context, err_linenum - 1 do
    do
      local line = string2.line(err.source, i)
      if line then
        err_msg = err_msg .. "\n\027[2m" .. tostring(fmt_str:format(i)) .. "\027[0m" .. tostring(line) .. "\027[0m"
      end
    end
  end
  if err_line then
    local box_width = 60
    local before = err_line:sub(1, err_linepos - 1)
    local during = err_line:sub(err_linepos, err_linepos + err_size - 1)
    local after = err_line:sub(err_linepos + err_size, -1)
    err_line = "\027[0m" .. tostring(before) .. "\027[41;30m" .. tostring(during) .. tostring(nl_indicator) .. "\027[0m" .. tostring(after)
    err_msg = err_msg .. "\n\027[2m" .. tostring(fmt_str:format(err_linenum)) .. tostring(err_line) .. "\027[0m\n" .. tostring(pointer)
    local err_text = "\027[47;31;1m" .. tostring((" " .. err.error):wrap_to_1(box_width):gsub("\n", "\n\027[47;31;1m "))
    if err.hint then
      err_text = err_text .. "\n\027[47;30m" .. tostring((" Suggestion: " .. tostring(err.hint)):wrap_to_1(box_width):gsub("\n", "\n\027[47;30m "))
    end
    err_msg = err_msg .. ("\n\027[33;1m " .. box(err_text):gsub("\n", "\n "))
  end
  for i = err_linenum + 1, err_linenum + context do
    do
      local line = string2.line(err.source, i)
      if line then
        err_msg = err_msg .. "\n\027[2m" .. tostring(fmt_str:format(i)) .. "\027[0m" .. tostring(line) .. "\027[0m"
      end
    end
  end
  return err_msg
end
return format_error
