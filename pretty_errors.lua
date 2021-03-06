require("containers")
local Text = require('text')
local C = require('colors')
local box
box = function(text)
  local max_line = 0
  for line in text:gmatch("[^\n]+") do
    max_line = math.max(max_line, #(line:gsub("\027%[[0-9;]*m", "")))
  end
  local ret = ("\n" .. text):gsub("\n([^\n]*)", function(line)
    local line_len = #(line:gsub("\027%[[0-9;]*m", ""))
    return "\n" .. tostring(line) .. tostring((" "):rep(max_line - line_len)) .. " " .. C('reset')
  end)
  return ret:sub(2, -1), max_line
end
local format_error
format_error = function(err)
  local context = err.context or 2
  local err_line, err_linenum, err_linepos = err.source:line_info_at(err.start)
  local err_size = math.min((err.stop - err.start), (#err_line - err_linepos) + 1)
  local nl_indicator = (err_linepos > #err_line) and " " or ""
  local fmt_str = " %" .. tostring(#tostring(err_linenum + context)) .. "d|"
  local pointer = (" "):rep(err_linepos + #fmt_str:format(0) - 1) .. ("^"):rep(err_size)
  local err_msg = C('bold red', err.title or "Error") .. C('red', " at " .. tostring(err.filename or '???') .. ":" .. tostring(err_linenum) .. "," .. tostring(err_linepos))
  if not (COLOR_ENABLED) then
    err_msg = err_msg .. "\n"
  end
  local lines = err.source:lines()
  for i = err_linenum - context, err_linenum - 1 do
    do
      local line = lines[i]
      if line then
        err_msg = err_msg .. ("\n" .. C('dim', fmt_str:format(i)) .. line)
      end
    end
  end
  if err_line then
    local before = err_line:sub(1, err_linepos - 1)
    local during = err_line:sub(err_linepos, err_linepos + err_size - 1)
    local after = err_line:sub(err_linepos + err_size, -1)
    err_line = before .. C('black on red', during .. nl_indicator) .. after
    err_msg = err_msg .. ("\n" .. C('dim', fmt_str:format(err_linenum)) .. err_line)
  end
  local _, err_linenum_end, err_linepos_end = err.source:line_info_at(err.stop)
  err_linenum_end = err_linenum_end or err_linenum
  if err_linenum_end == err_linenum then
    err_msg = err_msg .. "\n" .. tostring(pointer)
  else
    for i = err_linenum + 1, err_linenum_end do
      do
        local line = lines[i]
        if line then
          if i == err_linenum_end then
            local during, after = line:sub(1, err_linepos_end - 1), line:sub(err_linepos_end, -1)
            err_msg = err_msg .. ("\n" .. C('dim', fmt_str:format(i)) .. C('black on red', during) .. after)
          else
            err_msg = err_msg .. ("\n" .. C('dim', fmt_str:format(i)) .. C('black on red', line))
          end
        end
      end
      if i > err_linenum + 1 + 5 then
        err_msg = err_msg .. "\n       ...\n"
        break
      end
    end
  end
  local box_width = 70
  local err_text = C('black on white', " " .. err.error:wrapped_to(box_width, 16):gsub("\n", "\n" .. C('black on white') .. " "))
  if err.hint then
    err_text = err_text .. ("\n" .. C('italic black on white', (" Suggestion: " .. tostring(err.hint)):wrapped_to(box_width, 16):gsub("\n", "\n" .. C('italic black on white') .. " ")))
  end
  err_msg = err_msg .. ("\n " .. box(err_text):gsub("\n", "\n "))
  if not (COLOR_ENABLED) then
    err_msg = err_msg .. "\n"
  end
  for i = err_linenum_end + 1, err_linenum_end + context do
    do
      local line = lines[i]
      if line then
        err_msg = err_msg .. ("\n" .. C('dim', fmt_str:format(i)) .. line)
      end
    end
  end
  if not (COLOR_ENABLED) then
    err_msg = err_msg .. "\n"
  end
  return err_msg
end
return format_error
