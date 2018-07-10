local files = require("files")
local debug_getinfo = debug.getinfo
local ok, to_lua = pcall(function()
  return require('moonscript.base').to_lua
end)
if not ok then
  to_lua = function()
    return nil
  end
end
local MOON_SOURCE_MAP = setmetatable({ }, {
  __index = function(self, file)
    local _, line_table = to_lua(file)
    self[file] = line_table or false
    return line_table or false
  end
})
debug.getinfo = function(thread, f, what)
  if what == nil then
    f, what, thread = thread, f, nil
  end
  if type(f) == 'number' then
    f = f + 1
  end
  local info
  if thread == nil then
    info = debug_getinfo(f, what)
  else
    info = debug_getinfo(thread, f, what)
  end
  if not info or not info.func then
    return info
  end
  if info.short_src or info.source or info.linedefine or info.currentline then
    do
      local map = SOURCE_MAP[info.source]
      if map then
        if info.currentline then
          info.currentline = assert(map[info.currentline])
        end
        if info.linedefined then
          info.linedefined = assert(map[info.linedefined])
        end
        if info.lastlinedefined then
          info.lastlinedefined = assert(map[info.lastlinedefined])
        end
        info.short_src = info.source:match('@([^[]*)') or info.short_src
        if info.name then
          do
            local tmp = info.name:match("^A_([a-zA-Z0-9_]*)$")
            if tmp then
              info.name = tmp:gsub("_", " "):gsub("x([0-9A-F][0-9A-F])", function(self)
                return string.char(tonumber(self, 16))
              end)
            else
              info.name = info.name
            end
          end
        else
          info.name = "main chunk"
        end
      end
    end
  end
  return info
end
local print_error
print_error = function(error_message, start_fn, stop_fn)
  io.stderr:write(tostring(colored.red("ERROR:")) .. " " .. tostring(colored.bright(colored.red((error_message or "")))) .. "\n")
  io.stderr:write("stack traceback:\n")
  local level = 1
  local found_start = false
  while true do
    local _continue_0 = false
    repeat
      local calling_fn = debug_getinfo(level)
      if not calling_fn then
        break
      end
      level = level + 1
      if not (found_start) then
        if calling_fn.func == start_fn then
          found_start = true
        end
        _continue_0 = true
        break
      end
      local name = calling_fn.name and "function '" .. tostring(calling_fn.name) .. "'" or nil
      if calling_fn.linedefined == 0 then
        name = "main chunk"
      end
      if name == "run_lua_fn" then
        _continue_0 = true
        break
      end
      local line = nil
      do
        local map = SOURCE_MAP[calling_fn.source]
        if map then
          if calling_fn.currentline then
            calling_fn.currentline = assert(map[calling_fn.currentline])
          end
          if calling_fn.linedefined then
            calling_fn.linedefined = assert(map[calling_fn.linedefined])
          end
          if calling_fn.lastlinedefined then
            calling_fn.lastlinedefined = assert(map[calling_fn.lastlinedefined])
          end
          local filename, start, stop = calling_fn.source:match('@([^[]*)%[([0-9]+):([0-9]+)]')
          assert(filename)
          local file = files.read(filename)
          local err_line = files.get_line(file, calling_fn.currentline)
          local offending_statement = colored.bright(colored.red(err_line:match("^[ ]*(.*)")))
          if calling_fn.name then
            do
              local tmp = calling_fn.name:match("^A_([a-zA-Z0-9_]*)$")
              if tmp then
                name = "action '" .. tostring(tmp:gsub("_", " "):gsub("x([0-9A-F][0-9A-F])", function(self)
                  return string.char(tonumber(self, 16))
                end)) .. "'"
              else
                name = "action '" .. tostring(calling_fn.name) .. "'"
              end
            end
          else
            name = "main chunk"
          end
          line = colored.yellow(tostring(filename) .. ":" .. tostring(calling_fn.currentline) .. " in " .. tostring(name) .. "\n        " .. tostring(offending_statement))
        else
          local file
          ok, file = pcall(function()
            return files.read(calling_fn.short_src)
          end)
          if not ok then
            file = nil
          end
          local line_num
          if name == nil then
            local search_level = level
            local _info = debug.getinfo(search_level)
            while true do
              search_level = search_level + 1
              _info = debug.getinfo(search_level)
              if not (_info) then
                break
              end
              for i = 1, 999 do
                local varname, val = debug.getlocal(search_level, i)
                if not varname then
                  break
                end
                if val == calling_fn.func then
                  name = "local '" .. tostring(varname) .. "'"
                  if not varname:match("%(") then
                    break
                  end
                end
              end
              if not (name) then
                for i = 1, _info.nups do
                  local varname, val = debug.getupvalue(_info.func, i)
                  if not varname then
                    break
                  end
                  if val == calling_fn.func then
                    name = "upvalue '" .. tostring(varname) .. "'"
                    if not varname:match("%(") then
                      break
                    end
                  end
                end
              end
            end
          end
          if file and (calling_fn.short_src:match("%.moon$") or file:match("^#![^\n]*moon\n")) and type(MOON_SOURCE_MAP[file]) == 'table' then
            local char = MOON_SOURCE_MAP[file][calling_fn.currentline]
            line_num = 1
            for _ in file:sub(1, char):gmatch("\n") do
              line_num = line_num + 1
            end
            line = colored.cyan(tostring(calling_fn.short_src) .. ":" .. tostring(line_num) .. " in " .. tostring(name or '?'))
          else
            line_num = calling_fn.currentline
            if calling_fn.short_src == '[C]' then
              line = colored.green(tostring(calling_fn.short_src) .. " in " .. tostring(name or '?'))
            else
              line = colored.blue(tostring(calling_fn.short_src) .. ":" .. tostring(calling_fn.currentline) .. " in " .. tostring(name or '?'))
            end
          end
          if file then
            local err_line = files.get_line(file, line_num)
            local offending_statement = colored.bright(colored.red(err_line:match("^[ ]*(.*)$")))
            line = line .. ("\n        " .. offending_statement)
          end
        end
      end
      io.stderr:write(line, "\n")
      if calling_fn.istailcall then
        io.stderr:write("    " .. tostring(colored.dim(colored.white("  (...tail calls...)"))) .. "\n")
      end
      if calling_fn.func == stop_fn then
        break
      end
      _continue_0 = true
    until true
    if not _continue_0 then
      break
    end
  end
  return io.stderr:flush()
end
local guard
guard = function(fn)
  local error_handler
  error_handler = function(error_message)
    print_error(error_message, error_handler, fn)
    local EXIT_FAILURE = 1
    return os.exit(EXIT_FAILURE)
  end
  return xpcall(fn, error_handler)
end
return {
  guard = guard,
  print_error = print_error
}
