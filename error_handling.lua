local debug_getinfo = debug.getinfo
local Files = require("files")
local C = require("colors")
local pretty_error = require("pretty_errors")
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
          info.name = "action '" .. tostring(info.name:from_lua_id()) .. "'"
        else
          info.name = "main chunk"
        end
      end
    end
  end
  return info
end
local enhance_error
enhance_error = function(error_message, start_fn, stop_fn)
  if not (error_message and error_message:match("%d|")) then
    error_message = error_message or ""
    do
      local fn = (error_message:match("attempt to call a nil value %(global '(.*)'%)") or error_message:match("attempt to call global '(.*)' %(a nil value%)"))
      if fn then
        error_message = "The action '" .. tostring(fn:from_lua_id()) .. "' is not defined."
      end
    end
    local level = 2
    while true do
      local calling_fn = debug_getinfo(level)
      if not calling_fn then
        break
      end
      level = level + 1
      local filename, file, line_num
      do
        local map = SOURCE_MAP and SOURCE_MAP[calling_fn.source]
        if map then
          if calling_fn.currentline then
            line_num = assert(map[calling_fn.currentline])
          end
          local start, stop
          filename, start, stop = calling_fn.source:match('@([^[]*)%[([0-9]+):([0-9]+)]')
          if not filename then
            filename, start = calling_fn.source:match('@([^[]*)%[([0-9]+)]')
          end
          assert(filename)
          file = Files.read(filename)
        else
          filename = calling_fn.short_src
          file = Files.read(filename)
          if calling_fn.short_src:match("%.moon$") and type(MOON_SOURCE_MAP[file]) == 'table' then
            local char = MOON_SOURCE_MAP[file][calling_fn.currentline]
            line_num = file:line_number_at(char)
          else
            line_num = calling_fn.currentline
          end
        end
      end
      if file and filename and line_num then
        local start = 1
        local lines = file:lines()
        for i = 1, line_num - 1 do
          start = start + #lines[i] + 1
        end
        local stop = start + #lines[line_num]
        start = start + #lines[line_num]:match("^ *")
        error_message = pretty_error({
          title = "Error",
          error = error_message,
          source = file,
          start = start,
          stop = stop,
          filename = filename
        })
        break
      end
      if calling_fn.func == xpcall then
        break
      end
    end
  end
  local ret = {
    C('bold red', error_message or "Error"),
    "stack traceback:"
  }
  local level = 2
  while true do
    local _continue_0 = false
    repeat
      local calling_fn = debug_getinfo(level)
      if not calling_fn then
        break
      end
      if calling_fn.func == xpcall then
        break
      end
      level = level + 1
      local name = calling_fn.name and "function '" .. tostring(calling_fn.name) .. "'" or nil
      if calling_fn.linedefined == 0 then
        name = "main chunk"
      end
      if name == "function 'run_lua_fn'" then
        _continue_0 = true
        break
      end
      local line = nil
      do
        local map = SOURCE_MAP and SOURCE_MAP[calling_fn.source]
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
          if not filename then
            filename, start = calling_fn.source:match('@([^[]*)%[([0-9]+)]')
          end
          assert(filename)
          if calling_fn.name then
            name = "action '" .. tostring(calling_fn.name:from_lua_id()) .. "'"
          else
            name = "main chunk"
          end
          local file = Files.read(filename)
          local lines = file and file:lines() or { }
          do
            local err_line = lines[calling_fn.currentline]
            if err_line then
              local offending_statement = C('bright red', err_line:match("^[ ]*(.*)"))
              line = C('yellow', tostring(filename) .. ":" .. tostring(calling_fn.currentline) .. " in " .. tostring(name) .. "\n        " .. tostring(offending_statement))
            else
              line = C('yellow', tostring(filename) .. ":" .. tostring(calling_fn.currentline) .. " in " .. tostring(name))
            end
          end
        else
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
          local file, lines
          do
            file = Files.read(calling_fn.short_src)
            if file then
              lines = file:lines()
            end
          end
          if file and (calling_fn.short_src:match("%.moon$") or file:match("^#![^\n]*moon\n")) and type(MOON_SOURCE_MAP[file]) == 'table' then
            local char = MOON_SOURCE_MAP[file][calling_fn.currentline]
            line_num = file:line_number_at(char)
            line = C('cyan', tostring(calling_fn.short_src) .. ":" .. tostring(line_num) .. " in " .. tostring(name or '?'))
          else
            line_num = calling_fn.currentline
            if calling_fn.short_src == '[C]' then
              line = C('green', tostring(calling_fn.short_src) .. " in " .. tostring(name or '?'))
            else
              line = C('blue', tostring(calling_fn.short_src) .. ":" .. tostring(calling_fn.currentline) .. " in " .. tostring(name or '?'))
            end
          end
          if file then
            do
              local err_line = lines[line_num]
              if err_line then
                local offending_statement = C('bright red', tostring(err_line:match("^[ ]*(.*)$")))
                line = line .. ("\n        " .. offending_statement)
              end
            end
          end
        end
      end
      table.insert(ret, line)
      if calling_fn.istailcall then
        table.insert(ret, C('dim', "      (...tail calls...)"))
      end
      _continue_0 = true
    until true
    if not _continue_0 then
      break
    end
  end
  return table.concat(ret, "\n")
end
local guard
guard = function(fn)
  local err
  ok, err = xpcall(fn, enhance_error)
  if not ok then
    io.stderr:write(err)
    io.stderr:flush()
    return os.exit(1)
  end
end
return {
  guard = guard,
  enhance_error = enhance_error,
  print_error = print_error
}
