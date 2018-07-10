local lpeg = require('lpeg')
local re = require('re')
local files = { }
local _FILE_CACHE = { }
files.spoof = function(filename, contents)
  _FILE_CACHE[filename] = contents
end
files.read = function(filename)
  do
    local file_contents = _FILE_CACHE[filename]
    if file_contents then
      return file_contents
    end
  end
  if filename == 'stdin' then
    local contents = io.read('*a')
    _FILE_CACHE['stdin'] = contents
    return contents
  end
  local file = io.open(filename)
  if package.nomsupath and not file then
    for nomsupath in package.nomsupath:gmatch("[^;]+") do
      file = io.open(nomsupath .. "/" .. filename)
      if file then
        break
      end
    end
  end
  if not (file) then
    return nil
  end
  local contents = file:read("*a")
  file:close()
  _FILE_CACHE[filename] = contents
  return contents
end
local iterate_single
iterate_single = function(item, prev)
  if item == prev then
    return nil
  else
    return item
  end
end
local match, gsub
do
  local _obj_0 = string
  match, gsub = _obj_0.match, _obj_0.gsub
end
iterate_single = function(item, prev)
  if item == prev then
    return nil
  else
    return item
  end
end
local ok, lfs = pcall(require, "lfs")
if ok then
  local raw_file_exists
  raw_file_exists = function(filename)
    local mode = lfs.attributes(filename, 'mode')
    if mode == 'file' or mode == 'directory' or mode == 'link' then
      return true
    else
      return false
    end
  end
  files.exists = function(path)
    if path == 'stdin' or raw_file_exists(path) then
      return true
    end
    if package.nomsupath then
      for nomsupath in package.nomsupath:gmatch("[^;]+") do
        if raw_file_exists(nomsupath .. "/" .. path) then
          return true
        end
      end
    end
    return false
  end
  local browse
  browse = function(filename)
    local file_type, err = lfs.attributes(filename, 'mode')
    if file_type == 'file' then
      if match(filename, "%.nom$") or match(filename, "%.lua$") then
        coroutine.yield(filename)
        return true
      end
    elseif file_type == 'directory' or file_type == 'link' then
      for subfile in lfs.dir(filename) do
        if not (subfile == "." or subfile == "..") then
          browse(filename .. "/" .. subfile)
        end
      end
      return true
    elseif file_type == 'char device' then
      coroutine.yield(filename)
      return true
    end
    return false
  end
  files.walk = function(path)
    if match(path, "%.nom$") or match(path, "%.lua$") or path == 'stdin' then
      return iterate_single, path
    end
    return coroutine.wrap(function()
      if not browse(path) and package.nomsupath then
        for nomsupath in package.nomsupath:gmatch("[^;]+") do
          if browse(nomsupath .. "/" .. path) then
            break
          end
        end
      end
      return nil
    end)
  end
else
  if io.popen('find . -maxdepth 0'):close() then
    error("Could not find 'luafilesystem' module and couldn't run system command `find` (this might happen on Windows). Please install `luafilesystem` (which can be found at: http://keplerproject.github.io/luafilesystem/ or `luarocks install luafilesystem`)", 0)
  end
  local sanitize
  sanitize = function(path)
    path = gsub(path, "\\", "\\\\")
    path = gsub(path, "`", "")
    path = gsub(path, '"', '\\"')
    path = gsub(path, "$", "")
    return path
  end
  files.exists = function(path)
    if not (io.popen("ls " .. tostring(sanitize(path))):close()) then
      return true
    end
    if package.nomsupath then
      for nomsupath in package.nomsupath:gmatch("[^;]+") do
        if not (io.popen("ls " .. tostring(nomsupath) .. "/" .. tostring(sanitize(path))):close()) then
          return true
        end
      end
    end
    return false
  end
  files.walk = function(path)
    if match(path, "%.nom$") or match(path, "%.lua$") or path == 'stdin' then
      return iterate_single, path
    end
    path = sanitize(path)
    return coroutine.wrap(function()
      local f = io.popen('find -L "' .. path .. '" -not -path "*/\\.*" -type f -name "*.nom"')
      local found = false
      for line in f:lines() do
        found = true
        coroutine.yield(line)
      end
      if not found and package.nomsupath then
        f:close()
        for nomsupath in package.nomsupath:gmatch("[^;]+") do
          f = io.popen('find -L "' .. package.nomsupath .. '/' .. path .. '" -not -path "*/\\.*" -type f -name "*.nom"')
          for line in f:lines() do
            found = true
            coroutine.yield(line)
          end
          f:close()
          if found then
            break
          end
        end
      end
      if not (found) then
        return error("Invalid file path: " .. tostring(path))
      end
    end)
  end
end
local line_counter = re.compile([[    lines <- {| line (%nl line)* |}
    line <- {} (!%nl .)*
]], {
  nl = lpeg.P("\r") ^ -1 * lpeg.P("\n")
})
local get_lines = re.compile([[    lines <- {| line (%nl line)* |}
    line <- {[^%nl]*}
]], {
  nl = lpeg.P("\r") ^ -1 * lpeg.P("\n")
})
local _LINE_STARTS = { }
files.get_line_starts = function(str)
  if type(str) ~= 'string' then
    str = tostring(str)
  end
  do
    local starts = _LINE_STARTS[str]
    if starts then
      return starts
    end
  end
  local line_starts = line_counter:match(str)
  _LINE_STARTS[str] = line_starts
  return line_starts
end
files.get_line_number = function(str, pos)
  local line_starts = files.get_line_starts(str)
  local lo, hi = 1, #line_starts
  while lo <= hi do
    local mid = math.floor((lo + hi) / 2)
    if line_starts[mid] > pos then
      hi = mid - 1
    else
      lo = mid + 1
    end
  end
  return hi
end
files.get_line = function(str, line_no)
  local line_starts = files.get_line_starts(str)
  return str:sub(line_starts[line_no] or 1, (line_starts[line_no + 1] or 1) - 2)
end
files.get_lines = function(str)
  return get_lines:match(str)
end
return files
