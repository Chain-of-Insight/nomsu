local lpeg = require('lpeg')
local re = require('re')
local Files = { }
local _SPOOFED_FILES = { }
local _FILE_CACHE = setmetatable({ }, {
  __index = _SPOOFED_FILES
})
local _BROWSE_CACHE = { }
Files.spoof = function(filename, contents)
  _SPOOFED_FILES[filename] = contents
  return contents
end
Files.read = function(filename)
  do
    local file_contents = _FILE_CACHE[filename]
    if file_contents then
      return file_contents
    end
  end
  if filename == 'stdin' then
    return Files.spoof('stdin', io.read('*a'))
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
local match, gsub
do
  local _obj_0 = string
  match, gsub = _obj_0.match, _obj_0.gsub
end
local sanitize
sanitize = function(path)
  path = gsub(path, "\\", "\\\\")
  path = gsub(path, "`", "")
  path = gsub(path, '"', '\\"')
  path = gsub(path, "$", "")
  return path
end
Files.exists = function(path)
  if _SPOOFED_FILES[path] then
    return true
  end
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
local browse
browse = function(path)
  if not (_BROWSE_CACHE[path]) then
    local files
    if _SPOOFED_FILES[path] then
      _BROWSE_CACHE[path] = {
        path
      }
    else
      local f = io.popen('find -L "' .. package.nomsupath .. '/' .. path .. '" -not -path "*/\\.*" -type f -name "*.nom"')
      do
        local _tbl_0 = { }
        for line in f:lines() do
          local _key_0, _val_0 = line
          _tbl_0[_key_0] = _val_0
        end
        files = _tbl_0
      end
      _BROWSE_CACHE[path] = f:close() and files or false
    end
  end
  return _BROWSE_CACHE[path]
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
  Files.exists = function(path)
    if _SPOOFED_FILES[path] then
      return true
    end
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
  browse = function(filename)
    if not (_BROWSE_CACHE[filename]) then
      if _SPOOFED_FILES[filename] then
        _BROWSE_CACHE[filename] = {
          filename
        }
      else
        local file_type, err = lfs.attributes(filename, 'mode')
        if file_type == 'file' then
          if match(filename, "%.nom$") or match(filename, "%.lua$") then
            _BROWSE_CACHE[filename] = {
              filename
            }
          else
            _BROWSE_CACHE[filename] = false
          end
        elseif file_type == 'char device' then
          _BROWSE_CACHE[filename] = {
            filename
          }
        elseif file_type == 'directory' or file_type == 'link' then
          local files = { }
          for subfile in lfs.dir(filename) do
            if not (subfile == "." or subfile == "..") then
              local _list_0 = (browse(filename .. "/" .. subfile) or { })
              for _index_0 = 1, #_list_0 do
                local f = _list_0[_index_0]
                files[#files + 1] = f
              end
            end
          end
          _BROWSE_CACHE[filename] = files
        else
          _BROWSE_CACHE[filename] = false
        end
      end
    end
    return _BROWSE_CACHE[filename]
  end
else
  if io.popen('find . -maxdepth 0'):close() then
    error("Could not find 'luafilesystem' module and couldn't run system command `find` (this might happen on Windows). Please install `luafilesystem` (which can be found at: http://keplerproject.github.io/luafilesystem/ or `luarocks install luafilesystem`)", 0)
  end
end
Files.walk = function(path, flush_cache)
  if flush_cache == nil then
    flush_cache = false
  end
  if flush_cache then
    _BROWSE_CACHE = { }
  end
  local files = browse(path)
  if package.nomsupath and not files then
    for nomsupath in package.nomsupath:gmatch("[^;]+") do
      do
        files = browse(nomsupath .. "/" .. path)
        if files then
          break
        end
      end
    end
  end
  local iter
  iter = function(files, i)
    if not (files) then
      return 
    end
    i = i + 1
    do
      local f = files[i]
      if f then
        return i, f
      end
    end
  end
  return iter, files, 0
end
local line_counter = re.compile([[    lines <- {| line (%nl line)* |}
    line <- {} (!%nl .)*
]], {
  nl = lpeg.P("\r") ^ -1 * lpeg.P("\n")
})
local _LINE_STARTS = { }
Files.get_line_starts = function(str)
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
local log = { }
Files.get_line_number = function(str, pos)
  local line_starts = Files.get_line_starts(str)
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
Files.get_line = function(str, line_no)
  local line_starts = Files.get_line_starts(str)
  return str:sub(line_starts[line_no] or 1, (line_starts[line_no + 1] or 1) - 2)
end
local get_lines = re.compile([[    lines <- {| line (%nl line)* |}
    line <- {[^%nl]*}
]], {
  nl = lpeg.P("\r") ^ -1 * lpeg.P("\n")
})
Files.get_lines = function(str)
  return get_lines:match(str)
end
return Files
