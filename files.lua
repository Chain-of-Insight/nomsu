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
local _browse_cache = { }
local browse
browse = function(path)
  if not (_browse_cache[path]) then
    local f = io.popen('find -L "' .. package.nomsupath .. '/' .. path .. '" -not -path "*/\\.*" -type f -name "*.nom"')
    local _files
    do
      local _tbl_0 = { }
      for line in f:lines() do
        local _key_0, _val_0 = line
        _tbl_0[_key_0] = _val_0
      end
      _files = _tbl_0
    end
    if not (f:close()) then
      _files = false
    end
    _browse_cache[path] = _files
  end
  return _browse_cache[path]
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
  browse = function(filename)
    if not (_browse_cache[filename]) then
      _browse_cache[filename] = false
      local file_type, err = lfs.attributes(filename, 'mode')
      if file_type == 'file' then
        if match(filename, "%.nom$") or match(filename, "%.lua$") then
          _browse_cache[filename] = {
            filename
          }
        end
      elseif file_type == 'char device' then
        _browse_cache[filename] = {
          filename
        }
      elseif file_type == 'directory' or file_type == 'link' then
        local _files = { }
        for subfile in lfs.dir(filename) do
          if not (subfile == "." or subfile == "..") then
            local _list_0 = (browse(filename .. "/" .. subfile) or { })
            for _index_0 = 1, #_list_0 do
              local f = _list_0[_index_0]
              _files[#_files + 1] = f
            end
          end
        end
        _browse_cache[filename] = _files
      end
    end
    return _browse_cache[filename]
  end
else
  if io.popen('find . -maxdepth 0'):close() then
    error("Could not find 'luafilesystem' module and couldn't run system command `find` (this might happen on Windows). Please install `luafilesystem` (which can be found at: http://keplerproject.github.io/luafilesystem/ or `luarocks install luafilesystem`)", 0)
  end
end
files.walk = function(path, flush_cache)
  if flush_cache == nil then
    flush_cache = false
  end
  if flush_cache then
    _browse_cache = { }
  end
  local _files = browse(path)
  if package.nomsupath and not _files then
    for nomsupath in package.nomsupath:gmatch("[^;]+") do
      do
        _files = browse(nomsupath .. "/" .. path)
        if _files then
          break
        end
      end
    end
  end
  local iter
  iter = function(_files, i)
    i = i + 1
    do
      local f = _files[i]
      if f then
        return i, f
      end
    end
  end
  return iter, _files, 0
end
local line_counter = re.compile([[    lines <- {| line (%nl line)* |}
    line <- {} (!%nl .)*
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
local log = { }
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
local get_lines = re.compile([[    lines <- {| line (%nl line)* |}
    line <- {[^%nl]*}
]], {
  nl = lpeg.P("\r") ^ -1 * lpeg.P("\n")
})
files.get_lines = function(str)
  return get_lines:match(str)
end
return files
