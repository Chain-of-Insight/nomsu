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
  files.walk = function(path)
    local browse
    browse = function(filename)
      local file_type = lfs.attributes(filename, 'mode')
      if file_type == 'file' then
        if match(filename, "%.nom$") or match(filename, "%.lua$") then
          coroutine.yield(filename)
          return true
        end
      elseif file_type == 'directory' then
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
  local ret = os.execute('find . -maxdepth 0')
  if not (ret == true or ret == 0) then
    error("Could not find 'luafilesystem' module and couldn't run system command `find` (this might happen on Windows). Please install `luafilesystem` (which can be found at: http://keplerproject.github.io/luafilesystem/ or `luarocks install luafilesystem`)", 0)
  end
  files.walk = function(path)
    if match(path, "%.nom$") or match(path, "%.lua$") or match(path, "^/dev/fd/[012]$") then
      return iterate_single, path
    end
    path = gsub(path, "\\", "\\\\")
    path = gsub(path, "`", "")
    path = gsub(path, '"', '\\"')
    path = gsub(path, "$", "")
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
return files
