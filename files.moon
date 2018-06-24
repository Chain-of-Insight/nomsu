-- Some file utilities for searching for files recursively and using package.nomsupath
files = {}

_FILE_CACHE = {}

-- Create a fake file and put it in the cache
files.spoof = (filename, contents)->
    _FILE_CACHE[filename] = contents

-- Read a file's contents (searching first locally, then in the nomsupath)
files.read = (filename)->
    if file_contents = _FILE_CACHE[filename]
        return file_contents
    file = io.open(filename)
    if package.nomsupath and not file
        for nomsupath in package.nomsupath\gmatch("[^;]+")
            file = io.open(nomsupath.."/"..filename)
            break if file
    return nil unless file
    contents = file\read("*a")
    file\close!
    _FILE_CACHE[filename] = contents
    return contents

iterate_single = (item, prev) -> if item == prev then nil else item

-- `walk` returns an iterator over all files matching a path.
{:match, :sub, :rep, :gsub, :format, :byte, :match, :find} = string
iterate_single = (item, prev) -> if item == prev then nil else item
ok, lfs = pcall(require, "lfs")
if ok
    files.walk = (path)->
        -- Return 'true' if any files or directories are found, otherwise 'false'
        browse = (filename)->
            file_type = lfs.attributes(filename, 'mode')
            if file_type == 'file'
                if match(filename, "%.nom$") or match(filename, "%.lua$")
                    coroutine.yield filename
                    return true
            elseif file_type == 'directory'
                for subfile in lfs.dir(filename)
                    unless subfile == "." or subfile == ".."
                        browse(filename.."/"..subfile)
                return true
            elseif file_type == 'char device'
                coroutine.yield(filename)
                return true
            return false
        return coroutine.wrap ->
            if not browse(path) and package.nomsupath
                for nomsupath in package.nomsupath\gmatch("[^;]+")
                    break if browse(nomsupath.."/"..path)
            return nil
else
    ret = os.execute('find . -maxdepth 0')
    unless ret == true or ret == 0
        error "Could not find 'luafilesystem' module and couldn't run system command `find` (this might happen on Windows). Please install `luafilesystem` (which can be found at: http://keplerproject.github.io/luafilesystem/ or `luarocks install luafilesystem`)", 0

    files.walk = (path)->
        -- Sanitize path
        if match(path, "%.nom$") or match(path, "%.lua$") or match(path, "^/dev/fd/[012]$")
            return iterate_single, path
        -- TODO: improve sanitization
        path = gsub(path,"\\","\\\\")
        path = gsub(path,"`","")
        path = gsub(path,'"','\\"')
        path = gsub(path,"$","")
        return coroutine.wrap ->
            f = io.popen('find -L "'..path..'" -not -path "*/\\.*" -type f -name "*.nom"')
            found = false
            for line in f\lines!
                found = true
                coroutine.yield(line)
            if not found and package.nomsupath
                f\close!
                for nomsupath in package.nomsupath\gmatch("[^;]+")
                    f = io.popen('find -L "'..package.nomsupath..'/'..path..'" -not -path "*/\\.*" -type f -name "*.nom"')
                    for line in f\lines!
                        found = true
                        coroutine.yield(line)
                    f\close!
                    break if found
            unless found
                error("Invalid file path: "..tostring(path))

return files
