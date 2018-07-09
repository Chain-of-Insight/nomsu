-- Some file utilities for searching for files recursively and using package.nomsupath
lpeg = require 'lpeg'
re = require 're'
files = {}

_FILE_CACHE = {}

-- Create a fake file and put it in the cache
files.spoof = (filename, contents)->
    _FILE_CACHE[filename] = contents

-- Read a file's contents (searching first locally, then in the nomsupath)
files.read = (filename)->
    if file_contents = _FILE_CACHE[filename]
        return file_contents
    if filename == 'stdin'
        contents = io.read('*a')
        _FILE_CACHE['stdin'] = contents
        return contents
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
{:match, :gsub} = string
iterate_single = (item, prev) -> if item == prev then nil else item
ok, lfs = pcall(require, "lfs")
if ok
    raw_file_exists = (filename)->
        mode = lfs.attributes(filename, 'mode')
        return if mode == 'file' or mode == 'directory' then true else false
    files.exists = (path)->
        return true if path == 'stdin' or raw_file_exists(path)
        if package.nomsupath
            for nomsupath in package.nomsupath\gmatch("[^;]+")
                return true if raw_file_exists(nomsupath.."/"..path)
        return false
    -- Return 'true' if any files or directories are found, otherwise 'false', and yield every filename
    browse = (filename)->
        file_type = lfs.attributes(filename, 'mode')
        if file_type == 'file'
            if match(filename, "%.nom$") or match(filename, "%.lua$")
                coroutine.yield filename
                return true
        elseif file_type == 'directory'
            for subfile in lfs.dir(filename)
                -- Only include .nom files unless directly specified
                unless subfile == "." or subfile == ".." or not subfile\match("%.nom$")
                    browse(filename.."/"..subfile)
            return true
        elseif file_type == 'char device'
            coroutine.yield(filename)
            return true
        return false
    files.walk = (path)->
        if match(path, "%.nom$") or match(path, "%.lua$") or path == 'stdin'
            return iterate_single, path
        return coroutine.wrap ->
            if not browse(path) and package.nomsupath
                for nomsupath in package.nomsupath\gmatch("[^;]+")
                    break if browse(nomsupath.."/"..path)
            return nil
else
    if io.popen('find . -maxdepth 0')\close!
        error "Could not find 'luafilesystem' module and couldn't run system command `find` (this might happen on Windows). Please install `luafilesystem` (which can be found at: http://keplerproject.github.io/luafilesystem/ or `luarocks install luafilesystem`)", 0
    
    -- TODO: improve sanitization
    sanitize = (path)->
        path = gsub(path,"\\","\\\\")
        path = gsub(path,"`","")
        path = gsub(path,'"','\\"')
        path = gsub(path,"$","")
        return path

    files.exists = (path)->
        return true unless io.popen("ls #{sanitize(path)}")\close!
        if package.nomsupath
            for nomsupath in package.nomsupath\gmatch("[^;]+")
                return true unless io.popen("ls #{nomsupath}/#{sanitize(path)}")\close!
        return false

    files.walk = (path)->
        if match(path, "%.nom$") or match(path, "%.lua$") or path == 'stdin'
            return iterate_single, path
        path = sanitize(path)
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

line_counter = re.compile([[
    lines <- {| line (%nl line)* |}
    line <- {} (!%nl .)*
]], nl:lpeg.P("\r")^-1 * lpeg.P("\n"))

get_lines = re.compile([[
    lines <- {| line (%nl line)* |}
    line <- {[^%nl]*}
]], nl:lpeg.P("\r")^-1 * lpeg.P("\n"))

-- LINE_STARTS is a mapping from strings to a table that maps line number to character positions
_LINE_STARTS = {}
files.get_line_starts = (str)->
    if type(str) != 'string'
        str = tostring(str)
    if starts = _LINE_STARTS[str]
        return starts
    line_starts = line_counter\match(str)
    _LINE_STARTS[str] = line_starts
    return line_starts

files.get_line_number = (str, pos)->
    line_starts = files.get_line_starts(str)
    -- Binary search for line number of position
    lo, hi = 1, #line_starts
    while lo <= hi
        mid = math.floor((lo+hi)/2)
        if line_starts[mid] > pos
            hi = mid-1
        else lo = mid+1
    return hi

files.get_line = (str, line_no)->
    line_starts = files.get_line_starts(str)
    return str\sub(line_starts[line_no] or 1, (line_starts[line_no+1] or 1) - 2)

files.get_lines = (str)-> get_lines\match(str)

return files
