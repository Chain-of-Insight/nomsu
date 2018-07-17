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

-- `walk` returns an iterator over all files matching a path.
{:match, :gsub} = string

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

_browse_cache = {}
browse = (path)->
    unless _browse_cache[path]
        f = io.popen('find -L "'..package.nomsupath..'/'..path..'" -not -path "*/\\.*" -type f -name "*.nom"')
        _files = {line for line in f\lines!}
        _files = false unless f\close!
        _browse_cache[path] = _files
    return _browse_cache[path]

ok, lfs = pcall(require, "lfs")
if ok
    raw_file_exists = (filename)->
        mode = lfs.attributes(filename, 'mode')
        return if mode == 'file' or mode == 'directory' or mode == 'link' then true else false
    files.exists = (path)->
        return true if path == 'stdin' or raw_file_exists(path)
        if package.nomsupath
            for nomsupath in package.nomsupath\gmatch("[^;]+")
                return true if raw_file_exists(nomsupath.."/"..path)
        return false

    export browse
    browse = (filename)->
        unless _browse_cache[filename]
            _browse_cache[filename] = false
            file_type, err = lfs.attributes(filename, 'mode')
            if file_type == 'file'
                if match(filename, "%.nom$") or match(filename, "%.lua$")
                    _browse_cache[filename] = {filename}
            elseif file_type == 'char device'
                _browse_cache[filename] = {filename}
            elseif file_type == 'directory' or file_type == 'link'
                _files = {}
                for subfile in lfs.dir(filename)
                    unless subfile == "." or subfile == ".."
                        for f in *(browse(filename.."/"..subfile) or {})
                            _files[#_files+1] = f
                _browse_cache[filename] = _files
        return _browse_cache[filename]
else
    if io.popen('find . -maxdepth 0')\close!
        error "Could not find 'luafilesystem' module and couldn't run system command `find` (this might happen on Windows). Please install `luafilesystem` (which can be found at: http://keplerproject.github.io/luafilesystem/ or `luarocks install luafilesystem`)", 0

files.walk = (path, flush_cache=false)->
    if flush_cache
        export _browse_cache
        _browse_cache = {}
    _files = browse(path)
    if package.nomsupath and not _files
        for nomsupath in package.nomsupath\gmatch("[^;]+")
            if _files = browse(nomsupath.."/"..path) then break
    iter = (_files, i)->
        i += 1
        if f = _files[i]
            return i, f
    return iter, _files, 0

line_counter = re.compile([[
    lines <- {| line (%nl line)* |}
    line <- {} (!%nl .)*
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

log = {}
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

get_lines = re.compile([[
    lines <- {| line (%nl line)* |}
    line <- {[^%nl]*}
]], nl:lpeg.P("\r")^-1 * lpeg.P("\n"))

files.get_lines = (str)-> get_lines\match(str)

return files
