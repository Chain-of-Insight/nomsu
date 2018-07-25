-- Some file utilities for searching for files recursively and using package.nomsupath
lpeg = require 'lpeg'
re = require 're'
Files = {}
assert package.nomsupath, "No package.nomsupath was found"

run_cmd = (cmd)->
    f = io.popen(cmd)
    lines = [line for line in f\lines!]
    return nil unless f\close!
    return lines
        

_SPOOFED_FILES = {}
_FILE_CACHE = setmetatable {}, __index:_SPOOFED_FILES
_BROWSE_CACHE = {}

-- Create a fake file and put it in the cache
Files.spoof = (filename, contents)->
    _SPOOFED_FILES[filename] = contents
    return contents

-- Read a file's contents (searching first locally, then in the nomsupath)
Files.read = (filename)->
    if file_contents = _FILE_CACHE[filename]
        return file_contents
    if filename == 'stdin'
        return Files.spoof('stdin', io.read('*a'))
    file = io.open(filename)
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

Files.exists = (path)->
    return true if _SPOOFED_FILES[path]
    return true if path == 'stdin'
    return true if run_cmd("ls #{sanitize(path)}")
    for nomsupath in package.nomsupath\gmatch("[^;]+")
        return true if run_cmd("ls #{nomsupath}/#{sanitize(path)}")
    return false

browse = (path)->
    unless _BROWSE_CACHE[path]
        local files
        _BROWSE_CACHE[path] = if _SPOOFED_FILES[path]
            {path}
        else run_cmd('find -L "'..path..'" -not -path "*/\\.*" -type f') or false
    return _BROWSE_CACHE[path]

ok, lfs = pcall(require, "lfs")
if ok
    raw_file_exists = (filename)->
        mode = lfs.attributes(filename, 'mode')
        return if mode == 'file' or mode == 'directory' or mode == 'link' then true else false
    Files.exists = (path)->
        return true if _SPOOFED_FILES[path]
        return true if path == 'stdin' or raw_file_exists(path)
        for nomsupath in package.nomsupath\gmatch("[^;]+")
            return true if raw_file_exists(nomsupath.."/"..path)
        return false

    export browse
    browse = (filename)->
        unless _BROWSE_CACHE[filename]
            _BROWSE_CACHE[filename] = if _SPOOFED_FILES[filename] or filename == 'stdin'
                {filename}
            else
                file_type, err = lfs.attributes(filename, 'mode')
                switch file_type
                    when "file", "char device"
                        {filename}
                    when "directory", "link"
                        files = {}
                        for subfile in lfs.dir(filename)
                            continue if subfile == "." or subfile == ".."
                            for f in *(browse(filename.."/"..subfile) or {})
                                files[#files+1] = f
                        files
                    else false
        return _BROWSE_CACHE[filename]
else
    unless run_cmd('find . -maxdepth 0')
        url = if jit
            'https://github.com/spacewander/luafilesystem'
        else 'https://github.com/keplerproject/luafilesystem'
        error "Could not find 'luafilesystem' module and couldn't run system command `find` (this might happen on Windows). Please install `luafilesystem` (which can be found at: #{url} or `luarocks install luafilesystem`)", 0

Files.walk = (path, flush_cache=false)->
    if flush_cache
        export _BROWSE_CACHE
        _BROWSE_CACHE = {}
    local files
    if path == 'stdin'
        files = {path}
    else
        for nomsupath in package.nomsupath\gmatch("[^;]+")
            if files = browse(nomsupath.."/"..path) then break
    iter = (files, i)->
        return unless files
        i += 1
        if f = files[i]
            return i, f
    return iter, files, 0

line_counter = re.compile([[
    lines <- {| line (%nl line)* |}
    line <- {} (!%nl .)*
]], nl:lpeg.P("\r")^-1 * lpeg.P("\n"))

-- LINE_STARTS is a mapping from strings to a table that maps line number to character positions
_LINE_STARTS = {}
Files.get_line_starts = (str)->
    if type(str) != 'string'
        str = tostring(str)
    if starts = _LINE_STARTS[str]
        return starts
    line_starts = line_counter\match(str)
    _LINE_STARTS[str] = line_starts
    return line_starts

log = {}
Files.get_line_number = (str, pos)->
    line_starts = Files.get_line_starts(str)
    -- Binary search for line number of position
    lo, hi = 1, #line_starts
    while lo <= hi
        mid = math.floor((lo+hi)/2)
        if line_starts[mid] > pos
            hi = mid-1
        else lo = mid+1
    return hi

Files.get_line = (str, line_no)->
    line_starts = Files.get_line_starts(str)
    start = line_starts[line_no]
    return unless start
    stop = line_starts[line_no+1]
    return unless stop
    return str\sub(start, stop - 2)

get_lines = re.compile([[
    lines <- {| line (%nl line)* |}
    line <- {[^%nl]*}
]], nl:lpeg.P("\r")^-1 * lpeg.P("\n"))

Files.get_lines = (str)-> get_lines\match(str)

return Files
