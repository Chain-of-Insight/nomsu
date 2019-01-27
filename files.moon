-- Some file utilities for searching for files recursively and using package.nomsupath
lpeg = require 'lpeg'
re = require 're'
Files = {}

run_cmd = (cmd)->
    f = io.popen(cmd..' 2>/dev/null')
    lines = [line for line in f\lines!]
    return nil unless f\close!
    return lines
        
_SPOOFED_FILES = {}
_BROWSE_CACHE = {}

-- Create a fake file and put it in the cache
_anon_number = 0
Files.spoof = (filename, contents)->
    if not contents
        filename, contents = "<anonymous file ##{_anon_number}>", filename
        _anon_number += 1
    _SPOOFED_FILES[filename] = contents
    return filename

-- Read a file's contents
Files.read = (filename)->
    if contents = _SPOOFED_FILES[filename]
        return contents
    if filename == 'stdin' or filename == '-'
        contents = io.read('*a')
        Files.spoof('stdin', contents)
        Files.spoof('-', contents)
        return contents
    file = io.open(filename)
    return nil unless file
    contents = file\read("*a")
    file\close!
    return contents or nil

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
    return true if path == 'stdin' or path == '-'
    return true if run_cmd("ls #{sanitize(path)}")
    return false

Files.list = (path)->
    unless _BROWSE_CACHE[path]
        local files
        _BROWSE_CACHE[path] = if _SPOOFED_FILES[path] or path == 'stdin' or path == '-'
            {path}
        else run_cmd('find -L "'..path..'" -not -path "*/\\.*" -type f') or false
    return _BROWSE_CACHE[path]

Files.make_directory = (path)->
    run_cmd('mkdir '..path)

ok, lfs = pcall(require, "lfs")
if ok
    raw_file_exists = (filename)->
        mode = lfs.attributes(filename, 'mode')
        return if mode == 'file' or mode == 'directory' or mode == 'link' then true else false
    Files.exists = (path)->
        return true if _SPOOFED_FILES[path]
        return true if path == 'stdin' or path == '-' or raw_file_exists(path)
        return false

    Files.list = (path)->
        unless _BROWSE_CACHE[path]
            _BROWSE_CACHE[path] = if _SPOOFED_FILES[path] or path == 'stdin' or path == '-'
                {path}
            else
                file_type, err = lfs.attributes(path, 'mode')
                switch file_type
                    when "file", "char device"
                        {path}
                    when "directory", "link"
                        files = {}
                        for subfile in lfs.dir(path)
                            continue if subfile == "." or subfile == ".."
                            for f in *(Files.list(path.."/"..subfile) or {})
                                files[#files+1] = f
                        files
                    else false
            -- Filter out any "./" prefix to standardize
            if _BROWSE_CACHE[path]
                for i,f in ipairs(_BROWSE_CACHE[path])
                    if f\match("^%./") then _BROWSE_CACHE[path][i] = f\sub(3)
        return _BROWSE_CACHE[path]

    Files.make_directory = lfs.mkdir
else
    unless run_cmd('find . -maxdepth 0')
        url = if jit
            'https://github.com/spacewander/luafilesystem'
        else 'https://github.com/keplerproject/luafilesystem'
        error "Could not find 'luafilesystem' module and couldn't run system command `find` (this might happen on Windows). Please install `luafilesystem` (which can be found at: #{url} or `luarocks install luafilesystem`)\n#{lfs}\npackage.cpath: #{package.cpath}", 0

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
    return (str\sub(start, stop - 2))

return Files
