-- This file contains the logic for making nicer error messages
files = require "files"
debug_getinfo = debug.getinfo
export SOURCE_MAP

ok, to_lua = pcall -> require('moonscript.base').to_lua
if not ok then to_lua = -> nil
MOON_SOURCE_MAP = setmetatable {},
    __index: (file)=>
        _, line_table = to_lua(file)
        self[file] = line_table or false
        return line_table or false

-- Make a better version of debug.getinfo that provides info about the original source
-- where the error came from, even if that's in another language.
debug.getinfo = (thread,f,what)->
    if what == nil
        f,what,thread = thread,f,nil
    if type(f) == 'number' then f += 1 -- Account for this wrapper function
    info = if thread == nil
        debug_getinfo(f,what)
    else debug_getinfo(thread,f,what)
    if not info or not info.func then return info
    if info.short_src or info.source or info.linedefine or info.currentline
        -- TODO: get name properly
        if map = SOURCE_MAP[info.source]
            if info.currentline
                info.currentline = assert(map[info.currentline])
            if info.linedefined
                info.linedefined = assert(map[info.linedefined])
            if info.lastlinedefined
                info.lastlinedefined = assert(map[info.lastlinedefined])
            info.short_src = info.source\match('@([^[]*)') or info.short_src
    return info

print_error = (error_message, stack_offset=3)->
    io.stderr\write("#{colored.red "ERROR:"} #{colored.bright colored.red (error_message or "")}\n")
    io.stderr\write("stack traceback:\n")

    get_line = (file, line_no)->
        start = LINE_STARTS[file][line_no] or 1
        stop = (LINE_STARTS[file][line_no+1] or 0) - 1
        return file\sub(start, stop)

    level = stack_offset
    while true
        -- TODO: reduce duplicate code
        calling_fn = debug_getinfo(level)
        if not calling_fn then break
        if calling_fn.func == run then break
        level += 1
        name = calling_fn.name and "function '#{calling_fn.name}'" or nil
        if calling_fn.linedefined == 0 then name = "main chunk"
        if name == "run_lua_fn" then continue
        line = nil
        if map = SOURCE_MAP[calling_fn.source]
            if calling_fn.currentline
                calling_fn.currentline = assert(map[calling_fn.currentline])
            if calling_fn.linedefined
                calling_fn.linedefined = assert(map[calling_fn.linedefined])
            if calling_fn.lastlinedefined
                calling_fn.lastlinedefined = assert(map[calling_fn.lastlinedefined])
            --calling_fn.short_src = calling_fn.source\match('"([^[]*)')
            filename,start,stop = calling_fn.source\match('@([^[]*)%[([0-9]+):([0-9]+)]')
            assert(filename)
            file = read_file(filename)\sub(tonumber(start),tonumber(stop))
            err_line = get_line(file, calling_fn.currentline)\sub(1,-2)
            offending_statement = colored.bright(colored.red(err_line\match("^[ ]*(.*)")))
            -- TODO: get name properly
            name = if calling_fn.name
                if tmp = calling_fn.name\match("^A_([a-zA-Z0-9_]*)$")
                    "action '#{tmp\gsub("_"," ")\gsub("x([0-9A-F][0-9A-F])", => string.char(tonumber(@, 16)))}'"
                else "action '#{calling_fn.name}'"
            else "main chunk"
            line = colored.yellow("#{filename}:#{calling_fn.currentline} in #{name}\n        #{offending_statement}")
        else
            ok, file = pcall ->read_file(calling_fn.short_src)
            if not ok then file = nil
            local line_num
            if name == nil
                search_level = level
                _info = debug.getinfo(search_level)
                while _info and (_info.func == pcall or _info.func == xpcall)
                    search_level += 1
                    _info = debug.getinfo(search_level)
                if _info
                    for i=1,999
                        varname, val = debug.getlocal(search_level, i)
                        if not varname then break
                        if val == calling_fn.func
                            name = "local '#{varname}'"
                            if not varname\match("%(")
                                break
                    unless name
                        for i=1,_info.nups
                            varname, val = debug.getupvalue(_info.func, i)
                            if not varname then break
                            if val == calling_fn.func
                                name = "upvalue '#{varname}'"
                                if not varname\match("%(")
                                    break
            
            if file and (calling_fn.short_src\match("%.moon$") or file\match("^#![^\n]*moon\n")) and type(MOON_SOURCE_MAP[file]) == 'table'
                char = MOON_SOURCE_MAP[file][calling_fn.currentline]
                line_num = 1
                for _ in file\sub(1,char)\gmatch("\n") do line_num += 1
                line = colored.cyan("#{calling_fn.short_src}:#{line_num} in #{name or '?'}")
            else
                line_num = calling_fn.currentline
                if calling_fn.short_src == '[C]'
                    line = colored.green("#{calling_fn.short_src} in #{name or '?'}")
                else
                    line = colored.blue("#{calling_fn.short_src}:#{calling_fn.currentline} in #{name or '?'}")

            if file
                err_line = get_line(file, line_num)\sub(1,-2)
                offending_statement = colored.bright(colored.red(err_line\match("^[ ]*(.*)$")))
                line ..= "\n        "..offending_statement
        io.stderr\write("    #{line}\n")
        if calling_fn.istailcall
            io.stderr\write("    #{colored.dim colored.white "  (...tail calls...)"}\n")

    io.stderr\flush!

error_handler = (error_message)->
    print_error error_message
    EXIT_FAILURE = 1
    os.exit(EXIT_FAILURE)

run_safely = (fn)->
    xpcall(fn, error_handler)

return {:run_safely, :print_error, :error_handler}
