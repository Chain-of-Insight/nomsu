-- This file contains the logic for making nicer error messages
debug_getinfo = debug.getinfo
Files = require "files"
export SOURCE_MAP

RED = "\027[31m"
BRIGHT_RED = "\027[31;1m"
RESET = "\027[0m"
YELLOW = "\027[33m"
CYAN = "\027[36m"
GREEN = "\027[32m"
BLUE = "\027[34m"
DIM = "\027[37;2m"

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
            -- TODO: get name properly
            info.name = if info.name
                if tmp = info.name\match("^A_([a-zA-Z0-9_]*)$")
                    tmp\gsub("_"," ")\gsub("x([0-9A-F][0-9A-F])", => string.char(tonumber(@, 16)))
                else info.name
            else "main chunk"
    return info

print_error = (error_message, start_fn, stop_fn)->
    io.stderr\write("#{RED}ERROR: #{BRIGHT_RED}#{error_message or ""}#{RESET}\n")
    io.stderr\write("stack traceback:\n")

    level = 1
    found_start = false
    while true
        -- TODO: reduce duplicate code
        calling_fn = debug_getinfo(level)
        if not calling_fn then break
        level += 1
        unless found_start
            if calling_fn.func == start_fn then found_start = true
            continue
        name = calling_fn.name and "function '#{calling_fn.name}'" or nil
        if calling_fn.linedefined == 0 then name = "main chunk"
        if name == "run_lua_fn" then continue
        line = nil
        if map = SOURCE_MAP and SOURCE_MAP[calling_fn.source]
            if calling_fn.currentline
                calling_fn.currentline = assert(map[calling_fn.currentline])
            if calling_fn.linedefined
                calling_fn.linedefined = assert(map[calling_fn.linedefined])
            if calling_fn.lastlinedefined
                calling_fn.lastlinedefined = assert(map[calling_fn.lastlinedefined])
            --calling_fn.short_src = calling_fn.source\match('"([^[]*)')
            filename,start,stop = calling_fn.source\match('@([^[]*)%[([0-9]+):([0-9]+)]')
            if not filename
                filename,start = calling_fn.source\match('@([^[]*)%[([0-9]+)]')
            assert(filename)
            -- TODO: get name properly
            name = if calling_fn.name
                if tmp = calling_fn.name\match("^A_([a-zA-Z0-9_]*)$")
                    "action '#{tmp\gsub("_"," ")\gsub("x([0-9A-F][0-9A-F])", => string.char(tonumber(@, 16)))}'"
                else "action '#{calling_fn.name}'"
            else "main chunk"

            file = Files.read(filename)
            lines = file and file\lines! or {}
            if err_line = lines[calling_fn.currentline]
                offending_statement = "#{BRIGHT_RED}#{err_line\match("^[ ]*(.*)")}#{RESET}"
                line = "#{YELLOW}#{filename}:#{calling_fn.currentline} in #{name}\n        #{offending_statement}#{RESET}"
            else
                line = "#{YELLOW}#{filename}:#{calling_fn.currentline} in #{name}#{RESET}"
        else
            local line_num
            if name == nil
                search_level = level
                _info = debug.getinfo(search_level)
                while true
                    search_level += 1
                    _info = debug.getinfo(search_level)
                    break unless _info
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

            local file, lines
            if file = Files.read(calling_fn.short_src)
                lines = file\lines!
            
            if file and (calling_fn.short_src\match("%.moon$") or file\match("^#![^\n]*moon\n")) and type(MOON_SOURCE_MAP[file]) == 'table'
                char = MOON_SOURCE_MAP[file][calling_fn.currentline]
                line_num = 1
                for _ in file\sub(1,char)\gmatch("\n") do line_num += 1
                line = "#{CYAN}#{calling_fn.short_src}:#{line_num} in #{name or '?'}#{RESET}"
            else
                line_num = calling_fn.currentline
                if calling_fn.short_src == '[C]'
                    line = "#{GREEN}#{calling_fn.short_src} in #{name or '?'}#{RESET}"
                else
                    line = "#{BLUE}#{calling_fn.short_src}:#{calling_fn.currentline} in #{name or '?'}#{RESET}"

            if file
                if err_line = lines[line_num]
                    offending_statement = "#{BRIGHT_RED}#{err_line\match("^[ ]*(.*)$")}#{RESET}"
                    line ..= "\n        "..offending_statement
        io.stderr\write(line,"\n")
        if calling_fn.istailcall
            io.stderr\write("      #{DIM}(...tail calls...)#{RESET}\n")
        if calling_fn.func == stop_fn then break

    io.stderr\flush!

guard = (fn)->
    error_handler = (error_message)->
        print_error error_message, error_handler, fn
        EXIT_FAILURE = 1
        os.exit(EXIT_FAILURE)
    xpcall(fn, error_handler)

return {:guard, :print_error}
