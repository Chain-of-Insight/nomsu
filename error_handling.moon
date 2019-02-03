-- This file contains the logic for making nicer error messages
debug_getinfo = debug.getinfo
Files = require "files"
C = require "colors"
pretty_error = require("pretty_errors")
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
        -- TODO: reduce duplicate code
        if map = SOURCE_MAP[info.source]
            if info.currentline
                info.currentline = assert(map[info.currentline])
            if info.linedefined
                info.linedefined = assert(map[info.linedefined])
            if info.lastlinedefined
                info.lastlinedefined = assert(map[info.lastlinedefined])
            info.short_src = info.source\match('@([^[]*)') or info.short_src
            info.name = if info.name
                "action '#{info.name\from_lua_id!}'"
            else "main chunk"
    return info

-- This uses a slightly modified Damerau-Levenshtein distance:
strdist = (a,b,cache={})->
    if a == b then return 0
    if #a < #b then a,b = b,a
    if b == "" then return #a
    k = a..'\003'..b
    unless cache[k]
        -- Insert, delete, substitute (given weight 1.1 as a heuristic)
        cache[k] = math.min(
            strdist(a\sub(1,-2),b,cache) + 1,
            strdist(a,b\sub(1,-2),cache) + 1,
            strdist(a\sub(1,-2),b\sub(1,-2),cache) + (a\sub(-1) ~= b\sub(-1) and 1.1 or 0)
        )
        -- Transposition:
        if #a >= 2 and #b >= 2 and a\sub(-1,-1) == b\sub(-2,-2) and a\sub(-2,-2) == b\sub(-1,-1)
            cache[k] = math.min(cache[k], strdist(a\sub(1,-3),b\sub(1,-3),cache) + 1)
    return cache[k]

enhance_error = (error_message)->
    -- Hacky: detect the line numbering
    unless error_message and error_message\match("%d|")
        error_message or= ""
        -- When calling 'nil' actions, make a better error message
        if fn_name = (error_message\match("attempt to call a nil value %(global '(.*)'%)") or
            error_message\match("attempt to call global '(.*)' %(a nil value%)"))

            action_name = fn_name\from_lua_id!
            error_message = "The action '#{action_name}' is not defined."

            -- Look for simple misspellings:

            -- This check is necessary for handling both top-level code and code inside a fn
            func = debug.getinfo(2,'f').func
            ename,env = debug.getupvalue(func, 1)
            unless ename == "_ENV" or ename == "_G"
                func = debug.getinfo(3,'f').func
                ename,env = debug.getupvalue(func, 1)

            THRESHOLD = math.min(4.5, .9*#action_name) -- Ignore matches with strdist > THRESHOLD
            candidates = {}
            cache = {}

            -- Locals:
            for i=1,99
                k, v = debug.getlocal(2, i)
                break if k == nil
                unless k\sub(1,1) == "(" or type(v) != 'function'
                    k = k\from_lua_id!
                    if strdist(k, action_name, cache) <= THRESHOLD and k != ""
                        table.insert candidates, k

            -- Upvalues:
            for i=1,debug.getinfo(func, 'u').nups
                k, v = debug.getupvalue(func, i)
                unless k\sub(1,1) == "(" or type(v) != 'function'
                    k = k\from_lua_id!
                    if strdist(k, action_name, cache) <= THRESHOLD and k != ""
                        table.insert candidates, k

            -- Globals and global compile rules:
            scan = (t, is_lua_id)->
                for k,v in pairs(t)
                    if type(k) == 'string' and type(v) == 'function'
                        k = k\from_lua_id! unless is_lua_id
                        if strdist(k, action_name, cache) <= THRESHOLD and k != ""
                            table.insert candidates, k
            scan env.COMPILE_RULES, true
            scan env.COMPILE_RULES._IMPORTS, true
            scan env
            scan env._IMPORTS

            if #candidates > 0
                for c in *candidates do THRESHOLD = math.min(THRESHOLD, strdist(c, action_name, cache))
                candidates = [c for c in *candidates when strdist(c, action_name, cache) <= THRESHOLD]
                --candidates = ["#{c}[#{strdist(c,action_name,cache)}/#{THRESHOLD}]" for c in *candidates]
                if #candidates == 1
                    error_message ..= "\n\x1b[3mSuggestion: Maybe you meant '#{candidates[1]}'? "
                elseif #candidates > 0
                    last = table.remove(candidates)
                    error_message ..= "\n"..C('italic', "Suggestion: Maybe you meant '#{table.concat candidates, "', '"}'#{#candidates > 1 and ',' or ''} or '#{last}'? ")

        level = 2
        while true
            -- TODO: reduce duplicate code
            calling_fn = debug_getinfo(level)
            if not calling_fn then break
            level += 1
            local filename, file, line_num
            if map = SOURCE_MAP and SOURCE_MAP[calling_fn.source]
                if calling_fn.currentline
                    line_num = assert(map[calling_fn.currentline])
                filename,start,stop = calling_fn.source\match('@([^[]*)%[([0-9]+):([0-9]+)]')
                if not filename
                    filename,start = calling_fn.source\match('@([^[]*)%[([0-9]+)]')
                assert(filename)
                file = Files.read(filename)
            else
                filename = calling_fn.short_src
                file = Files.read(filename)
                if calling_fn.short_src\match("%.moon$") and type(MOON_SOURCE_MAP[file]) == 'table'
                    char = MOON_SOURCE_MAP[file][calling_fn.currentline]
                    line_num = file\line_number_at(char)
                else
                    line_num = calling_fn.currentline

            if file and filename and line_num
                start = 1
                lines = file\lines!
                for i=1,line_num-1 do start += #lines[i] + 1
                stop = start + #lines[line_num]
                start += #lines[line_num]\match("^ *")
                error_message = pretty_error{
                    title:"Error"
                    error:error_message, source:file
                    start:start, stop:stop, filename:filename
                }
                break
            if calling_fn.func == xpcall then break


    ret = {
        C('bold red', error_message or "Error")
        "stack traceback:"
    }

    level = 2
    while true
        -- TODO: reduce duplicate code
        calling_fn = debug_getinfo(level)
        if not calling_fn then break
        if calling_fn.func == xpcall then break
        level += 1
        name = calling_fn.name and "function '#{calling_fn.name}'" or nil
        if calling_fn.linedefined == 0 then name = "main chunk"
        if name == "function 'run_lua_fn'" then continue
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
            name = if calling_fn.name
                "action '#{calling_fn.name\from_lua_id!}'"
            else "main chunk"

            file = Files.read(filename)
            lines = file and file\lines! or {}
            if err_line = lines[calling_fn.currentline]
                offending_statement = C('bright red', err_line\match("^[ ]*(.*)"))
                line = C('yellow', "#{filename}:#{calling_fn.currentline} in #{name}\n        #{offending_statement}")
            else
                line = C('yellow', "#{filename}:#{calling_fn.currentline} in #{name}")
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
                line_num = file\line_number_at(char)
                line = C('cyan', "#{calling_fn.short_src}:#{line_num} in #{name or '?'}")
            else
                line_num = calling_fn.currentline
                if calling_fn.short_src == '[C]'
                    line = C('green', "#{calling_fn.short_src} in #{name or '?'}")
                else
                    line = C('blue', "#{calling_fn.short_src}:#{calling_fn.currentline} in #{name or '?'}")

            if file
                if err_line = lines[line_num]
                    offending_statement = C('bright red', "#{err_line\match("^[ ]*(.*)$")}")
                    line ..= "\n        "..offending_statement
        table.insert ret, line
        if calling_fn.istailcall
            table.insert ret, C('dim', "      (...tail calls...)")

    return table.concat(ret, "\n")

guard = (fn)->
    ok, err = xpcall(fn, enhance_error)
    if not ok
        io.stderr\write err
        io.stderr\flush!
        os.exit 1

return {:guard, :enhance_error, :print_error}
