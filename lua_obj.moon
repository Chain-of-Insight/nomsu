{:insert, :remove, :concat} = table
immutable = require 'immutable'
local Lua, LuaValue, Location
export LINE_STARTS

Location = immutable {"filename","start","stop"}, {
    name:"Location"
    __new: (filename, start, stop)=>
        assert(type(filename) == 'string' and type(start) == 'number' and type(stop) == 'number')
        return filename, start, stop or start
    __tostring: => "Location(\"#{@filename}\", #{@start}, #{@stop})"
    __lt: (other)=>
        assert(@filename == other.filename, "Cannot compare sources from different files")
        return if @start == other.start
            @stop < other.stop
        else @start < other.start
    __le: (other)=>
        assert(@filename == other.filename, "Cannot compare sources from different files")
        return if @start == other.start
            @stop <= other.stop
        else @start <= other.start
    get_text: => FILE_CACHE[@filename]\sub(@start,@stop)
    get_line_number: =>
        -- TODO: do a binary search if this is actually slow, which I doubt
        line_starts = LINE_STARTS[FILE_CACHE[@filename]]
        start_line = 1
        while (line_starts[start_line+1] or (#src+1)) <= @start
            start_line += 1
        stop_line = start_line
        while (line_starts[stop_line+1] or (#src+1)) <= @stop
            stop_line += 1
        return start_line, stop_line
    get_line: => "#{@filename}:#{@get_line_number!}"
    get_line_range: =>
        start_line, stop_line = @get_line_number!
        return if stop_line == start_line
            "#{@filename}:#{start_line}"
        else "#{@filename}:#{start_line}-#{stop_line}"
}

class Lua
    is_statement: true
    is_value: false

    new: (@source, ...)=>
        if type(@source) == 'string'
            filename,start,stop = @source\match("^(.-)[(%d+):(%d+)]$")
            @source = Location(filename, tonumber(start), tonumber(stop))
        for i=1,select("#",...)
            x = select(i,...)
            assert(type(x) != 'table' or getmetatable(x))
        @bits = {...}
        @free_vars = {}
    
    add_free_vars: (free_vars)=>
        seen = {[v]:true for v in *@free_vars}
        for var in *free_vars
            unless seen[var]
                @free_vars[#@free_vars+1] = var
                seen[var] = true

    as_statements: => self

    declare_locals: (skip={})=>
        if next(skip) == 1
            skip = {[s]:true for s in *skip}
        if #@free_vars > 0
            @prepend "local #{concat @free_vars, ", "};\n"
        for var in *@free_vars do skip[var] = true
        for bit in *@bits
            if type(bit) == Lua
                bit\declare_locals(skip)

    __tostring: =>
        buff = {}
        for i,b in ipairs @bits
            buff[#buff+1] = tostring(b)
            if i < #@bits and type(b) != 'string' and b.is_statement
                buff[#buff+1] = "\n"
        ret = concat(buff, "")
        assert(not ret\match(".*table: 0x.*"))
        return ret

    __len: =>
        len = 0
        for b in *@bits
            len += #b
        return len
    
    append: (...)=>
        n = select("#",...)
        bits = @bits
        for i=1,n
            x = select(i,...)
            assert(type(x) != 'table' or getmetatable(x))
            bits[#bits+1] = select(i, ...)
    
    prepend: (...)=>
        n = select("#",...)
        bits = @bits
        for i=#bits+n,n+1,-1
            x = select(i,...)
            assert(type(x) != 'table' or getmetatable(x))
            bits[i] = bits[i-n]
        for i=1,n
            x = select(i,...)
            assert(type(x) != 'table' or getmetatable(x))
            bits[i] = select(i, ...)

    make_offset_table: (lua_chunkname)=>
        -- Return a mapping from output (lua) character number to input (nomsu) character number
        lua_str = tostring(self)
        metadata = {
            nomsu_filename:@source.filename
            lua_filename:lua_chunkname, lua_file:lua_str
            lua_to_nomsu: {}, nomsu_to_lua: {}
        }
        lua_offset = 1
        walk = (lua)->
            if type(lua) == 'string'
                lua_offset += #lua
            else
                lua_start = lua_offset
                for b in *lua.bits
                    walk b
                lua_stop = lua_offset
                nomsu_src, lua_src = lua.source, Location(lua_chunkname, lua_start, lua_stop)
                metadata.lua_to_nomsu[lua_src] = nomsu_src
                metadata.nomsu_to_lua[nomsu_src] = lua_src
        walk self
        return lua_str, metadata

class LuaValue extends Lua
    is_statement: false
    is_value: true

    new: (@source, ...)=>
        @bits = {...}

    __tostring: =>
        buff = {}
        for b in *@bits
            buff[#buff+1] = tostring(b)
        ret = concat(buff, "")
        assert(not ret\match(".*table: 0x.*"))
        return ret

    as_statements: (prefix="", suffix=";")=>
        bits = {prefix, unpack @bits}
        bits[#bits+1] = suffix
        return Lua(@source, unpack(bits))

    parenthesize: =>
        @prepend "("
        @append ")"

return {:Lua, :LuaValue, :Location}
