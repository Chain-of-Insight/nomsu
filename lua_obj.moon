{:insert, :remove, :concat} = table
immutable = require 'immutable'
local Lua, Location
export LINE_STARTS

Location = immutable {"filename","start","stop"}, {
    name:"Location"
    __new: (filename, start, stop)=>
        --assert(type(filename) == 'string' and type(start) == 'number' and type(stop) == 'number')
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
        src = FILE_CACHE[@filename]
        line_starts = LINE_STARTS[src]
        start_line = 1
        while (line_starts[start_line+1] or math.huge) <= @start
            start_line += 1
        stop_line = start_line
        while (line_starts[stop_line+1] or math.huge) <= @stop
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
    new: (@source, ...)=>
        if type(@source) == 'string'
            filename,start,stop = @source\match("^(.-)[(%d+):(%d+)]$")
            @source = Location(filename, tonumber(start), tonumber(stop))
        @bits = {...}
        @free_vars = {}
        @is_value = false
    
    @Value = (...)->
        lua = Lua(...)
        lua.is_value = true
        return lua

    clone: =>
        copy = Lua(@source, {unpack(@bits)})
        copy.is_value = @is_value
        for k,v in pairs @free_vars
            copy.free_vars[k] = v
        return copy
    
    add_free_vars: (...)=>
        seen = {[v]:true for v in *@free_vars}
        for i=1,select("#",...)
            var = select(i, ...)
            unless seen[var]
                @free_vars[#@free_vars+1] = var
                seen[var] = true

    convert_to_statements: (prefix="", suffix=";")=>
        unless @is_value
            return
        if prefix != ""
            @prepend prefix
        if suffix != ""
            @append suffix

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
            if i < #@bits and type(b) != 'string' and not b.is_value
                buff[#buff+1] = "\n"
        ret = concat(buff, "")
        return ret

    __len: =>
        len = 0
        for b in *@bits
            len += #b
        return len
    
    __add: (other)=>
        Lua(nil, self, other)
    
    __concat: (other)=>
        Lua(nil, self, other)

    append: (...)=>
        n = select("#",...)
        bits = @bits
        for i=1,n
            bits[#bits+1] = select(i, ...)
    
    prepend: (...)=>
        n = select("#",...)
        bits = @bits
        for i=#bits+n,n+1,-1
            bits[i] = bits[i-n]
        for i=1,n
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

    parenthesize: =>
        if @is_value
            @prepend "("
            @append ")"
        else
            error "Cannot parenthesize lua statements"

return {:Lua, :Location}
