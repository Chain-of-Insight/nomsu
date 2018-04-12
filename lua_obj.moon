{:insert, :remove, :concat} = table
immutable = require 'immutable'
local Lua, LuaValue, Location
export LINE_STARTS

Location = immutable {"text_name","text","start","stop"}, {
    name:"Location"
    __new: (text_name, text, start, stop)=> text_name, text, start, stop or start
    __tostring: => "#{@text_name}[#{@start}:#{@stop}]"
    __lt: (other)=>
        assert(@text == other.text, "Cannot compare sources from different texts")
        return if @start == other.start
            @stop < other.stop
        else @start < other.start
    __le: (other)=>
        assert(@text == other.text, "Cannot compare sources from different texts")
        return if @start == other.start
            @stop <= other.stop
        else @start <= other.start
    get_text: => @text\sub(@start,@stop)
    get_line_number: =>
        -- TODO: do a binary search if this is actually slow, which I doubt
        line_starts = LINE_STARTS[@text]
        start_line = 1
        while (line_starts[start_line+1] or (#src+1)) <= @start
            start_line += 1
        stop_line = start_line
        while (line_starts[stop_line+1] or (#src+1)) <= @stop
            stop_line += 1
        return start_line, stop_line
    get_line: => "#{@text_name}:#{@get_line_number}"
    get_line_range: =>
        start_line, stop_line = @get_line_number
        return if stop_line == start_line
            "#{text_name}:#{start_line}"
        else "#{text_name}:#{start_line}-#{stop_line}"
}

class Lua
    is_statement: true
    is_value: false

    new: (@source, ...)=>
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
        @prepend "local #{concat @free_vars, ", "};\n"
        for var in *@free_vars do skip[var] = true
        for bit in *@bits
            if type(bit) == Lua
                bit\declare_locals(skip)

    __tostring: =>
        buff = {}
        for b in *@bits
            buff[#buff+1] = tostring(b)
        return concat(buff, "")

    __len: =>
        len = 0
        for b in *@bits
            len += #b
        return len
    
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
            nomsu_filename:@source.text_name, nomsu_file:@source.text,
            lua_filename:lua_chunkname, lua_file:lua_str
            lua_to_nomsu: {}, nomsu_to_lua: {}
        }
        metadata, lua_offset = {}, 1
        lua_offset = 1
        walk = (lua)->
            if type(lua) == 'string'
                lua_offset += #lua
            else
                lua_start = lua_offset
                for b in *lua.bits
                    walk b
                lua_stop = lua_offset
                nomsu_src, lua_src = lua.souce, Location(lua_chunkname, lua_str, lua_start, lua_stop)
                metadata.lua_to_nomsu[lua_src] = nomsu_src
                metadata.nomsu_to_lua[nomsu_src] = lua_src
        walk self
        return lua_str, metadata

class LuaValue extends Lua
    is_statement: false
    is_value: true

    new: (@source, ...)=>
        @bits = {...}

    as_statements: =>
        bits = {unpack @bits}
        bits[#bits+1] = ";"
        return Lua(@source, bits)

    parenthesize: =>
        @prepend "("
        @append ")"

return {:Lua, :LuaValue, :Location}
