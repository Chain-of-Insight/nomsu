{:insert, :remove, :concat} = table
immutable = require 'immutable'
local Lua, Source
export LINE_STARTS

Source = immutable {"filename","start","stop"}, {
    name:"Source"
    __new: (filename, start, stop)=>
        if stop then assert(start <= stop, "Invalid range: #{start}, #{stop}")
        return filename, start, stop
    __tostring: =>
        if @stop
            "Source(\"#{@filename}\", #{@start}, #{@stop})"
        else
            "Source(\"#{@filename}\", #{@start})"
    __lt: (other)=>
        assert(@filename == other.filename, "Cannot compare sources from different files")
        return if @start == other.start
            (@stop or @start) < (other.stop or other.start)
        else @start < other.start
    __le: (other)=>
        assert(@filename == other.filename, "Cannot compare sources from different files")
        return if @start == other.start
            (@stop or @start) <= (other.stop or other.start)
        else @start <= other.start
    __add: (offset)=>
        if type(self) == 'number'
            offset, self = self, offset
        else assert(type(offset) == 'number', "Cannot add Source and #{type(offset)}")
        return Source(@filename, @start+offset, @stop)
    sub: (start, stop)=>
        if not @stop
            assert(not stop, "cannot subscript non-range with range")
            assert(start > 0, "cannot subscript non-range with negative index")
            return Source(@filename, @start + (start or 0))
        else
            start or= 1
            if start < 0 then start = @stop + start + 1
            stop or= -1
            if stop < 0 then stop = @stop + stop + 1
            return Source(@filename, @start + start - 1, @start + stop - 1)
    get_text: =>
        FILE_CACHE[@filename]\sub(@start,@stop)
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

class Code
    new: (@source, ...)=>
        @bits = {...}
        if type(@source) == 'string'
            filename,start,stop = @source\match("^(.-)[(%d+):(%d+)]$")
            if start or stop
                @source = Source(filename, tonumber(start), tonumber(stop))
            else
                @source = Source(@source, 1, #self)

    clone: =>
        cls = @__class
        copy = cls(@source, unpack(@bits))
        copy.is_value = @is_value
        for k,v in pairs @free_vars
            copy.free_vars[k] = v
        return copy

    __tostring: =>
        buff = {}
        for i,b in ipairs @bits
            buff[#buff+1] = tostring(b)
        ret = concat(buff, "")
        return ret

    __len: =>
        len = 0
        for b in *@bits
            len += #b
        return len
    
    sub: (start,stop)=>
        str = tostring(self)\sub(start,stop)
        cls = @__class
        return cls(@source\sub(start-@source.start+1,stop-@source.start+1), str)

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

class Lua extends Code
    new: (...)=>
        super ...
        @free_vars = {}
        @is_value = false
    
    @Value = (...)->
        lua = Lua(...)
        lua.is_value = true
        return lua

    add_free_vars: (...)=>
        seen = {[v]:true for v in *@free_vars}
        for i=1,select("#",...)
            var = select(i, ...)
            if type(var) == 'userdata' and var.type == "Var"
                var = tostring(var\as_lua!)
            elseif type(var) != 'string'
                var = tostring(var)
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
        to_declare = {}
        walk = =>
            for var in *@free_vars
                unless skip[var]
                    skip[var] = true
                    to_declare[#to_declare+1] = var
            for bit in *@bits
                if bit.__class == Lua
                    walk bit
            if #@free_vars > 0
                @free_vars = {}
        walk self
        if #to_declare > 0
            @prepend "local #{concat to_declare, ", "};\n"
        [[
        if #@free_vars > 0
            @prepend "local #{concat @free_vars, ", "};\n"
            for var in *@free_vars do skip[var] = true
            @free_vars = {}
        for bit in *@bits
            if bit.__class == Lua
                bit\declare_locals(skip)
                    ]]

    __tostring: =>
        buff = {}
        for i,b in ipairs @bits
            buff[#buff+1] = tostring(b)
        ret = concat(buff, "")
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
            bit = select(i, ...)
            bits[#bits+1] = bit
            if type(bit) != 'string' and not bit.is_value and #@bits > 0
                bits[#bits+1] = "\n"
    
    prepend: (...)=>
        n = select("#",...)
        bits = @bits
        insert_index = 1
        -- TODO: optimize?
        for i=1,n
            bit = select(i, ...)
            insert bits, insert_index, bit
            insert_index += 1
            if type(bit) != 'string' and not bit.is_value and insert_index < #@bits + 1
                insert bits, insert_index, "\n"
                insert_index += 1

    make_offset_table: =>
        -- Return a mapping from output (lua) character number to input (nomsu) character number
        lua_chunkname = tostring(@source)..".lua"
        lua_str = tostring(self)
        metadata = {
            nomsu_filename:@source.filename
            lua_filename:lua_chunkname, lua_file:lua_str
            lua_to_nomsu: {}, nomsu_to_lua: {}
        }
        walk = (lua, output_range)->
            pos = 1
            for b in *lua.bits
                if type(b) == 'string'
                    output = output_range\sub(pos, pos+#b)
                    metadata.lua_to_nomsu[output] = lua.source
                    metadata.nomsu_to_lua[lua.source] = output
                else
                    walk b, output_range\sub(pos, pos+#b)
                pos += #b
        
        walk self, Source(lua_chunkname, 1, #lua_str)
        return lua_str, metadata

    parenthesize: =>
        if @is_value
            @prepend "("
            @append ")"
        else
            error "Cannot parenthesize lua statements"

class Nomsu extends Code
    __tostring: =>
        buff = {}
        for i,b in ipairs @bits
            buff[#buff+1] = tostring(b)
        ret = concat(buff, "")
        return ret

    __len: =>
        len = 0
        for b in *@bits
            len += #b
        return len

return {:Code, :Nomsu, :Lua, :Source}
