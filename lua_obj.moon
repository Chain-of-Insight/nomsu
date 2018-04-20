{:insert, :remove, :concat} = table
immutable = require 'immutable'
local Lua, Source
export LINE_STARTS

Source = immutable {"filename","start","stop"}, {
    name:"Source"
    __new: (filename, start, stop)=>
        if not start
            start, stop = 1, #FILE_CACHE[filename]
        if stop then assert(start <= stop, "Invalid range: #{start}, #{stop}")
        return filename, start, stop
    __tostring: =>
        if @stop
            "\"#{@filename}[#{@start}:#{@stop}]\""
        else
            "\"#{@filename}[#{@start}]\""
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
        start or= 1
        assert(start > 0 and (stop == nil or stop > 0), "Negative subscripts not supported")
        if not @stop
            assert(not stop, "cannot subscript non-range with range")
            return Source(@filename, @start + start - 1)
        else
            stop or= @stop
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
            filename,start,stop = @source\match("^(.-)%[(%d+):(%d+)%]$")
            unless filename
                filename,start = @source\match("^(.-)%[(%d+)%]$")
            if start or stop
                @source = Source(filename, tonumber(start), tonumber(stop))
            else
                @source = Source(@source, 1, #self+1)
        assert(@source == nil or Source\is_instance(@source))
        for b in *@bits
            assert(not Source\is_instance(b))

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
        return cls(@source\sub(start,stop), str)

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
        @__str = nil
    
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
        @__str = nil
    
    remove_free_vars: (...)=>
        removals = {}
        for i=1,select("#",...)
            var = select(i, ...)
            if type(var) == 'userdata' and var.type == "Var"
                var = tostring(var\as_lua!)
            elseif type(var) != 'string'
                var = tostring(var)
            removals[var] = true
        
        stack = {self}
        while #stack > 0
            lua, stack[#stack] = stack[#stack], nil
            for i=#lua.free_vars,1,-1
                if removals[lua.free_vars[i]]
                    remove lua.free_vars, i
            for b in *lua.bits
                if type(b) != 'string'
                    stack[#stack+1] = b
        @__str = nil

    convert_to_statements: (prefix="", suffix=";")=>
        unless @is_value
            return
        if prefix != ""
            @prepend prefix
        if suffix != ""
            @append suffix

    declare_locals: (to_declare=nil)=>
        if to_declare == nil
            to_declare, seen = {}, {}
            gather_from = =>
                for var in *@free_vars
                    unless seen[var]
                        seen[var] = true
                        to_declare[#to_declare+1] = var
                for bit in *@bits
                    if bit.__class == Lua
                        gather_from bit
            gather_from self
        @remove_free_vars to_declare
        if #to_declare > 0
            @prepend "local #{concat to_declare, ", "};\n"

    stringify: =>
        if @__str == nil
            buff = {}
            for i,b in ipairs @bits
                if type(b) != 'string'
                    b = b\stringify!
                buff[#buff+1] = b
            @__str = concat(buff, "")
        return @__str
    __tostring: =>
        if @__str == nil
            buff = {}
            for i,b in ipairs @bits
                buff[#buff+1] = tostring(b)
            @__str = concat(buff, "")
        return @__str

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
        @__str = nil
    
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
        @__str = nil

    make_offset_table: =>
        -- Return a mapping from output (lua) character number to input (nomsu) character number
        lua_to_nomsu, nomsu_to_lua = {}, {}
        walk = (lua, pos)->
            for b in *lua.bits
                if type(b) == 'string'
                    if lua.source
                        lua_to_nomsu[pos] = lua.source.start
                        nomsu_to_lua[lua.source.start] = pos
                else
                    walk b, pos
                pos += #b
        walk self, 1
        return {
            nomsu_filename:@source.filename
            lua_filename:tostring(@source)..".lua", lua_file:@stringify!
            :lua_to_nomsu, :nomsu_to_lua
        }

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
