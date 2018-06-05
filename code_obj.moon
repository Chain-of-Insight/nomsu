{:insert, :remove, :concat} = table
immutable = require 'immutable'
local Lua, Source
export LINE_STARTS

Source = immutable {"filename","start","stop"}, {
    name:"Source"
    from_string: (str)=>
        filename,start,stop = str\match("^@(.-)%[(%d+):(%d+)%]$")
        unless filename
            filename,start = str\match("^@(.-)%[(%d+)%]$")
        return Source(filename or str, tonumber(start or 1), tonumber(stop))
    __tostring: =>
        if @stop
            "@#{@filename}[#{@start}:#{@stop}]"
        else
            "@#{@filename}[#{@start}]"
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
        else if type(offset) != 'number' then error("Cannot add Source and #{type(offset)}")
        return Source(@filename, @start+offset, @stop)
}

class Code
    new: (@source, ...)=>
        @bits, @indents, @current_indent = {}, {}, 0
        @append(...)
        if type(@source) == 'string'
            @source = Source\from_string(@source)
        assert(@source and Source\is_instance(@source))
            
    append: (...)=>
        n = select("#",...)
        bits, indents = @bits, @indents
        match = string.match
        for i=1,n
            b = select(i, ...)
            bits[#bits+1] = b
            if type(b) == 'string'
                if spaces = match(b, "\n([ ]*)[^\n]*$")
                    @current_indent = #spaces
            elseif @current_indent != 0
                indents[#bits] = @current_indent
        @__str = nil
    
    prepend: (...)=>
        n = select("#",...)
        bits, indents = @bits, @indents
        for i=#bits+n,n+1,-1
            bits[i] = bits[i-n]
        for i=1,n
            bits[i] = select(i, ...)
        @current_indent = 0
        for i,b in ipairs(bits)
            if type(b) == 'string'
                if spaces = b\match("\n([ ]*)[^\n]*$")
                    @current_indent = #spaces
            elseif @current_indent != 0
                indents[i] = @current_indent
            else indents[i] = nil
        @__str = nil

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

    add_free_vars: (vars)=>
        return unless #vars > 0
        seen = {[v]:true for v in *@free_vars}
        for var in *vars
            assert(type(var) == 'userdata' and var.type == "Var")
            unless seen[var]
                @free_vars[#@free_vars+1] = var
                seen[var] = true
        @__str = nil
    
    remove_free_vars: (vars)=>
        return unless #vars > 0
        removals = {}
        for var in *vars
            assert(type(var) == 'userdata' and var.type == "Var")
            removals[var.value] = true
        
        stack = {self}
        while #stack > 0
            lua, stack[#stack] = stack[#stack], nil
            for i=#lua.free_vars,1,-1
                if removals[lua.free_vars[i].value]
                    remove lua.free_vars, i
            for b in *lua.bits
                if type(b) != 'string'
                    stack[#stack+1] = b
        @__str = nil

    as_statements: (prefix="", suffix=";")=>
        unless @is_value
            return self
        statements = Lua(@source)
        if prefix != ""
            statements\append prefix
        statements\append self
        if suffix != ""
            statements\append suffix
        return statements

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
        if #to_declare > 0
            @remove_free_vars to_declare
            @prepend "local #{concat [string.as_lua_id(v.value) for v in *to_declare], ", "};\n"
        return to_declare

    __tostring: =>
        if @__str == nil
            buff, indents = {}, @indents
            for i,b in ipairs @bits
                b = tostring(b)
                if indents[i]
                    b = b\gsub("\n", "\n"..((" ")\rep(indents[i])))
                buff[#buff+1] = b
            @__str = concat(buff, "")
        return @__str

    __len: =>
        #tostring(self)

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
                pos += #tostring(b)
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
        if @__str == nil
            buff, indents = {}, @indents
            for i,b in ipairs @bits
                b = tostring(b)
                if indents[i]
                    b = b\gsub("\n", "\n"..((" ")\rep(indents[i])))
                buff[#buff+1] = b
            @__str = concat(buff, "")
        return @__str

    __len: =>
        #tostring(self)

    parenthesize: =>
        @prepend "("
        @append ")"

return {:Code, :Nomsu, :Lua, :Source}
