-- This file contains objects that are used to track code positions and incrementally
-- build up generated code, while keeping track of where it came from, and managing
-- indentation levels.
{:insert, :remove, :concat} = table
unpack or= table.unpack
local LuaCode, NomsuCode, Source

class Source
    new: (@filename, @start, @stop)=>

    @from_string: (str)=>
        filename,start,stop = str\match("^@(.-)%[(%d+):(%d+)%]$")
        unless filename
            filename,start = str\match("^@(.-)%[(%d+)%]$")
        return @(filename or str, tonumber(start or 1), tonumber(stop))

    @is_instance: (x)=> type(x) == 'table' and x.__class == @

    __tostring: => "@#{@filename}[#{@start}#{@stop and ':'..@stop or ''}]"

    as_lua: => "Source(#{@filename\as_lua!}, #{@start}#{@stop and ', '..@stop or ''})"
    
    __eq: (other)=>
        getmetatable(@) == getmetatable(other) and @filename == other.filename and @start == other.start and @stop == other.stop

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

class Code
    new: (...)=>
        @bits = {}
        @add(...)

    @from: (source, ...)=>
        inst = self(...)
        if type(source) == 'string'
            source = Source\from_string(source)
        inst.source = source
        return inst

    @is_instance: (x)=> type(x) == 'table' and x.__class == @

    text: =>
        if @__str == nil
            buff, indent = {}, 0
            {:match, :gsub, :rep} = string
            for i,b in ipairs @bits
                if type(b) == 'string'
                    if spaces = match(b, "\n([ ]*)[^\n]*$")
                        indent = #spaces
                else
                    b = b\text!
                    if indent > 0
                        b = gsub(b, "\n", "\n"..rep(" ", indent))
                buff[#buff+1] = b
            @__str = concat(buff, "")
        return @__str

    last: (n)=>
        if @__str
            return @__str\sub(-n, -1)
        last = ""
        for i=#@bits,1,-1
            b = @bits[i]
            last = (type(b) == 'string' and b\sub(-(n-#last)) or b\last(n-#last))..last
            break if #last == n
        return last

    first: (n)=>
        if @__str
            return @__str\sub(1,n)
        first = ""
        for b in *@bits
            first ..= type(b) == 'string' and b\sub(1,n-#first+1) or b\first(n-#first+1)
            break if #first == n
        return first

    __tostring: => @text!

    as_lua: =>
        if @source
            "#{@__class.__name}:from(#{concat {tostring(@source)\as_lua!, unpack([b\as_lua! for b in *@bits])}, ", "})"
        else
            "#{@__class.__name}(#{concat [b\as_lua! for b in *@bits], ", "})"

    __len: =>
        if @__str
            return #@__str
        len = 0
        for b in *@bits
            len += #b
        return len
    
    match: (...)=> @text!\match(...)

    gmatch: (...)=> @text!\gmatch(...)
    
    dirty: =>
        @__str = nil
        @_trailing_line_len = nil
        -- Multi-line only goes from false->true, since there is no API for removing bits
        @_is_multiline = nil if @_is_multiline == false
            
    add: (...)=>
        n = select("#",...)
        match = string.match
        bits = @bits
        for i=1,n
            b = select(i, ...)
            assert(b, "code bit is nil")
            assert(not Source\is_instance(b), "code bit is a Source")
            if b == '' then continue
            bits[#bits+1] = b
        @dirty!
    
    trailing_line_len: =>
        if @_trailing_line_len == nil
            @_trailing_line_len = #@text!\match("[^\n]*$")
        return @_trailing_line_len
    
    is_multiline: =>
        if @_is_multiline == nil
            match = string.match
            @_is_multiline = false
            for b in *@bits
                if type(b) == 'string'
                    if match(b, '\n')
                        @_is_multiline = true
                        break
                elseif b\is_multiline!
                    @_is_multiline = true
                    break
        return @_is_multiline

    concat_add: (values, joiner, wrapping_joiner)=>
        wrapping_joiner or= joiner
        match = string.match
        bits = @bits
        line_len = 0
        for i=1,#values
            b = values[i]
            if i > 1
                if line_len > 80
                    bits[#bits+1] = wrapping_joiner
                    line_len = 0
                else
                    bits[#bits+1] = joiner
            bits[#bits+1] = b
            b.dirty = error if type(b) != 'string'
            line = b\match("\n([^\n]*)$")
            if line
                line_len = #line
            else
                line_len += #b
        @dirty!
    
    prepend: (...)=>
        n = select("#",...)
        bits = @bits
        for i=#bits+n,n+1,-1
            bits[i] = bits[i-n]
        for i=1,n
            b = select(i, ...)
            b.dirty = error if type(b) != 'string'
            bits[i] = b
        @dirty!

    parenthesize: =>
        @prepend "("
        @add ")"

class LuaCode extends Code
    __tostring: Code.__tostring
    as_lua: Code.as_lua
    __len: Code.__len
    new: (...)=>
        super ...
        @free_vars = {}
    
    add_free_vars: (vars)=>
        return unless #vars > 0
        seen = {[v]:true for v in *@free_vars}
        for var in *vars
            assert type(var) == 'string'
            unless seen[var]
                @free_vars[#@free_vars+1] = var
                seen[var] = true
        @dirty!
    
    remove_free_vars: (vars=nil)=>
        vars or= @get_free_vars!
        return unless #vars > 0
        removals = {}
        for var in *vars
            assert type(var) == 'string'
            removals[var] = true
        
        stack = {self}
        while #stack > 0
            lua, stack[#stack] = stack[#stack], nil
            for i=#lua.free_vars,1,-1
                free_var = lua.free_vars[i]
                if removals[free_var]
                    remove lua.free_vars, i
            for b in *lua.bits
                if type(b) != 'string'
                    stack[#stack+1] = b
        @dirty!

    get_free_vars: =>
        vars, seen = {}, {}
        gather_from = =>
            for var in *@free_vars
                unless seen[var]
                    seen[var] = true
                    vars[#vars+1] = var
            for bit in *@bits
                unless type(bit) == 'string'
                    gather_from bit
        gather_from self
        return vars

    declare_locals: (to_declare=nil)=>
        to_declare or= @get_free_vars!
        if #to_declare > 0
            @remove_free_vars to_declare
            @prepend "local #{concat to_declare, ", "};\n"
        return to_declare

    make_offset_table: =>
        assert @source, "This code doesn't have a source"
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
            lua_filename:tostring(@source)..".lua", lua_file:@text!
            :lua_to_nomsu, :nomsu_to_lua
        }

    parenthesize: =>
        @prepend "("
        @add ")"

class NomsuCode extends Code
    __tostring: Code.__tostring
    as_lua: Code.as_lua
    __len: Code.__len

Code.__base.add_1_joined_with = assert Code.__base.concat_add
Code.__base.add = assert Code.__base.add

return {:Code, :NomsuCode, :LuaCode, :Source}
