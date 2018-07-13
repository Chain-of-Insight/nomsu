-- This file contains objects that are used to track code positions and incrementally
-- build up generated code, while keeping track of where it came from, and managing
-- indentation levels.
{:insert, :remove, :concat} = table
{:repr} = require 'utils'
local LuaCode, NomsuCode, Source
export LINE_STARTS

class Source
    new: (@filename, @start, @stop)=>

    @from_string: (str)=>
        filename,start,stop = str\match("^@(.-)%[(%d+):(%d+)%]$")
        unless filename
            filename,start = str\match("^@(.-)%[(%d+)%]$")
        return @(filename or str, tonumber(start or 1), tonumber(stop))

    @is_instance: (x)=> type(x) == 'table' and x.__class == @

    __tostring: =>
        if @stop
            "@#{@filename}[#{@start}:#{@stop}]"
        else
            "@#{@filename}[#{@start}]"
    
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
    is_code: true
    new: (@source, ...)=>
        @bits = {}
        @indents, @current_indent = {}, 0
        @trailing_line_len = 0
        if type(@source) == 'string'
            @source = Source\from_string(@source)
        assert(@source and Source\is_instance(@source), "Source has the wrong type")
        @append(...)
            
    append: (...)=>
        n = select("#",...)
        bits, indents = @bits, @indents
        match = string.match
        for i=1,n
            b = select(i, ...)
            assert(b, "code bit is nil")
            assert(not Source\is_instance(b), "code bit is a Source")
            if b == '' then continue
            bits[#bits+1] = b
            if type(b) != 'string' and not (type(b) == 'table' and b.is_code)
                b = repr(b)
            if type(b) == 'string'
                trailing_text, spaces = match(b, "\n(([ ]*)[^\n]*)$")
                if trailing_text
                    @current_indent = #spaces
                    @trailing_line_len = #trailing_text
            else
                if #b.indents > 1
                    @trailing_line_len = b.trailing_line_len
                else
                    @trailing_line_len += #tostring(b)
                if @current_indent != 0
                    indents[#bits] = @current_indent
        @__str = nil
            
    concat_append: (values, joiner, wrapping_joiner)=>
        wrapping_joiner or= joiner
        bits, indents = @bits, @indents
        match = string.match
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
            if type(b) != 'string' and @current_indent != 0
                indents[#bits] = @current_indent
            b_str = tostring(b)
            line, spaces = match(b_str, "\n(([ ]*)[^\n]*)$")
            if spaces
                if type(b) == 'string'
                    @current_indent = #spaces
                line_len = #line
            else
                line_len += #b
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

class LuaCode extends Code
    new: (...)=>
        super ...
        @free_vars = {}
        @is_value = false
        @__str = nil
    
    @Value = (...)->
        lua = LuaCode(...)
        lua.is_value = true
        return lua
    
    @Comment = (...)->
        lua = LuaCode(...)
        lua.is_comment = true
        return lua

    add_free_vars: (vars)=>
        return unless #vars > 0
        seen = {[v]:true for v in *@free_vars}
        for var in *vars
            assert type(var) == 'string'
            unless seen[var]
                @free_vars[#@free_vars+1] = var
                seen[var] = true
        @__str = nil
    
    remove_free_vars: (vars)=>
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
        @__str = nil

    declare_locals: (to_declare=nil)=>
        if to_declare == nil
            to_declare, seen = {}, {}
            gather_from = =>
                for var in *@free_vars
                    unless seen[var]
                        seen[var] = true
                        to_declare[#to_declare+1] = var
                for bit in *@bits
                    if bit.__class == LuaCode
                        gather_from bit
            gather_from self
        if #to_declare > 0
            @remove_free_vars to_declare
            @prepend "local #{concat to_declare, ", "};\n"
        return to_declare

    as_statements: (prefix="", suffix=";")=>
        unless @is_value
            return self
        statements = LuaCode(@source)
        if prefix != ""
            statements\append prefix
        statements\append self
        if suffix != ""
            statements\append suffix
        return statements
    
    as_expr: =>
        if @is_value
            return self
        error("Cannot convert to expression: #{tostring self}")

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

class NomsuCode extends Code
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

return {:Code, :NomsuCode, :LuaCode, :Source}
