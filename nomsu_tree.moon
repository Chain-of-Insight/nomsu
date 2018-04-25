-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
utils = require 'utils'
re = require 're'
lpeg = require 'lpeg'
{:repr, :stringify, :min, :max, :equivalent, :set, :is_list, :sum} = utils
immutable = require 'immutable'
{:insert, :remove, :concat} = table
{:Lua, :Nomsu, :Location} = require "lua_obj"


Types = {}
Types.DictEntry = immutable({"key","value"}, {name:"DictEntry"})
Types.is_node = (n)->
    type(n) == 'userdata' and getmetatable(n) and Types[n.type] == getmetatable(n)

-- Helper method:
Tree = (name, methods)->
    with methods
        .__tostring = => "#{@name}(#{repr(@value)}, #{repr @source})"
        .with_value = (value)=> getmetatable(self)(value, @source)
        .type = name
        .name = name
        .original_nomsu = =>
            leading_space = 0
            src_file = FILE_CACHE[@source.filename]
            while src_file\sub(@source.start-leading_space-1, @source.start-leading_space-1) == " "
                leading_space += 1
            if src_file\sub(@source.start-leading_space-1, @source.start-leading_space-1) != "\n"
                leading_space = 0
            ret = tostring(@source\get_text!)\gsub("\n"..((" ")\rep(leading_space)), "\n")
            return ret

    Types[name] = immutable {"value","source"}, methods


Tree "File",
    as_lua: (nomsu)=>
        if #@value == 1
            return @value[1]\as_lua(nomsu)
        lua = Lua(@source)
        for i, line in ipairs @value
            line_lua = line\as_lua(nomsu)
            if not line_lua
                error("No lua produced by #{repr line}", 0)
            if i > 1
                lua\append "\n"
            lua\convert_to_statements!
            lua\append line_lua
        lua\declare_locals!
        return lua

    as_nomsu: (inline=false)=>
        return nil if inline
        nomsu = Nomsu(@source)
        for i, line in ipairs @value
            line = assert(line\as_nomsu!, "Could not convert line to nomsu")
            nomsu\append line
            if i < #@value
                nomsu\append "\n"
        return nomsu

Tree "Nomsu",
    as_lua: (nomsu)=>
        Lua.Value(@source, "nomsu:parse(Nomsu(",repr(@value.source),", ",repr(tostring(@value.source\get_text!)),")).value[1]")

    as_nomsu: (inline=false)=>
        nomsu = @value\as_nomsu(true)
        if nomsu == nil and not inline
            nomsu = @value\as_nomsu!
            return nomsu and Nomsu(@source, "\\:\n    ", nomsu)
        return nomsu and Nomsu(@source, "\\(", nomsu, ")")

Tree "Block",
    as_lua: (nomsu)=>
        if #@value == 1
            return @value[1]\as_lua(nomsu)
        lua = Lua(@source)
        for i,line in ipairs @value
            line_lua = line\as_lua(nomsu)
            if i > 1
                lua\append "\n"
            line_lua\convert_to_statements!
            lua\append line_lua
        return lua

    as_nomsu: (inline=false)=>
        if #@value == 1
            return @value[1]\as_nomsu(inline)
        return nil if inline
        nomsu = Nomsu(@source)
        for i, line in ipairs @value
            line = assert(line\as_nomsu!, "Could not convert line to nomsu")
            nomsu\append line
            if i < #@value
                nomsu\append "\n"
        return nomsu

math_expression = re.compile [[ "%" (" " [*/^+-] " %")+ ]]
Tree "Action",
    as_lua: (nomsu)=>
        stub = @get_stub!
        action = rawget(nomsu.environment.ACTIONS, stub)
        metadata = nomsu.action_metadata[action]
        if metadata and metadata.compile_time
            args = [arg for arg in *@value when arg.type != "Word"]
            -- Force all compile-time actions to take a tree location
            if metadata.arg_orders
                new_args = [args[p-1] for p in *metadata.arg_orders[stub]]
                args = new_args
            -- Force Lua to avoid tail call optimization for debugging purposes
            ret = action(self, unpack(args))
            return ret

        lua = Lua.Value(@source)
        if not metadata and math_expression\match(stub)
            -- This is a bit of a hack, but this code handles arbitrarily complex
            -- math expressions like 2*x + 3^2 without having to define a single
            -- action for every possibility.
            for i,tok in ipairs @value
                if tok.type == "Word"
                    lua\append tok.value
                else
                    tok_lua = tok\as_lua(nomsu)
                    unless tok_lua.is_value
                        src = tok.source\get_text!
                        error("non-expression value inside math expression: #{colored.yellow src}")
                    lua\append tok_lua
                if i < #@value
                    lua\append " "
            lua\parenthesize!
            return lua

        args = {}
        for i, tok in ipairs @value
            if tok.type == "Word" then continue
            arg_lua = tok\as_lua(nomsu)
            unless arg_lua.is_value
                line, src = tok.source\get_line!, tok.source\get_text!
                error "#{line}: Cannot use:\n#{colored.yellow src}\nas an argument to #{stub}, since it's not an expression, it produces: #{repr arg_lua}", 0
            insert args, arg_lua

        if metadata and metadata.arg_orders
            args = [args[p] for p in *metadata.arg_orders[stub]]

        -- Not really worth bothering with ACTIONS.foo(...) style since almost every action
        -- has arguments, so it won't work
        lua\append "ACTIONS[",repr(stub),"]("
        for i, arg in ipairs args
            lua\append arg
            if i < #args then lua\append ", "
        lua\append ")"
        return lua

    get_stub: (include_names=false)=>
        bits = if include_names
            [(t.type == "Word" and t.value or "%#{t.value}") for t in *@value]
        else [(t.type == "Word" and t.value or "%") for t in *@value]
        return concat(bits, " ")

    as_nomsu: (inline=false)=>
        if inline
            nomsu = Nomsu(@source)
            for i,bit in ipairs @value
                if bit.type == "Word"
                    if i > 1
                        nomsu\append " "
                    nomsu\append bit.value
                else
                    arg_nomsu = bit\as_nomsu(true)
                    return nil unless arg_nomsu
                    unless i == 1
                        nomsu\append " "
                    if bit.type == "Action" or bit.type == "Block"
                        arg_nomsu\parenthesize!
                    nomsu\append arg_nomsu
            return nomsu
        else
            inline_version = @as_nomsu(true)
            if inline_version and #inline_version <= 80
                return inline_version
            nomsu = Nomsu(@source)
            spacer = nil
            for i,bit in ipairs @value
                if spacer
                    nomsu\append spacer

                if bit.type == "Word"
                    nomsu\append bit.value
                    spacer = " "
                else
                    arg_nomsu = bit\as_nomsu(true)
                    if arg_nomsu and #arg_nomsu < 80
                        if bit.type == "Action" or bit.type == "Block"
                            arg_nomsu\parenthesize!
                        spacer = " "
                    else
                        arg_nomsu = bit\as_nomsu!
                        return nil unless nomsu
                        if bit.type == "Action" or bit.type == "Block"
                            nomsu\append "\n    "
                        spacer = "\n.."
                    nomsu\append arg_nomsu
            return nomsu
    

Tree "Text",
    as_lua: (nomsu)=>
        lua = Lua.Value(@source)
        string_buffer = ""
        for bit in *@value
            if type(bit) == "string"
                string_buffer ..= bit
                continue
            if string_buffer ~= ""
                if #lua.bits > 0 then lua\append ".."
                lua\append repr(string_buffer)
                string_buffer = ""
            bit_lua = bit\as_lua(nomsu)
            unless bit_lua.is_value
                line, src = bit.source\get_line!, bit.source\get_text!
                error "#{line}: Cannot use #{colored.yellow bit} as a string interpolation value, since it's not an expression.", 0
            if #lua.bits > 0 then lua\append ".."
            if bit.type != "Text"
                bit_lua = Lua.Value(bit.source, "stringify(",bit_lua,")")
            lua\append bit_lua

        if string_buffer ~= "" or #lua.bits == 0
            if #lua.bits > 0 then lua\append ".."
            lua\append repr(string_buffer)

        if #lua.bits > 1
            lua\parenthesize!
        return lua

    as_nomsu: (inline=false)=>
        if inline
            nomsu = Nomsu(@source, '"')
            for bit in *@value
                if type(bit) == 'string'
                    -- Force indented text
                    return nil if bit\find("\n")
                    -- TODO: unescape better
                    nomsu\append (bit\gsub("\\","\\\\")\gsub("\n","\\n"))
                else
                    interp_nomsu = bit\as_nomsu(true)
                    if interp_nomsu
                        if bit.type != "Word" and bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
                            interp_nomsu\parenthesize!
                        nomsu\append "\\", interp_nomsu
                    else return nil
            nomsu\append '"'
            return nomsu
        else
            inline_version = @as_nomsu(true)
            if inline_version and #inline_version <= 80
                return inline_version
            nomsu = Nomsu(@source, '".."\n    ')
            for i, bit in ipairs @value
                if type(bit) == 'string'
                    nomsu\append (bit\gsub("\\","\\\\")\gsub("\n","\n    "))
                else
                    interp_nomsu = bit\as_nomsu(true)
                    if interp_nomsu
                        if bit.type != "Word" and bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
                            interp_nomsu\parenthesize!
                        nomsu\append "\\", interp_nomsu
                    else
                        interp_nomsu = bit\as_nomsu!
                        return nil unless interp_nomsu
                        nomsu\append "\\\n        ", interp_nomsu
                        if i < #@value
                            nomsu\append "\n    .."
            return nomsu

Tree "List",
    as_lua: (nomsu)=>
        lua = Lua.Value @source, "{"
        line_length = 0
        for i, item in ipairs @value
            item_lua = item\as_lua(nomsu)
            unless item_lua.is_value
                line, src = item.source\get_line!, item.source\get_text!
                error "#{line}: Cannot use #{colored.yellow src} as a list item, since it's not an expression.", 0
            lua\append item_lua
            item_string = tostring(item_lua)
            last_line = item_string\match("[^\n]*$")
            if item_string\match("\n")
                line_length = #last_line
            else
                line_length += #last_line
            if i < #@value
                if line_length >= 80
                    lua\append ",\n"
                    line_length = 0
                else
                    lua\append ", "
                    line_length += 2
        lua\append "}"
        return lua

    as_nomsu: (inline=false)=>
        if inline
            nomsu = Nomsu(@source, "[")
            for i, item in ipairs @value
                item_nomsu = item\as_nomsu(true)
                return nil unless item_nomsu
                if i > 1
                    nomsu\append ", "
                nomsu\append item_nomsu
            nomsu\append "]"
            return nomsu
        else
            inline_version = @as_nomsu(true)
            if inline_version and #inline_version <= 80
                return inline_version
            nomsu = Nomsu(@source, "[..]")
            line = Nomsu(@source, "\n    ")
            for item in *@value
                item_nomsu = item\as_nomsu(true)
                if item_nomsu and #line + #", " + #item_nomsu <= 80
                    if #line.bits > 1
                        line\append ", "
                    line\append item_nomsu
                else
                    unless item_nomsu
                        item_nomsu = item\as_nomsu!
                        return nil unless item_nomsu
                    if #line.bits > 1
                        nomsu\append line
                        line = Nomsu(item.source, "\n    ")
                    line\append item_nomsu
            if #line.bits > 1
                nomsu\append line
            return nomsu

Tree "Dict",
    as_lua: (nomsu)=>
        lua = Lua.Value @source, "{"
        line_length = 0
        for i, entry in ipairs @value
            key_lua = entry.key\as_lua(nomsu)
            unless key_lua.is_value
                line, src = key.source\get_line!, key.source\get_text!
                error "#{line}: Cannot use #{colored.yellow src} as a dict key, since it's not an expression.", 0
            value_lua = entry.value\as_lua(nomsu)
            unless value_lua.is_value
                line, src = value.source\get_line!, value.source\get_text!
                error "#{line}: Cannot use #{colored.yellow src} as a dict value, since it's not an expression.", 0
            key_str = tostring(key_lua)\match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
            if key_str
                lua\append key_str,"=",value_lua
            elseif tostring(key_lua)\sub(1,1) == "["
                -- NOTE: this *must* use a space after the [ to avoid freaking out
                -- Lua's parser if the inner expression is a long string. Lua
                -- parses x[[[y]]] as x("[y]"), not as x["y"]
                lua\append "[ ",key_lua,"]=",value_lua
            else
                lua\append "[",key_lua,"]=",value_lua

            -- TODO: maybe make this more accurate? It's only a heuristic, so eh...
            newlines, last_line = ("[#{key_lua}=#{value_lua}")\match("^(.-)([^\n]*)$")
            if #newlines > 0
                line_length = #last_line
            else
                line_length += #last_line
            if i < #@value
                if line_length >= 80
                    lua\append ",\n"
                    line_length = 0
                else
                    lua\append ", "
                    line_length += 2
        lua\append "}"
        return lua

    as_nomsu: (inline=false)=>
        if inline
            nomsu = Nomsu(@source, "{")
            for i, entry in ipairs @value
                key_nomsu = entry.key\as_nomsu(true)
                return nil unless key_nomsu
                if entry.key.type == "Action" or entry.key.type == "Block"
                    key_nomsu\parenthesize!
                value_nomsu = entry.value\as_nomsu(true)
                return nil unless value_nomsu
                if i > 1
                    nomsu\append ", "
                nomsu\append key_nomsu,":",value_nomsu
            nomsu\append "}"
            return nomsu
        else
            inline_version = @as_nomsu(true)
            if inline_version then return inline_version
            nomsu = Nomsu(@source, "{..}")
            line = Nomsu(@source, "\n    ")
            for entry in *@value
                key_nomsu = entry.key\as_nomsu(true)
                return nil unless key_nomsu
                if entry.key.type == "Action" or entry.key.type == "Block"
                    key_nomsu\parenthesize!
                value_nomsu = entry.value\as_nomsu(true)
                if value_nomsu and #line + #", " + #key_nomsu + #":" + #value_nomsu <= 80
                    if #line.bits > 1
                        line\append ", "
                    line\append key_nomsu,":",value_nomsu
                else
                    unless value_nomsu
                        value_nomsu = entry.value\as_nomsu!
                        return nil unless value_nomsu
                    if #line.bits > 1
                        nomsu\append line
                        line = Nomsu(bit.source, "\n    ")
                    line\append key_nomsu,":",value_nomsu
            if #line.bits > 1
                nomsu\append line
            return nomsu

Tree "IndexChain",
    as_lua: (nomsu)=>
        lua = @value[1]\as_lua(nomsu)
        unless lua.is_value
            line, src = @value[1].source\get_line!, @value[1].source\get_text!
            error "#{line}: Cannot index #{colored.yellow src}, since it's not an expression.", 0
        first_char = tostring(lua)\sub(1,1)
        if first_char == "{" or first_char == '"' or first_char == "["
            lua\parenthesize!

        for i=2,#@value
            key = @value[i]
            if key.type == 'Text' and #key.value == 1 and type(key.value[1]) == 'string' and key.value[1]\match("^[a-zA-Z_][a-zA-Z0-9_]$")
                lua\append ".#{key.value[1]}"
                continue
            key_lua = key\as_lua(nomsu)
            unless key_lua.is_value
                line, src = key.source\get_line!, key.source\get_text!
                error "#{line}: Cannot use #{colored.yellow src} as an index, since it's not an expression.", 0
            -- NOTE: this *must* use a space after the [ to avoid freaking out
            -- Lua's parser if the inner expression is a long string. Lua
            -- parses x[[[y]]] as x("[y]"), not as x["y"]
            if tostring(key_lua)\sub(1,1) == '['
                lua\append "[ ",key_lua,"]"
            else
                lua\append "[",key_lua,"]"
        return lua

    as_nomsu: (inline=false)=>
        nomsu = Nomsu(@source)
        for i, bit in ipairs @value
            if i > 1
                nomsu\append "."
            bit_nomsu = bit\as_nomsu(true)
            return nil unless bit_nomsu
            nomsu\append bit_nomsu
        return nomsu

Tree "Number",
    as_lua: (nomsu)=>
        Lua.Value(@source, tostring(@value))
    
    as_nomsu: (inline=false)=>
        return Nomsu(@source, tostring(@value))

Tree "Var",
    as_lua: (nomsu)=>
        lua_id = "_"..(@value\gsub "%W", (verboten)->
            if verboten == "_" then "__" else ("_%x")\format(verboten\byte!))
        Lua.Value(@source, lua_id)

    as_nomsu: (inline=false)=>
        return Nomsu(@source, "%", @value)

Tree "Word",
    as_lua: (nomsu)=>
        error("Attempt to convert Word to lua")

    as_nomsu: (inline=false)=>
        return Nomsu(@source, @value)

Tree "Comment",
    as_lua: (nomsu)=>
        Lua(@source, "--"..@value\gsub("\n","\n--").."\n")

    as_nomsu: (inline=false)=>
        return nil if inline
        if @value\match("\n")
            return Nomsu(@source, "#..", @value\gsub("\n", "\n    "))
        else
            return Nomsu(@source, "#", @value)

return Types
