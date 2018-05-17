-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
utils = require 'utils'
{:repr, :stringify, :min, :max, :equivalent, :set, :is_list, :sum} = utils
immutable = require 'immutable'
{:insert, :remove, :concat} = table
{:Lua, :Nomsu, :Location} = require "code_obj"

MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value

Types = {}
Types.is_node = (n)->
    type(n) == 'userdata' and getmetatable(n) and Types[n.type] == getmetatable(n)

-- Helper method:
Tree = (name, kind, methods)->
    assert((kind == 'single') or (kind == 'multi'))
    is_multi = (kind == 'multi')
    with methods
        .with_value = (value)=> getmetatable(self)(value)
        .type = name
        .name = name
        .is_multi = is_multi
        if is_multi
            .__tostring = => "#{@name}(#{table.concat [repr(v) for v in *@], ', '})"
            .map = (fn)=>
                if ret = fn(@)
                    return ret
                new_vals = [v.map and v\map(fn) or v for v in *@]
                ret = getmetatable(self)(unpack(new_vals))
                return ret
        else
            .__tostring = => "#{@name}(#{repr(@value)})"
            .map = (fn)=> fn(@) or @

    if is_multi
        Types[name] = immutable nil, methods
    else
        Types[name] = immutable {"value"}, methods


Tree "EscapedNomsu", 'single',
    as_lua: (nomsu)=>
        make_tree = (t)->
            if type(t) != 'userdata'
                return repr(t)
            if t.is_multi
                bits = [make_tree(bit) for bit in *t]
                return t.type.."("..table.concat(bits, ", ")..")"
            else
                return t.type.."("..make_tree(t.value)..")"
        Lua.Value nil, make_tree(@value)

    as_nomsu: (inline=false)=>
        nomsu = @value\as_nomsu(true)
        if nomsu == nil and not inline
            nomsu = @value\as_nomsu!
            return nomsu and Nomsu nil, "\\:\n    ", nomsu
        return nomsu and Nomsu nil, "\\(", nomsu, ")"

    map: (fn)=> fn(@) or @\map(fn)

Tree "Block", 'multi',
    as_lua: (nomsu)=>
        lua = Lua!
        for i,line in ipairs @
            line_lua = line\as_lua(nomsu)
            if i > 1
                lua\append "\n"
            lua\append line_lua\as_statements!
        return lua

    as_nomsu: (inline=false)=>
        if inline
            nomsu = Nomsu!
            for i,line in ipairs @
                if i > 1
                    nomsu\append "; "
                line_nomsu = line\as_nomsu(true)
                return nil unless line_nomsu
                nomsu\append line_nomsu
            return nomsu
        nomsu = Nomsu!
        for i, line in ipairs @
            line = assert(line\as_nomsu(nil, true), "Could not convert line to nomsu")
            nomsu\append line
            if i < #@
                nomsu\append "\n"
                if tostring(line)\match("\n")
                    nomsu\append "\n"
        return nomsu

math_expression = re.compile [[ ([+-] " ")* "%" (" " [*/^+-] (" " [+-])* " %")+ !. ]]
Tree "Action", 'multi',
    as_lua: (nomsu)=>
        stub = @get_stub!
        compile_action = nomsu.environment.COMPILE_ACTIONS[stub]
        if compile_action
            args = [arg for arg in *@ when arg.type != "Word"]
            -- Force all compile-time actions to take a tree location
            args = [args[p-1] for p in *nomsu.environment.ARG_ORDERS[compile_action][stub]]
            -- Force Lua to avoid tail call optimization for debugging purposes
            ret = compile_action(self, unpack(args))
            if not ret then error("Failed to produce any Lua")
            return ret
        action = rawget(nomsu.environment.ACTIONS, stub)
        lua = Lua.Value!
        if not action and math_expression\match(stub)
            -- This is a bit of a hack, but this code handles arbitrarily complex
            -- math expressions like 2*x + 3^2 without having to define a single
            -- action for every possibility.
            for i,tok in ipairs @
                if tok.type == "Word"
                    lua\append tok.value
                else
                    tok_lua = tok\as_lua(nomsu)
                    unless tok_lua.is_value
                        error("non-expression value inside math expression: #{colored.yellow repr(tok)}")
                    if tok.type == "Action"
                        tok_lua\parenthesize!
                    lua\append tok_lua
                if i < #@
                    lua\append " "
            return lua

        args = {}
        for i, tok in ipairs @
            if tok.type == "Word" then continue
            arg_lua = tok\as_lua(nomsu)
            unless arg_lua.is_value
                error "Cannot use:\n#{colored.yellow repr(tok)}\nas an argument to #{stub}, since it's not an expression, it produces: #{repr arg_lua}", 0
            insert args, arg_lua

        if action
            args = [args[p] for p in *nomsu.environment.ARG_ORDERS[action][stub]]

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
            [(t.type == "Word" and t.value or "%#{t.value}") for t in *@]
        else [(t.type == "Word" and t.value or "%") for t in *@]
        return concat(bits, " ")

    as_nomsu: (inline=false, can_use_colon=false)=>
        if inline
            nomsu = Nomsu!
            for i,bit in ipairs @
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
            nomsu = Nomsu!
            next_space = ""
            -- TODO: track line length as we go and use 80-that instead of 80 for wrapping
            last_colon = nil
            for i,bit in ipairs @
                if bit.type == "Word"
                    nomsu\append next_space, bit.value
                    next_space = " "
                else
                    arg_nomsu = if last_colon == i-1 and bit.type == "Action" then nil
                    elseif bit.type == "Block" then nil
                    else bit\as_nomsu(true)

                    if arg_nomsu and #arg_nomsu < MAX_LINE
                        if bit.type == "Action"
                            if can_use_colon and i > 1
                                nomsu\append next_space\match("[^ ]*"), ": ", arg_nomsu
                                next_space = "\n.."
                                last_colon = i
                            else
                                nomsu\append next_space, "(", arg_nomsu, ")"
                                next_space = " "
                        else
                            nomsu\append next_space, arg_nomsu
                            next_space = " "
                    else
                        arg_nomsu = bit\as_nomsu(nil, true)
                        return nil unless nomsu
                        -- These types carry their own indentation
                        if bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
                            if i == 1
                                arg_nomsu = Nomsu(nil, "(..)\n    ", arg_nomsu)
                            else
                                arg_nomsu = Nomsu(nil, "\n    ", arg_nomsu)
                        
                        if last_colon == i-1 and (bit.type == "Action" or bit.type == "Block")
                            next_space = ""
                        nomsu\append next_space, arg_nomsu
                        next_space = "\n.."

                    if next_space == " " and #(tostring(nomsu)\match("[^\n]*$")) > MAX_LINE
                        next_space = "\n.."
            return nomsu

Tree "Text", 'multi',
    as_lua: (nomsu)=>
        lua = Lua.Value!
        string_buffer = ""
        for bit in *@
            if type(bit) == "string"
                string_buffer ..= bit
                continue
            if string_buffer ~= ""
                if #lua.bits > 0 then lua\append ".."
                lua\append repr(string_buffer)
                string_buffer = ""
            bit_lua = bit\as_lua(nomsu)
            unless bit_lua.is_value
                error "Cannot use #{colored.yellow repr(bit)} as a string interpolation value, since it's not an expression.", 0
            if #lua.bits > 0 then lua\append ".."
            if bit.type != "Text"
                bit_lua = Lua.Value(nil, "stringify(",bit_lua,")")
            lua\append bit_lua

        if string_buffer ~= "" or #lua.bits == 0
            if #lua.bits > 0 then lua\append ".."
            lua\append repr(string_buffer)

        if #lua.bits > 1
            lua\parenthesize!
        return lua

    as_nomsu: (inline=false)=>
        if inline
            nomsu = Nomsu(nil, '"')
            for bit in *@
                if type(bit) == 'string'
                    -- TODO: unescape better?
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
            if inline_version and #inline_version <= MAX_LINE
                return inline_version
            nomsu = Nomsu(nil, '".."\n    ')
            for i, bit in ipairs @
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
                        if i < #@
                            nomsu\append "\n    .."
            return nomsu

Tree "List", 'multi',
    as_lua: (nomsu)=>
        lua = Lua.Value nil, "{"
        line_length = 0
        for i, item in ipairs @
            item_lua = item\as_lua(nomsu)
            unless item_lua.is_value
                error "Cannot use #{colored.yellow repr(item)} as a list item, since it's not an expression.", 0
            lua\append item_lua
            item_string = tostring(item_lua)
            last_line = item_string\match("[^\n]*$")
            if item_string\match("\n")
                line_length = #last_line
            else
                line_length += #last_line
            if i < #@
                if line_length >= MAX_LINE
                    lua\append ",\n  "
                    line_length = 0
                else
                    lua\append ", "
                    line_length += 2
        lua\append "}"
        return lua

    as_nomsu: (inline=false)=>
        if inline
            nomsu = Nomsu(nil, "[")
            for i, item in ipairs @
                item_nomsu = item\as_nomsu(true)
                return nil unless item_nomsu
                if i > 1
                    nomsu\append ", "
                nomsu\append item_nomsu
            nomsu\append "]"
            return nomsu
        else
            inline_version = @as_nomsu(true)
            if inline_version and #inline_version <= MAX_LINE
                return inline_version
            nomsu = Nomsu(nil, "[..]")
            line = Nomsu(nil, "\n    ")
            for item in *@
                item_nomsu = item\as_nomsu(true)
                if item_nomsu and #line + #", " + #item_nomsu <= MAX_LINE
                    if #line.bits > 1
                        line\append ", "
                    line\append item_nomsu
                else
                    unless item_nomsu
                        item_nomsu = item\as_nomsu!
                        return nil unless item_nomsu
                    if #line.bits > 1
                        nomsu\append line
                        line = Nomsu(nil, "\n    ")
                    line\append item_nomsu
            if #line.bits > 1
                nomsu\append line
            return nomsu

Tree "Dict", 'multi',
    as_lua: (nomsu)=>
        lua = Lua.Value nil, "{"
        line_length = 0
        for i, entry in ipairs @
            entry_lua = entry\as_lua(nomsu)
            lua\append entry_lua
            entry_lua_str = tostring(entry_lua)
            -- TODO: maybe make this more accurate? It's only a heuristic, so eh...
            last_line = entry_lua_str\match("\n([^\n]*)$")
            if last_line
                line_length = #last_line
            else
                line_length += #entry_lua_str
            if i < #@
                if line_length >= MAX_LINE
                    lua\append ",\n  "
                    line_length = 0
                else
                    lua\append ", "
                    line_length += 2
        lua\append "}"
        return lua

    as_nomsu: (inline=false)=>
        if inline
            nomsu = Nomsu(nil, "{")
            for i, entry in ipairs @
                entry_nomsu = entry\as_nomsu(true)
                return nil unless entry_nomsu
                if i > 1
                    nomsu\append ", "
                nomsu\append entry_nomsu
            nomsu\append "}"
            return nomsu
        else
            inline_version = @as_nomsu(true)
            if inline_version then return inline_version
            nomsu = Nomsu(nil, "{..}")
            line = Nomsu(nil, "\n    ")
            for entry in *@
                entry_nomsu = entry\as_nomsu!
                return nil unless entry_nomsu
                if #line + #tostring(entry_nomsu) <= MAX_LINE
                    if #line.bits > 1
                        line\append ", "
                    line\append entry_nomsu
                else
                    if #line.bits > 1
                        nomsu\append line
                        line = Nomsu(nil, "\n    ")
                    line\append entry_nomsu
            if #line.bits > 1
                nomsu\append line
            return nomsu

Tree "DictEntry", 'multi',
    as_lua: (nomsu)=>
        key, value = @[1], @[2]
        key_lua = key\as_lua(nomsu)
        unless key_lua.is_value
            error "Cannot use #{colored.yellow repr(key)} as a dict key, since it's not an expression.", 0
        value_lua = value and value\as_lua(nomsu) or Lua.Value(nil, "true")
        unless value_lua.is_value
            error "Cannot use #{colored.yellow repr(value)} as a dict value, since it's not an expression.", 0
        key_str = tostring(key_lua)\match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
        return if key_str
            Lua nil, key_str,"=",value_lua
        elseif tostring(key_lua)\sub(1,1) == "["
            -- NOTE: this *must* use a space after the [ to avoid freaking out
            -- Lua's parser if the inner expression is a long string. Lua
            -- parses x[[[y]]] as x("[y]"), not as x["y"]
            Lua nil, "[ ",key_lua,"]=",value_lua
        else
            Lua nil, "[",key_lua,"]=",value_lua

    as_nomsu: (inline=true)=>
        key, value = @[1], @[2]
        key_nomsu = key\as_nomsu(true)
        return nil unless key_nomsu
        if key.type == "Action" or key.type == "Block"
            key_nomsu\parenthesize!
        value_nomsu = if value
            value\as_nomsu(true)
        else Nomsu(nil, "")
        if inline and not value_nomsu then return nil
        if not value_nomsu
            return nil if inline
            value_nomsu = value\as_nomsu!
            return nil unless value_nomsu
        return Nomsu nil, key_nomsu, ":", value_nomsu


Tree "IndexChain", 'multi',
    as_lua: (nomsu)=>
        lua = @[1]\as_lua(nomsu)
        unless lua.is_value
            error "Cannot index #{colored.yellow repr(@[1])}, since it's not an expression.", 0
        first_char = tostring(lua)\sub(1,1)
        if first_char == "{" or first_char == '"' or first_char == "["
            lua\parenthesize!

        for i=2,#@
            key = @[i]
            key_lua = key\as_lua(nomsu)
            unless key_lua.is_value
                error "Cannot use #{colored.yellow repr(key)} as an index, since it's not an expression.", 0
            key_lua_str = tostring(key_lua)
            if lua_id = key_lua_str\match("^['\"]([a-zA-Z_][a-zA-Z0-9_]*)['\"]$")
                lua\append ".#{lua_id}"
            elseif key_lua_str\sub(1,1) == '['
                -- NOTE: this *must* use a space after the [ to avoid freaking out
                -- Lua's parser if the inner expression is a long string. Lua
                -- parses x[[[y]]] as x("[y]"), not as x["y"]
                lua\append "[ ",key_lua," ]"
            else
                lua\append "[",key_lua,"]"
        return lua

    as_nomsu: (inline=false)=>
        nomsu = Nomsu!
        for i, bit in ipairs @
            if i > 1
                nomsu\append "."
            bit_nomsu = bit\as_nomsu(true)
            return nil unless bit_nomsu
            if bit.type == "Action" or bit.type == "Block"
                bit_nomsu\parenthesize!
            nomsu\append bit_nomsu
        return nomsu

Tree "Number", 'single',
    as_lua: (nomsu)=>
        Lua.Value(nil, tostring(@value))
    
    as_nomsu: (inline=false)=>
        return Nomsu(nil, tostring(@value))

Tree "Var", 'single',
    as_lua_id: (v)->
        "_"..(v\gsub("%W", (c)-> if c == "_" then "__" else ("_%x")\format(c\byte!)))

    as_lua: (nomsu)=>
        Lua.Value(nil, self.as_lua_id(@value))

    as_nomsu: (inline=false)=>
        return Nomsu(nil, "%", @value)

Tree "Word", 'single',
    as_lua: (nomsu)=>
        error("Attempt to convert Word to lua")

    as_nomsu: (inline=false)=>
        return Nomsu(nil, @value)

Tree "Comment", 'single',
    as_lua: (nomsu)=>
        Lua(nil, "--"..@value\gsub("\n","\n--").."\n")

    as_nomsu: (inline=false)=>
        return nil if inline
        if @value\match("\n")
            return Nomsu(nil, "#..", @value\gsub("\n", "\n    "))
        else
            return Nomsu(nil, "#", @value)

return Types
