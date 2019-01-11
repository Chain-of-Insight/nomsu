-- 
-- This file contains the source code of the Nomsu compiler.
--
{:List, :Dict, :Text} = require 'containers'
unpack or= table.unpack
{:match, :sub, :gsub, :format, :byte, :find} = string
{:LuaCode, :Source} = require "code_obj"
SyntaxTree = require "syntax_tree"
Files = require "files"

-- TODO: de-duplicate this
pretty_error = require("pretty_errors")
compile_error = (source, err_msg, hint=nil)->
    local file
    if SyntaxTree\is_instance(source)
        file = source\get_source_file!
        source = source.source
    elseif type(source) == 'string'
        source = Source\from_string(source)
    if source and not file
        file = Files.read(source.filename)

    err_str = pretty_error{
        title: "Compile error"
        error:err_msg, hint:hint, source:file
        start:source.start, stop:source.stop, filename:source.filename
    }
    error(err_str, 0)

-- This is a bit of a hack, but this code handles arbitrarily complex
-- math expressions like 2*x + 3^2 without having to define a single
-- action for every possibility.
re = require 're'
math_expression = re.compile [[ (([*/^+-] / [0-9]+) " ")* [*/^+-] !. ]]

MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value
compile = (tree)=>
    switch tree.type
        when "Action"
            stub = tree.stub
            compile_action = @COMPILE_RULES[stub]
            if not compile_action and math_expression\match(stub)
                lua = LuaCode\from(tree.source)
                for i,tok in ipairs tree
                    if type(tok) == 'string'
                        lua\add tok
                    else
                        tok_lua = @compile(tok)
                        -- TODO: this is overly eager, should be less aggressive
                        tok_lua\parenthesize! if tok.type == "Action"
                        lua\add tok_lua
                    lua\add " " if i < #tree
                return lua

            if compile_action
                args = [arg for arg in *tree when type(arg) != "string"]
                -- Force Lua to avoid tail call optimization for debugging purposes
                -- TODO: use tail call?
                ret = compile_action(@, unpack(args))
                if ret == nil
                    info = debug.getinfo(compile_action, "S")
                    filename = Source\from_string(info.source).filename
                    compile_error tree,
                        "The compile-time action here (#{stub}) failed to return any value.",
                        "Look at the implementation of (#{stub}) in #{filename}:#{info.linedefined} and make sure it's returning something."
                unless SyntaxTree\is_instance(ret)
                    ret.source or= tree.source
                    return ret
                if ret != tree
                    return @compile(ret)

            lua = LuaCode\from(tree.source)
            lua\add((stub)\as_lua_id!,"(")
            for argnum, arg in ipairs tree\get_args!
                arg_lua = @compile(arg)
                if arg.type == "Block"
                    arg_lua = LuaCode\from(arg.source, "(function()\n    ", arg_lua, "\nend)()")
                if lua\trailing_line_len! + #arg_lua\text! > MAX_LINE
                    lua\add(argnum > 1 and ",\n    " or "\n    ")
                elseif argnum > 1
                    lua\add ", "
                lua\add arg_lua
            lua\add ")"
            return lua

        when "MethodCall"
            lua = LuaCode\from tree.source
            target_lua = @compile tree[1]
            target_text = target_lua\text!
            -- TODO: this parenthesizing is maybe overly conservative
            if not (target_text\match("^%(.*%)$") or target_text\match("^[_a-zA-Z][_a-zA-Z0-9.]*$") or
                tree[1].type == "IndexChain")
                target_lua\parenthesize!

            for i=2,#tree
                lua\add "\n" if i > 2
                lua\add target_lua, ":"
                lua\add((tree[i].stub)\as_lua_id!,"(")
                for argnum, arg in ipairs tree[i]\get_args!
                    arg_lua = @compile(arg)
                    if arg.type == "Block"
                        arg_lua = LuaCode\from(arg.source, "(function()\n    ", arg_lua, "\nend)()")
                    if lua\trailing_line_len! + #arg_lua\text! > MAX_LINE
                        lua\add(argnum > 1 and ",\n    " or "\n    ")
                    elseif argnum > 1
                        lua\add ", "
                    lua\add arg_lua
                lua\add ")"
            return lua

        when "EscapedNomsu"
            lua = LuaCode\from tree.source, "SyntaxTree{"
            needs_comma, i = false, 1
            as_lua = (x)->
                if type(x) == 'number'
                    tostring(x)
                elseif SyntaxTree\is_instance(x)
                    @compile(x)
                elseif Source\is_instance(x)
                    tostring(x)\as_lua!
                else x\as_lua!

            for k,v in pairs((SyntaxTree\is_instance(tree[1]) and tree[1].type == "EscapedNomsu" and tree) or tree[1])
                entry_lua = LuaCode!
                if k == i
                    i += 1
                elseif type(k) == 'string' and match(k,"[_a-zA-Z][_a-zA-Z0-9]*")
                    entry_lua\add(k, "= ")
                else
                    entry_lua\add("[", as_lua(k), "]= ")
                entry_lua\add as_lua(v)
                if needs_comma then lua\add ","
                if lua\trailing_line_len! + #(entry_lua\text!\match("^[\n]*")) > MAX_LINE
                    lua\add "\n    "
                elseif needs_comma
                    lua\add " "
                lua\add entry_lua
                needs_comma = true
            lua\add "}"
            return lua
        
        when "Block"
            lua = LuaCode\from(tree.source)
            for i, line in ipairs tree
                if i > 1 then lua\add "\n"
                lua\add @compile(line)
            return lua

        when "Text"
            lua = LuaCode\from(tree.source)
            added = 0
            string_buffer = ""
            add_bit = (bit)->
                if added > 0
                    if lua\trailing_line_len! + #bit > MAX_LINE
                        lua\add "\n  "
                    lua\add ".."
                lua\add bit
                added += 1

            for i, bit in ipairs tree
                if type(bit) == "string"
                    string_buffer ..= bit
                    continue
                if string_buffer != ""
                    for i=1,#string_buffer,MAX_LINE
                        add_bit string_buffer\sub(i, i+MAX_LINE-1)\as_lua!
                    string_buffer = ""

                bit_lua = @compile(bit)
                if bit.type == "Block" and #bit == 1
                    bit = bit[1]
                if bit.type == "Block"
                    bit_lua = LuaCode\from bit.source, "List(function(add)",
                        "\n    ", bit_lua,
                        "\nend):joined()"
                elseif bit.type != "Text"
                    bit_lua = LuaCode\from(bit.source, "tostring(",bit_lua,")")
                add_bit bit_lua

            if string_buffer != ""
                for i=1,#string_buffer,MAX_LINE
                    add_bit string_buffer\sub(i, i+MAX_LINE-1)\as_lua!
                string_buffer = ""

            if added == 0
                add_bit '""'
            if added > 1
                lua\parenthesize!
            return lua

        when "List", "Dict"
            if #tree == 0
                return LuaCode\from tree.source, tree.type, "{}"

            lua = LuaCode\from tree.source
            chunks = 0
            i = 1
            while tree[i]
                if tree[i].type == 'Block'
                    lua\add " + " if chunks > 0
                    lua\add tree.type, "(function(", (tree.type == 'List' and "add" or ("add, "..("add 1 =")\as_lua_id!)), ")"
                    lua\add "\n    ", @compile(tree[i]), "\nend)"
                    chunks += 1
                    i += 1
                else
                    lua\add " + " if chunks > 0
                    sep = ''
                    items_lua = LuaCode\from tree[i].source
                    while tree[i]
                        if tree[i].type == "Block"
                            break
                        item_lua = @compile tree[i]
                        if item_lua\text!\match("^%.[a-zA-Z_]")
                            item_lua = item_lua\text!\sub(2)
                        if tree.type == 'Dict' and tree[i].type == 'Index'
                            item_lua = LuaCode\from tree[i].source, item_lua, "=true"
                        items_lua\add sep, item_lua
                        if tree[i].type == "Comment"
                            items_lua\add "\n"
                            sep = ''
                        elseif items_lua\trailing_line_len! > MAX_LINE
                            sep = ',\n    '
                        else
                            sep = ', '
                        i += 1
                    if items_lua\is_multiline!
                        lua\add LuaCode\from items_lua.source, tree.type, "{\n    ", items_lua, "\n}"
                    else
                        lua\add LuaCode\from items_lua.source, tree.type, "{", items_lua, "}"
                    chunks += 1
            
            return lua

        when "Index"
            key_lua = @compile(tree[1])
            key_str = match(key_lua\text!, '^"([a-zA-Z_][a-zA-Z0-9_]*)"$')
            return if key_str and key_str\is_lua_id!
                LuaCode\from tree.source, ".", key_str
            elseif sub(key_lua\text!,1,1) == "["
                -- NOTE: this *must* use a space after the [ to avoid freaking out
                -- Lua's parser if the inner expression is a long string. Lua
                -- parses x[[[y]]] as x("[y]"), not as x["y"]
                LuaCode\from tree.source, "[ ",key_lua,"]"
            else
                LuaCode\from tree.source, "[",key_lua,"]"

        when "DictEntry"
            key = tree[1]
            if key.type != "Index"
                key = SyntaxTree{type:"Index", source:key.source, key}
            return LuaCode\from tree.source, @compile(key),"=",(tree[2] and @compile(tree[2]) or "true")
        
        when "IndexChain"
            lua = @compile(tree[1])
            if lua\text!\match("['\"}]$") or lua\text!\match("]=*]$")
                lua\parenthesize!
            for i=2,#tree
                key = tree[i]
                -- TODO: remove this shim
                if key.type != "Index"
                    key = SyntaxTree{type:"Index", source:key.source, key}
                lua\add @compile(key)
            return lua

        when "Number"
            return LuaCode\from(tree.source, tostring(tree[1]))

        when "Var"
            return LuaCode\from(tree.source, tree\as_var!\as_lua_id!)

        when "FileChunks"
            error("Can't convert FileChunks to a single block of lua, since each chunk's "..
                "compilation depends on the earlier chunks")
        
        when "Comment"
            return LuaCode\from(tree.source, "-- ", (tree[1]\gsub('\n', '\n-- ')))
        
        when "Error"
            error("Can't compile errors")

        else
            error("Unknown type: #{tree.type}")

return {:compile, :compile_error}
