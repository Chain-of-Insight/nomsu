-- This file contains the source code of the Nomsu compiler.
-- Nomsu is a programming language that cross-compiles to Lua. It was designed to be good
-- at natural-language-like code that is highly self-modifying and flexible.
-- The only dependency is LPEG, which can be installed using "luarocks install lpeg"
-- File usage:
--    Either, in a lua/moonscript file:
--        Nomsu = require "nomsu"
--        nomsu = Nomsu()
--        nomsu:run(your_nomsu_code)
--    Or from the command line:
--        lua nomsu.lua your_file.nom
lpeg = require 'lpeg'
{:R,:P,:S} = lpeg
re = require 're'
{:List, :Dict, :Text} = require 'containers'
{:insert, :remove, :concat} = table
unpack or= table.unpack
{:match, :sub, :gsub, :format, :byte, :find} = string
{:LuaCode, :Source} = require "code_obj"
SyntaxTree = require "syntax_tree"
{:Importer, :import_to_1_from, :_1_forked} = require 'importer'

table.map = (t, fn)-> setmetatable([fn(v) for _,v in ipairs(t)], getmetatable(t))

-- TODO:
-- Re-implement nomsu-to-lua comment translation?

-- TODO: de-duplicate this
pretty_error = require("pretty_errors")
compile_error = (tree, err_msg, hint=nil)->
    err_str = pretty_error{
        title: "Compile error"
        error:err_msg, hint:hint, source:tree\get_source_file!
        start:tree.source.start, stop:tree.source.stop, filename:tree.source.filename
    }
    error(err_str, 0)
{:tree_to_nomsu, :tree_to_inline_nomsu} = require "nomsu_decompiler"

-- This is a bit of a hack, but this code handles arbitrarily complex
-- math expressions like 2*x + 3^2 without having to define a single
-- action for every possibility.
math_expression = re.compile [[ (([*/^+-] / [0-9]+) " ")* [*/^+-] !. ]]
compile_math_expression = (compile, tree, ...)->
    lua = LuaCode(tree.source)
    for i,tok in ipairs tree
        if type(tok) == 'string'
            lua\append tok
        else
            tok_lua = compile(tok)
            tok_lua\parenthesize! if tok.type == "Action"
            lua\append tok_lua
        lua\append " " if i < #tree
    return lua

MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value
compile = setmetatable({
    action: Importer{
        [""]: (compile, tree, fn, ...)->
            lua = LuaCode(tree.source)
            fn_lua = compile(fn)
            lua\append fn_lua
            unless fn_lua\text!\match("^%(.*%)$") or fn_lua\text!\match("^[_a-zA-Z][_a-zA-Z0-9.]*$")
                lua\parenthesize!
            lua\append "("
            for i=1,select('#',...)
                lua\append(", ") if i > 1
                lua\append compile(select(i, ...))
            lua\append ")"
            return lua

        ["Lua"]: (compile, tree, code)->
            if code.type != "Text"
                return LuaCode(code.source, "LuaCode(", tostring(code.source)\as_lua!, ", ", compile(code), ")")
            add_bit_lua = (lua, bit_lua)->
                bit_leading_len = #(bit_lua\match("^[^\n]*"))
                lua\append(lua\trailing_line_len! + bit_leading_len > MAX_LINE and ",\n    " or ", ")
                lua\append(bit_lua)
            operate_on_text = (text)->
                lua = LuaCode(text.source, "LuaCode(", tostring(text.source)\as_lua!)
                for bit in *text
                    if type(bit) == "string"
                        add_bit_lua(lua, bit\as_lua!)
                    elseif bit.type == "Text"
                        add_bit_lua(lua, operate_on_text(bit))
                    else
                        add_bit_lua(lua, compile(bit))
                lua\append ")"
                return lua
            return operate_on_text code

        ["lua >"]: (compile, tree, code)->
            if code.type != "Text"
                return tree
            operate_on_text = (text)->
                lua = LuaCode(text.source)
                for bit in *text
                    if type(bit) == "string"
                        lua\append bit
                    elseif bit.type == "Text"
                        lua\append(operate_on_text(bit))
                    else
                        lua\append compile(bit)
                return lua
            return operate_on_text code

        ["= lua"]: (compile, tree, code)-> compile.action["lua >"](compile, tree, code)

        ["use"]: (compile, tree, path)->
            --if path.type == 'Text' and #path == 1 and type(path[1]) == 'string'
            --    unless import_to_1_from(compile, path[1])
            --        compile_error tree, "Could not find anything to import for #{path}"
            return LuaCode(tree.source, "run_file_1_in(#{compile(path)}, _ENV)")

        ["tests"]: (compile, tree)-> LuaCode(tree.source, "TESTS")
        ["test"]: (compile, tree, body)->
            unless body.type == 'Block'
                compile_error(tree, "This should be a Block")
            test_nomsu = body\get_source_code!\match(":[ ]*(.*)")
            if indent = test_nomsu\match("\n([ ]*)")
                test_nomsu = test_nomsu\gsub("\n"..indent, "\n")
            LuaCode tree.source, "TESTS[#{tostring(tree.source)\as_lua!}] = ", test_nomsu\as_lua!

        ["is jit"]: (compile, tree, code)-> LuaCode(tree.source, "jit")
        ["Lua version"]: (compile, tree, code)-> LuaCode(tree.source, "_VERSION")
        ["nomsu environment"]: (compile, tree)-> LuaCode(tree.source, "_ENV")
    }
}, {
    __import: (other)=>
        import_to_1_from(@action, other.action)
        return
    __call: (compile, tree, force_value=false)->
        if tree.version
            if get_version = compile.action[("Nomsu version")\as_lua_id!]
                if upgrade = compile.action[("1 upgraded from 2 to")\as_lua_id!]
                    tree = upgrade(tree, tree.version, get_version!)
        switch tree.type
            when "Action"
                stub = tree.stub
                compile_action = compile.action[stub]
                if not compile_action and math_expression\match(stub)
                    compile_action = compile_math_expression
                if compile_action and not tree.target
                    args = [arg for arg in *tree when type(arg) != "string"]
                    -- Force Lua to avoid tail call optimization for debugging purposes
                    -- TODO: use tail call?
                    ret = compile_action(compile, tree, unpack(args))
                    if ret == nil
                        info = debug.getinfo(compile_action, "S")
                        filename = Source\from_string(info.source).filename
                        compile_error tree,
                            "The compile-time action here (#{stub}) failed to return any value.",
                            "Look at the implementation of (#{stub}) in #{filename}:#{info.linedefined} and make sure it's returning something."
                    unless SyntaxTree\is_instance(ret)
                        return ret
                    if ret != tree
                        return compile(ret)

                lua = LuaCode(tree.source)
                if tree.target -- Method call
                    target_lua = compile tree.target
                    target_text = target_lua\text!
                    if target_text\match("^%(.*%)$") or target_text\match("^[_a-zA-Z][_a-zA-Z0-9.]*$")
                        lua\append target_lua, ":"
                    else
                        lua\append "(", target_lua, "):"
                lua\append((stub)\as_lua_id!,"(")
                args = {}
                for i, tok in ipairs tree
                    if type(tok) == "string" then continue
                    arg_lua = compile(tok, true)
                    insert args, arg_lua
                lua\concat_append args, ", "
                lua\append ")"
                return lua

            when "EscapedNomsu"
                lua = LuaCode tree.source, "SyntaxTree{"
                needs_comma, i = false, 1
                as_lua = (x)->
                    if type(x) == 'number'
                        tostring(x)
                    elseif SyntaxTree\is_instance(x)
                        compile(x)
                    else x\as_lua!

                for k,v in pairs((SyntaxTree\is_instance(tree[1]) and tree[1].type == "EscapedNomsu" and tree) or tree[1])
                    if needs_comma then lua\append ", "
                    else needs_comma = true
                    if k == i
                        i += 1
                    elseif type(k) == 'string' and match(k,"[_a-zA-Z][_a-zA-Z0-9]*")
                        lua\append(k, "= ")
                    else
                        lua\append("[", as_lua(k), "]= ")
                    if k == "source"
                        lua\append tostring(v)\as_lua!
                    else
                        lua\append as_lua(v)
                lua\append "}"
                return lua
            
            when "Block"
                if not force_value
                    lua = LuaCode(tree.source)
                    lua\concat_append([compile(line) for line in *tree], "\n")
                    return lua
                else
                    lua = LuaCode(tree.source)
                    lua\append("((function()")
                    for i, line in ipairs(tree)
                        lua\append "\n    ", compile(line)
                    lua\append("\nend)())")
                    return lua

            when "Text"
                lua = LuaCode(tree.source)
                string_buffer = ""
                for i, bit in ipairs tree
                    if type(bit) == "string"
                        string_buffer ..= bit
                        continue
                    if string_buffer != ""
                        if #lua.bits > 0 then lua\append ".."
                        lua\append string_buffer\as_lua!
                        string_buffer = ""
                    bit_lua = compile(bit)
                    if #lua.bits > 0 then lua\append ".."
                    if bit.type != "Text"
                        bit_lua = LuaCode(bit.source, "tostring(",bit_lua,")")
                    lua\append bit_lua

                if string_buffer ~= "" or #lua.bits == 0
                    if #lua.bits > 0 then lua\append ".."
                    lua\append string_buffer\as_lua!

                if #lua.bits > 1
                    lua\parenthesize!
                return lua

            when "List", "Dict"
                lua = LuaCode tree.source, "#{tree.type}{"
                i = 1
                sep = ''
                while i <= #tree
                    item = tree[i]
                    if item.type == "Block"
                        break
                    lua\append sep
                    if item.type == "Comment"
                        lua\append compile(item), "\n"
                        sep = ''
                    else
                        item_lua = compile(item)
                        lua\append item_lua
                        sep = ', '
                    i += 1
                lua\append "}"
                if i <= #tree
                    lua = LuaCode tree.source, "(function()\n    local it = ", lua
                    while i <= #tree
                        lua\append "\n    "
                        if tree[i].type == 'Block' or tree[i].type == 'Comment'
                            lua\append compile(tree[i])
                        elseif tree[i].type == "DictEntry"
                            lua\append "it[ ", compile(tree[i][1]), "] = ", (tree[i][2] and compile(tree[i][2]) or "true")
                        else
                            lua\append "it:add(", compile(tree[i]), ")"
                        i += 1
                    lua\append "\n    return it\nend)()"
                return lua
                --lua = LuaCode tree.source, "#{tree.type}{"
                --lua\concat_append([compile(e) for e in *tree when e.type != 'Comment'], ", ", ",\n  ")
                --lua\append "}"
                --return lua

            when "DictEntry"
                key, value = tree[1], tree[2]
                key_lua = compile(key)
                value_lua = value and compile(value) or LuaCode(key.source, "true")
                key_str = match(key_lua\text!, [=[^["']([a-zA-Z_][a-zA-Z0-9_]*)['"]$]=])
                return if key_str and key_str\is_lua_id!
                    LuaCode tree.source, key_str,"=",value_lua
                elseif sub(key_lua\text!,1,1) == "["
                    -- NOTE: this *must* use a space after the [ to avoid freaking out
                    -- Lua's parser if the inner expression is a long string. Lua
                    -- parses x[[[y]]] as x("[y]"), not as x["y"]
                    LuaCode tree.source, "[ ",key_lua,"]=",value_lua
                else
                    LuaCode tree.source, "[",key_lua,"]=",value_lua
            
            when "IndexChain"
                lua = compile(tree[1])
                first_char = sub(lua\text!,1,1)
                if first_char == "{" or first_char == '"' or first_char == "["
                    lua\parenthesize!

                for i=2,#tree
                    key = tree[i]
                    key_lua = compile(key)
                    key_lua_str = key_lua\text!
                    lua_id = match(key_lua_str, "^['\"]([a-zA-Z_][a-zA-Z0-9_]*)['\"]$")
                    if lua_id and lua_id\is_lua_id!
                        lua\append ".#{lua_id}"
                    elseif sub(key_lua_str,1,1) == '['
                        -- NOTE: this *must* use a space after the [ to avoid freaking out
                        -- Lua's parser if the inner expression is a long string. Lua
                        -- parses x[[[y]]] as x("[y]"), not as x["y"]
                        lua\append "[ ",key_lua," ]"
                    else
                        lua\append "[",key_lua,"]"
                return lua

            when "Number"
                return LuaCode(tree.source, tostring(tree[1]))

            when "Var"
                return LuaCode(tree.source, (tree[1])\as_lua_id!)

            when "FileChunks"
                error("Can't convert FileChunks to a single block of lua, since each chunk's "..
                    "compilation depends on the earlier chunks")
            
            when "Comment"
                -- TODO: implement?
                return LuaCode(tree.source, "")
            
            when "Error"
                error("Can't compile errors")

            else
                error("Unknown type: #{tree.type}")

})

return compile
