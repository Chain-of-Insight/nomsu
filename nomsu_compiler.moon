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
Files = require "files"

table.map = (t, fn)-> setmetatable([fn(v) for _,v in ipairs(t)], getmetatable(t))

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
math_expression = re.compile [[ (([*/^+-] / [0-9]+) " ")* [*/^+-] !. ]]

MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value
compile = setmetatable({
    action: Importer{
        [""]: (compile, fn, ...)->
            lua = LuaCode!
            fn_lua = compile(fn)
            lua\append fn_lua
            unless fn_lua\text!\match("^%(.*%)$") or fn_lua\text!\match("^[_a-zA-Z][_a-zA-Z0-9.]*$")
                lua\parenthesize!
            lua\append "("
            for i=1,select('#',...)
                lua\append(", ") if i > 1
                lua\append compile((select(i, ...)))
            lua\append ")"
            return lua

        ["Lua"]: (compile, code)->
            if not code
                return LuaCode("LuaCode()")
            if code.type != "Text"
                return LuaCode("LuaCode:from(", tostring(code.source)\as_lua!, ", ", compile(code), ")")
            add_bit_lua = (lua, bit_lua)->
                bit_leading_len = #(bit_lua\match("^[^\n]*"))
                lua\append(lua\trailing_line_len! + bit_leading_len > MAX_LINE and ",\n    " or ", ")
                lua\append(bit_lua)
            operate_on_text = (text)->
                lua = LuaCode\from(text.source, "LuaCode:from(", tostring(text.source)\as_lua!)
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

        ["lua >"]: (compile, code)->
            if code.type != "Text"
                return code
            operate_on_text = (text)->
                lua = LuaCode\from(text.source)
                for bit in *text
                    if type(bit) == "string"
                        lua\append bit
                    elseif bit.type == "Text"
                        lua\append(operate_on_text(bit))
                    else
                        lua\append compile(bit)
                return lua
            return operate_on_text code

        ["= lua"]: (compile, code)-> compile.action["lua >"](compile, code)

        ["use"]: (compile, path)-> LuaCode("run_file_1_in(#{compile(path)}, _ENV, OPTIMIZATION)")

        ["use 1 with prefix"]: (compile, path, prefix)->
            LuaCode("run_file_1_in(#{compile(path)}, _ENV, OPTIMIZATION, ", compile(prefix), ")")

        ["tests"]: (compile)-> LuaCode("TESTS")
        ["test"]: (compile, body)->
            unless body.type == 'Block'
                compile_error(body, "This should be a Block")
            test_nomsu = body\get_source_code!\match(":[ ]*(.*)")
            if indent = test_nomsu\match("\n([ ]*)")
                test_nomsu = test_nomsu\gsub("\n"..indent, "\n")
            return LuaCode "TESTS[#{tostring(body.source)\as_lua!}] = ", test_nomsu\as_lua!

        ["is jit"]: (compile, code)-> LuaCode("jit")
        ["Lua version"]: (compile, code)-> LuaCode("_VERSION")
        ["nomsu environment"]: (compile)-> LuaCode("_ENV")
    }
}, {
    __import: import_to_1_from
    __call: (compile, tree)->
        switch tree.type
            when "Action"
                stub = tree.stub
                compile_action = compile.action[stub]
                if not compile_action and not tree.target and math_expression\match(stub)
                    lua = LuaCode\from(tree.source)
                    for i,tok in ipairs tree
                        if type(tok) == 'string'
                            lua\append tok
                        else
                            tok_lua = compile(tok)
                            tok_lua\parenthesize! if tok.type == "Action"
                            lua\append tok_lua
                        lua\append " " if i < #tree
                    return lua

                if compile_action and not tree.target
                    args = [arg for arg in *tree when type(arg) != "string"]
                    -- Force Lua to avoid tail call optimization for debugging purposes
                    -- TODO: use tail call?
                    ret = compile_action(compile, unpack(args))
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
                        return compile(ret)

                lua = LuaCode\from(tree.source)
                if tree.target -- Method call
                    target_lua = compile tree.target
                    target_text = target_lua\text!
                    -- TODO: this parenthesizing is maybe overly conservative
                    if target_text\match("^%(.*%)$") or target_text\match("^[_a-zA-Z][_a-zA-Z0-9.]*$") or
                        tree.target.type == "IndexChain"
                        lua\append target_lua, ":"
                    else
                        lua\append "(", target_lua, "):"
                lua\append((stub)\as_lua_id!,"(")
                args = {}
                for i, tok in ipairs tree
                    if type(tok) == "string" then continue
                    arg_lua = compile(tok)
                    if tok.type == "Block"
                        arg_lua = LuaCode\from(tok.source, "(function()\n    ", arg_lua, "\nend)()")
                    insert args, arg_lua
                lua\concat_append args, ", "
                lua\append ")"
                return lua

            when "EscapedNomsu"
                lua = LuaCode\from tree.source, "SyntaxTree{"
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
                lua = LuaCode\from(tree.source)
                for i, line in ipairs tree
                    if i > 1 then lua\append "\n"
                    lua\append compile(line)
                return lua

            when "Text"
                lua = LuaCode\from(tree.source)
                added = 0
                string_buffer = ""
                for i, bit in ipairs tree
                    if type(bit) == "string"
                        string_buffer ..= bit
                        continue
                    if string_buffer != ""
                        string_buffer = string_buffer\as_lua!
                        if lua\trailing_line_len! + #string_buffer > MAX_LINE
                            lua\append "\n  "
                        if added > 0 then lua\append ".."
                        lua\append string_buffer
                        added += 1
                        string_buffer = ""
                    bit_lua = compile(bit)
                    if lua\trailing_line_len! + #bit_lua\text! > MAX_LINE
                        lua\append "\n  "
                    if added > 0 then lua\append ".."
                    if bit.type != "Text"
                        bit_lua = LuaCode\from(bit.source, "tostring(",bit_lua,")")
                    lua\append bit_lua
                    added += 1

                if string_buffer ~= "" or #lua.bits == 0
                    string_buffer = string_buffer\as_lua!
                    if lua\trailing_line_len! + #string_buffer > MAX_LINE
                        lua\append "\n  "
                    if added > 0 then lua\append ".."
                    lua\append string_buffer
                    added += 1

                if #lua.bits > 1
                    lua\parenthesize!
                return lua

            when "List", "Dict"
                lua = LuaCode\from tree.source
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

                if lua\is_multiline!
                    lua = LuaCode\from tree.source, "#{tree.type}{\n    ", lua, "\n}"
                else
                    lua = LuaCode\from tree.source, "#{tree.type}{", lua, "}"

                -- List/dict comprehenstion
                if i <= #tree
                    lua = LuaCode\from tree.source, "(function()\n    local comprehension = ", lua
                    if tree.type == "List"
                        lua\append "\n    local function add(x) comprehension[#comprehension+1] = x end"
                    else
                        lua\append "\n    local function #{("add 1 =")\as_lua_id!}(k, v) comprehension[k] = v end"
                    while i <= #tree
                        lua\append "\n    "
                        if tree[i].type == 'Block' or tree[i].type == 'Comment'
                            lua\append compile(tree[i])
                        elseif tree[i].type == "DictEntry"
                            entry_lua = compile(tree[i])
                            lua\append (entry_lua\text!\sub(1,1) == '[' and "comprehension" or "comprehension."), entry_lua
                        else
                            lua\append "comprehension[#comprehension+1] = ", compile(tree[i])
                        i += 1
                    lua\append "\n    return comprehension\nend)()"
                
                return lua

            when "DictEntry"
                key, value = tree[1], tree[2]
                key_lua = compile(key)
                value_lua = value and compile(value) or LuaCode\from(key.source, "true")
                key_str = match(key_lua\text!, [=[^["']([a-zA-Z_][a-zA-Z0-9_]*)['"]$]=])
                return if key_str and key_str\is_lua_id!
                    LuaCode\from tree.source, key_str,"=",value_lua
                elseif sub(key_lua\text!,1,1) == "["
                    -- NOTE: this *must* use a space after the [ to avoid freaking out
                    -- Lua's parser if the inner expression is a long string. Lua
                    -- parses x[[[y]]] as x("[y]"), not as x["y"]
                    LuaCode\from tree.source, "[ ",key_lua,"]=",value_lua
                else
                    LuaCode\from tree.source, "[",key_lua,"]=",value_lua
            
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
                return LuaCode\from(tree.source, tostring(tree[1]))

            when "Var"
                return LuaCode\from(tree.source, (concat(tree, " "))\as_lua_id!)

            when "FileChunks"
                error("Can't convert FileChunks to a single block of lua, since each chunk's "..
                    "compilation depends on the earlier chunks")
            
            when "Comment"
                return LuaCode\from(tree.source, "-- ", (tree[1]\gsub('\n', '\n-- ')))
            
            when "Error"
                error("Can't compile errors")

            else
                error("Unknown type: #{tree.type}")

})

return {:compile, :compile_error}
