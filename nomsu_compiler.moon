-- 
-- This file contains the source code of the Nomsu compiler.
--
unpack or= table.unpack
{:match, :sub, :gsub, :format, :byte, :find} = string
{:LuaCode, :Source} = require "code_obj"
require "text"
SyntaxTree = require "syntax_tree"
Files = require "files"

pretty_error = require("pretty_errors")
fail_at = (source, msg)->
    local file
    if SyntaxTree\is_instance(source)
        file = source\get_source_file!
        source = source.source
    elseif type(source) == 'string'
        source = Source\from_string(source)
    elseif not Source\is_instance(source)
        -- debug.getinfo() output:
        assert(source.short_src and source.currentline)
        file = Files.read(source.short_src)
        assert file, "Could not find #{source.short_src}"
        lines = file\lines!
        start = 1
        for i=1,source.currentline-1
            start += #lines[i]
        stop = start + #lines[source.currentline]
        source = Source(source.short_src, start, stop)

    if source and not file
        file = Files.read(source.filename)

    title, err_msg, hint = msg\match("([^:]*):[ \n]+(.*)[ \n]+Hint: (.*)")
    if not err_msg
        err_msg, hint = msg\match("(.*)[ \n]+Hint:[ \n]+(.*)")
        title = "Failure"
    if not err_msg
        title, err_msg = msg\match("([^:]*):[ \n]+(.*)")
    if not err_msg
        err_msg = msg
        title = "Failure"

    err_str = pretty_error{
        title: title,
        error: err_msg, hint: hint, source: file,
        start:source.start, stop:source.stop, filename:source.filename,
    }
    error(err_str, 0)

-- This is a bit of a hack, but this code handles arbitrarily complex
-- math expressions like 2*x + 3^2 without having to define a single
-- action for every possibility.
re = require 're'
math_expression = re.compile [[ (([*/^+-] / [0-9]+) " ")* [*/^+-] !. ]]

MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value
compile = (tree)=>
    -- Automatically upgrade trees from older versions:
    if tree.version and tree.version < @NOMSU_VERSION\up_to(#tree.version) and @_1_upgraded_from_2_to
        tree = @._1_upgraded_from_2_to(tree, tree.version, @NOMSU_VERSION)

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

            if not compile_action
                seen_words = {}
                words = {}
                for word in stub\gmatch("[^0-9 ][^ ]*")
                    unless seen_words[word]
                        seen_words[word] = true
                        table.insert words, word
                table.sort(words)
                stub2 = table.concat(words, " ")
                compile_action = @COMPILE_RULES[stub2]
                if compile_action
                    if debug.getinfo(compile_action, 'u').isvararg
                        stub = stub2
                    else compile_action = nil

            if compile_action
                args = [arg for arg in *tree when type(arg) != "string"]
                ret = compile_action(@, tree, unpack(args))
                if ret == nil
                    info = debug.getinfo(compile_action, "S")
                    filename = Source\from_string(info.source).filename
                    fail_at tree,
                        ("Compile error: The compile-time action here (#{stub}) failed to return any value. "..
                        "Hint: Look at the implementation of (#{stub}) in #{filename}:#{info.linedefined} and make sure it's returning something.")
                unless SyntaxTree\is_instance(ret)
                    ret.source or= tree.source
                    return ret
                if ret != tree
                    return @compile(ret)

            lua = LuaCode\from(tree.source)
            lua\add((stub)\as_lua_id!,"(")
            for argnum, arg in ipairs tree\get_args!
                arg_lua = @compile(arg)
                if arg.type == "Block" and #arg > 1
                    arg_lua = LuaCode\from(arg.source, "(function()\n    ", arg_lua, "\nend)()")
                if lua\trailing_line_len! + #arg_lua > MAX_LINE
                    lua\add(argnum > 1 and ",\n    " or "\n    ")
                elseif argnum > 1
                    lua\add ", "
                lua\add arg_lua
            lua\add ")"
            return lua

        when "MethodCall"
            stub = tree\get_stub!
            compile_action = @COMPILE_RULES[stub]
            if compile_action
                args = tree\get_args!
                ret = compile_action(@, tree, unpack(args))
                if ret == nil
                    info = debug.getinfo(compile_action, "S")
                    filename = Source\from_string(info.source).filename
                    fail_at tree,
                        ("Compile error: The compile-time method here (#{stub}) failed to return any value. "..
                        "Hint: Look at the implementation of (#{stub}) in #{filename}:#{info.linedefined} "..
                        "and make sure it's returning something.")
                unless SyntaxTree\is_instance(ret)
                    ret.source or= tree.source
                    return ret
                if ret != tree
                    return @compile(ret)

            lua = LuaCode\from tree.source
            target_lua = @compile tree[1]
            target_text = target_lua\text!
            -- TODO: this parenthesizing is maybe overly conservative
            if not (target_text\match("^%(.*%)$") or target_text\match("^[_a-zA-Z][_a-zA-Z0-9.]*$") or
                tree[1].type == "IndexChain")
                target_lua\parenthesize!

            self_lua = #tree > 2 and "_self" or target_lua
            if #tree > 2
                lua\add "(function(", self_lua, ")\n    "
            for i=2,#tree
                lua\add "\n    " if i > 2
                if i > 2 and i == #tree
                    lua\add "return "
                lua\add self_lua, ":"
                lua\add((tree[i].stub)\as_lua_id!,"(")
                for argnum, arg in ipairs tree[i]\get_args!
                    arg_lua = @compile(arg)
                    if arg.type == "Block" and #arg > 1
                        arg_lua = LuaCode\from(arg.source, "(function()\n    ", arg_lua, "\nend)()")
                    if lua\trailing_line_len! + #arg_lua > MAX_LINE
                        lua\add(argnum > 1 and ",\n    " or "\n    ")
                    elseif argnum > 1
                        lua\add ", "
                    lua\add arg_lua
                lua\add ")"
            if #tree > 2
                lua\add "\nend)(", target_lua, ")"
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
                if lua\trailing_line_len! + #(entry_lua\match("^[\n]*")) > MAX_LINE
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
                line_lua = @compile(line)
                lua\add line_lua
                unless line_lua\last(1) == ";" or line_lua\last(4)\match("[^_a-zA-Z0-9]end$")
                    lua\add ";"
            return lua

        when "Text"
            if #tree == 0
                return LuaCode\from(tree.source, '""')
            if #tree == 1 and type(tree[1]) == 'string'
                return LuaCode\from(tree.source, tree[1]\as_lua!)
            lua = LuaCode\from(tree.source, "Text(")
            added = 0
            string_buffer = ""
            add_bit = (bit)->
                if added > 0
                    if lua\trailing_line_len! + #bit > MAX_LINE
                        lua\add ",\n  "
                    else
                        lua\add ", "
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
                if bit.type == "Block"
                    bit_lua = LuaCode\from bit.source, "a_List(function(add)",
                        "\n    ", bit_lua,
                        "\nend):joined()"
                add_bit bit_lua

            if string_buffer != ""
                for i=1,#string_buffer,MAX_LINE
                    add_bit string_buffer\sub(i, i+MAX_LINE-1)\as_lua!
                string_buffer = ""

            if added == 0
                return LuaCode\from(tree.source, '""')
            lua\add ")"
            return lua

        when "List", "Dict"
            typename = "a_"..tree.type
            if #tree == 0
                return LuaCode\from tree.source, typename, "{}"

            lua = LuaCode\from tree.source
            chunks = 0
            i = 1
            while tree[i]
                if tree[i].type == 'Block'
                    lua\add " + " if chunks > 0
                    lua\add typename, "(function(", (tree.type == 'List' and "add" or ("add, "..("add 1 =")\as_lua_id!)), ")"
                    body = @compile(tree[i])
                    body\declare_locals!
                    lua\add "\n    ", body, "\nend)"
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
                        if item_lua\match("^%.[a-zA-Z_]")
                            item_lua = item_lua\text!\sub(2)
                        if tree.type == 'Dict' and tree[i].type == 'Index'
                            item_lua = LuaCode\from tree[i].source, item_lua, "=true"
                        items_lua\add sep, item_lua
                        if tree[i].type == "Comment"
                            items_lua\add "\n"
                            sep = ''
                        elseif items_lua\trailing_line_len! > MAX_LINE
                            sep = items_lua\last(1) == ";" and "\n    " or ",\n    "
                        else
                            sep = items_lua\last(1) == ";" and " " or ", "
                        i += 1
                    if items_lua\is_multiline!
                        lua\add LuaCode\from items_lua.source, typename, "{\n    ", items_lua, "\n}"
                    else
                        lua\add LuaCode\from items_lua.source, typename, "{", items_lua, "}"
                    chunks += 1
            
            return lua

        when "Index"
            key_lua = @compile(tree[1])
            key_str = key_lua\match('^"([a-zA-Z_][a-zA-Z0-9_]*)"$')
            return if key_str and key_str\is_lua_id!
                LuaCode\from tree.source, ".", key_str
            elseif key_lua\first(1) == "["
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
            if lua\match("['\"}]$") or lua\match("]=*]$")
                lua\parenthesize!
            if lua\text! == "..."
                return LuaCode\from(tree.source, "select(", @compile(tree[2][1]), ", ...)")
            for i=2,#tree
                key = tree[i]
                -- TODO: remove this shim
                if key.type != "Index"
                    key = SyntaxTree{type:"Index", source:key.source, key}
                lua\add @compile(key)
            return lua

        when "Number"
            number = tostring(tree[1])\gsub("_", "")
            return LuaCode\from(tree.source, number)

        when "Var"
            return LuaCode\from(tree.source, tree\as_var!\as_lua_id!)

        when "FileChunks"
            error("Can't convert FileChunks to a single block of lua, since each chunk's "..
                "compilation depends on the earlier chunks")
        
        when "Comment"
            return LuaCode\from(tree.source, "-- ", (tree[1]\gsub('\n', '\n-- ')))
        
        when "Error"
            err_msg = pretty_error{
                title:"Parse error"
                error:tree.error, hint:tree.hint, source:tree\get_source_file!
                start:tree.source.start, stop:tree.source.stop, filename:tree.source.filename
            }
            -- Coroutine yield here?
            error(err_msg)

        else
            error("Unknown type: #{tree.type}")

return {:compile, :fail_at}
