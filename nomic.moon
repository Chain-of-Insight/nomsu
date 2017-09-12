#!/usr/bin/env moon
re = require 're'
lpeg = require 'lpeg'
utils = require 'utils'
moon = require 'moon'

-- TODO:
-- string interpolation
-- comprehensions?
-- dicts?
-- better scoping?
-- first-class functions

INDENT = "    "
lpeg.setmaxstack 10000 -- whoa
{:P,:V,:S,:Cg,:C,:Cp,:B,:Cmt} = lpeg

wordchar = P(1)-S(' \t\n\r%:;,.{}[]()"')
comment = re.compile [[comment <- "(#" (comment / ((! "#)") .))* "#)"]]
whitespace = (S(" \t") + comment)^1
nl = P("\n")
blank_line = whitespace^-1 * nl

get_line_indentation = (line)->
    indent_amounts = {[" "]:1, ["\t"]:4}
    with sum = 0
        leading_space = line\gsub("([\t ]*).*", "%1")
        for c in leading_space\gmatch "[\t ]"
            sum += indent_amounts[c]

make_parser = (lingo, extra_definitions)->
    indent_stack = {0}
    push = (n)-> table.insert indent_stack, n
    pop = ()-> table.remove indent_stack
    check_indent = (subject,end_pos,spaces)->
        num_spaces = get_line_indentation(spaces)
        if num_spaces <= indent_stack[#indent_stack] then return nil
        push num_spaces
        return end_pos
    check_dedent = (subject,end_pos,spaces)->
        num_spaces = get_line_indentation(spaces)
        if num_spaces >= indent_stack[#indent_stack] then return nil
        pop!
        return end_pos
    check_nodent = (subject,end_pos,spaces)->
        num_spaces = get_line_indentation(spaces)
        if num_spaces != indent_stack[#indent_stack] then return nil
        return end_pos

    defs =
        :wordchar, :nl, ws:whitespace, :comment
        eol: #nl + (P("")-P(1))
        word_boundary: S(" \t")^1 + B(P("..")) + B(S("\";)]")) + #S("\":([") + #P("..")
        indent: #(nl * blank_line^0 * Cmt(whitespace^-1, check_indent))
        dedent: #(nl * blank_line^0 * Cmt(whitespace^-1, check_dedent))
        new_line: nl * blank_line^0 * Cmt(whitespace^-1, check_nodent)
        error_handler: (src,pos,errors)->
            line_no = 1
            for _ in src\sub(1,-#errors)\gmatch("\n") do line_no += 1
            err_pos = #src - #errors + 1
            if errors\sub(1,1) == "\n"
                -- Indentation error
                err_pos += #errors\match("[ \t]*", 2)
            start_of_err_line = err_pos
            while src\sub(start_of_err_line, start_of_err_line) != "\n" do start_of_err_line -= 1
            start_of_prev_line = start_of_err_line - 1
            while src\sub(start_of_prev_line, start_of_prev_line) != "\n" do start_of_prev_line -= 1
            
            prev_line,err_line,next_line = src\match("([^\n]*)\n([^\n]*)\n([^\n]*)", start_of_prev_line+1)

            pointer = ("-")\rep(err_pos - start_of_err_line + 0) .. "^"
            error("\nParse error on line #{line_no}:\n\n#{prev_line}\n#{err_line}\n#{pointer}\n#{next_line}\n")
    
    if extra_definitions
        for k,v in pairs(extra_definitions) do defs[k] = v

    setmetatable(defs, {
        __index: (t,key)->
            fn = (src, value, errors)->
                token = {type: key, :src, :value, :errors}
                return token
            t[key] = fn
            return fn
    })
    return re.compile lingo, defs

class Game
    new:(parent)=>
        @defs = setmetatable({}, {__index:parent and parent.defs})
        @debug = false

    call: (fn_name,...)=>
        fn_info = @defs[fn_name]
        if fn_info == nil
            error "Attempt to call undefined function: #{fn_name}"
        if fn_info.is_macro
            error "Attempt to call macro at runtime: #{fn_name}"
        {:fn, :arg_names} = fn_info
        args = {name, select(i,...) for i,name in ipairs(arg_names)}
        if @debug
            print "Calling #{fn_name} with args: #{utils.repr(args)}"
        return fn(self, args)

    def: (spec, fn)=>
        invocations,arg_names = @get_invocations spec
        fn_info = {:fn, :arg_names, :invocations, is_macro:false}
        for invocation in *invocations
            @defs[invocation] = fn_info

    get_invocations:(text)=>
        if type(text) == 'string' then text = {text}
        invocations = {}
        local arg_names
        for _text in *text
            invocation = _text\gsub("%%%S+","%%")
            _arg_names = [arg for arg in _text\gmatch("%%(%S+)")]
            table.insert(invocations, invocation)
            if arg_names
                if not utils.equivalent(utils.set(arg_names), utils.set(_arg_names))
                    error("Conflicting argument names #{utils.repr(arg_names)} and #{utils.repr(_arg_names)} for #{utils.repr(text)}")
            else arg_names = _arg_names
        return invocations, arg_names

    defmacro: (spec, lua_gen_fn)=>
        invocations,arg_names = @get_invocations spec
        fn_info = {fn:lua_gen_fn, :arg_names, :invocations, is_macro:true}
        for invocation in *invocations
            @defs[invocation] = fn_info
    
    run: (text)=>
        if @debug
            print "RUNNING TEXT:\n#{text}"
        code = @compile(text)
        if @debug
            print "\nGENERATED LUA CODE:\n#{code}"
        lua_thunk, err = loadstring(code)
        if not lua_thunk
            error("Failed to compile generated code:\n#{code}\n\n#{err}")
        action = lua_thunk!
        if @debug
            print("Running...")
        return action(self, {})
    
    run_debug:(text)=>
        old_debug = @debug
        @debug = true
        ret = @run(text)
        @debug = old_debug
        return ret

    parse: (str)=>
        if @debug
            print("PARSING:\n#{str}")
        lingo = [=[
            file <- ({ {| %new_line? {:body: block :} %new_line? (errors)? |} }) -> File
            errors <- (({.+}) => error_handler)
            block <- ({ {| statement (%new_line statement)* |} }) -> Block
            statement <- ({ (functioncall / expression) }) -> Statement
            one_liner <- ({ {|
                    (({ 
                        (({ {|
                            (expression (%word_boundary fn_bit)+) / (word (%word_boundary fn_bit)*)
                        |} }) -> FunctionCall)
                        / (expression)
                     }) -> Statement)
                |} }) -> Block

            functioncall <- ({ {| (expression %word_boundary fn_bits) / (word (%word_boundary fn_bits)?) |} }) -> FunctionCall
            fn_bit <- (expression / word)
            fn_bits <-
                ((".." %ws? (%indent %new_line indented_fn_bits %dedent) (%new_line ".." %ws? fn_bits)?)
                 / (%new_line ".." fn_bit fn_bits)
                 / (fn_bit (%word_boundary fn_bits)?))
            indented_fn_bits <-
                fn_bit ((%new_line / %word_boundary) indented_fn_bits)?
            
            thunk <-
                ({ ":" %ws?
                   ((%indent %new_line block ((%dedent (%new_line "..")?) / errors))
                    / (one_liner (%ws? (%new_line? ".."))?)) }) -> Thunk

            word <- ({ !number {%wordchar+} }) -> Word
            expression <- ({ (longstring / string / number / variable / list / thunk / subexpression) }) -> Expression

            string <- ({ (!longstring) '"' {(("\" .) / [^"])*} '"' }) -> String
            longstring <- ({ '".."' %ws? %indent {(%new_line "|" [^%nl]*)+} ((%dedent (%new_line '..')?) / errors) }) -> Longstring
            number <- ({ {'-'? [0-9]+ ("." [0-9]+)?} }) -> Number
            variable <- ({ ("%" {%wordchar+}) }) -> Var

            subexpression <-
                (!%comment "(" %ws? (functioncall / expression) %ws? ")")
                / ("(..)" %ws? %indent %new_line ((({ {| indented_fn_bits |} }) -> FunctionCall) / expression) %dedent (%new_line "..")?)

            list <- ({ {|
                ("[..]" %ws? %indent %new_line indented_list ","? ((%dedent (%new_line "..")?) / errors))
                / ("[" %ws? (list_items ","?)?  %ws?"]")
              |} }) -> List
            list_items <- (expression (list_sep list_items)?)
            list_sep <- %ws? "," %ws?
            indented_list <-
                expression (((list_sep %new_line?) / %new_line) indented_list)?
        ]=]
        lingo = make_parser lingo

        tree = lingo\match(str\gsub("\r","").."\n")
        if @debug
            print("\nPARSE TREE:")
            @print_tree(tree)
        assert tree, "Failed to parse: #{str}"
        return tree

    tree_to_value: (tree)=>
        code = "return (function(game, vars)\nreturn #{@tree_to_lua(tree)}\nend)"
        lua_thunk, err = loadstring(code)
        if not lua_thunk
            error("Failed to compile generated code:\n#{code}\n\n#{err}")
        return (lua_thunk!)(self, {})

    tree_to_lua: (tree, kind="Expression")=>
        assert tree, "No tree provided."
        indent = ""
        buffer = {}

        to_lua = (t,kind)->
            ret = @tree_to_lua(t,kind)
            return ret

        add = (code)-> table.insert(buffer, code)

        switch tree.type
            when "File"
                add [[return (function(game, vars)
                        local ret]]
                add to_lua(tree.value.body)
                add [[
                        return ret
                    end)
                ]]

            when "Block"
                for chunk in *tree.value
                    add to_lua(chunk)
        
            when "Thunk"
                assert tree.value.type == "Block", "Non-block value in Thunk"
                add [[
                    (function(game, vars)
                        local ret]]
                add to_lua(tree.value)
                add [[
                        return ret
                    end)
                ]]

            when "Statement"
                -- This case here is to prevent "ret =" from getting prepended when the macro might not want it
                if tree.value.type == "FunctionCall"
                    name = @fn_name_from_tree(tree.value)
                    if @defs[name] and @defs[name].is_macro
                        add @run_macro(tree.value, "Statement")
                    else
                        add "ret = "..(to_lua(tree.value)\match("%s*(.*)"))
                else
                    add "ret = "..(to_lua(tree.value)\match("%s*(.*)"))

            when "Expression"
                add to_lua(tree.value)

            when "FunctionCall"
                name = @fn_name_from_tree(tree)
                if @defs[name] and @defs[name].is_macro
                    add @run_macro(tree, "Expression")
                else
                    args = [to_lua(a) for a in *tree.value when a.type != "Word"]
                    table.insert args, 1, utils.repr(name, true)
                    add @@comma_separated_items("game:call(", args, ")")

            when "String"
                escapes = n:"\n", t:"\t", b:"\b", a:"\a", v:"\v", f:"\f", r:"\r"
                unescaped = tree.value\gsub("\\(.)", ((c)-> escapes[c] or c))
                add utils.repr(unescaped, true)

            when "Longstring"
                -- TODO: handle comments here?
                result = [line for line in tree.value\gmatch("[ \t]*|([^\n]*)")]
                add utils.repr(table.concat(result, "\n"), true)

            when "Number"
                add tree.value

            when "List"
                if #tree.value == 0
                    add "{}"
                elseif #tree.value == 1
                    add "{#{to_lua(tree.value[1])}}"
                else
                    add @@comma_separated_items("{", [to_lua(item) for item in *tree.value], "}")

            when "Var"
                add "vars[#{utils.repr(tree.value,true)}]"

            else
                error("Unknown/unimplemented thingy: #{tree.type}")

        -- TODO: make indentation clean
        buffer = table.concat(buffer, "\n")
        return buffer

    @comma_separated_items: (open, items, close)=>
        utils.accumulate "\n", ->
            buffer = open
            so_far = 0
            for i,item in ipairs(items)
                if i < #items then item ..= ", "
                if so_far + #item >= 80 and #buffer > 0
                    coroutine.yield buffer
                    so_far -= #buffer
                    buffer = item
                else
                    so_far += #item
                    buffer ..= item
            buffer ..= close
            coroutine.yield buffer
    
    fn_name_from_tree: (tree)=>
        assert(tree.type == "FunctionCall", "Attempt to get fn name from non-functioncall tree: #{tree.type}")
        name_bits = {}
        for token in *tree.value
            table.insert name_bits, if token.type == "Word" then token.value else "%"
        table.concat(name_bits, " ")
    
    run_macro: (tree, kind="Expression")=>
        name = @fn_name_from_tree(tree)
        unless @defs[name] and @defs[name].is_macro
            error("Macro not found: #{name}")
        {:fn, :arg_names} = @defs[name]
        args = [a for a in *tree.value when a.type != "Word"]
        args = {name,args[i] for i,name in ipairs(arg_names)}
        ret, manual_mode = fn(self, args, kind)
        if not ret
            error("No return value for macro: #{name}")
        if kind == "Statement" and not manual_mode
            ret = "ret = "..ret
        return ret

    _yield_tree: (tree, indent_level=0)=>
        ind = (s) -> INDENT\rep(indent_level)..s
        switch tree.type
            when "File"
                coroutine.yield(ind"File:")
                @_yield_tree(tree.value.body, indent_level+1)

            when "Errors"
                coroutine.yield(ind"Error:\n#{tree.value}")

            when "Block"
                for chunk in *tree.value
                    @_yield_tree(chunk, indent_level)
        
            when "Thunk"
                coroutine.yield(ind"Thunk:")
                @_yield_tree(tree.value, indent_level+1)

            when "Statement"
                @_yield_tree(tree.value, indent_level)

            when "Expression"
                @_yield_tree(tree.value, indent_level)

            when "FunctionCall"
                name = @fn_name_from_tree(tree)
                args = [a for a in *tree.value when a.type != "Word"]
                if #args == 0
                    coroutine.yield(ind"Call [#{name}]!")
                else
                    coroutine.yield(ind"Call [#{name}]:")
                    for a in *args
                        @_yield_tree(a, indent_level+1)

            when "String"
                -- TODO: Better implement
                coroutine.yield(ind(utils.repr(tree.value, true)))

            when "Longstring"
                -- TODO: Better implement
                coroutine.yield(ind(utils.repr(tree.value, true)))

            when "Number"
                coroutine.yield(ind(tree.value))

            when "List"
                if #tree.value == 0
                    coroutine.yield(ind("<Empty List>"))
                else
                    coroutine.yield(ind"List:")
                    for item in *tree.value
                        @_yield_tree(item, indent_level+1)

            when "Var"
                coroutine.yield ind"Var[#{utils.repr(tree.value)}]"

            else
                error("Unknown/unimplemented thingy: #{tree.type}")
        return nil -- to prevent tail calls

    print_tree:(tree)=>
        for line in coroutine.wrap(-> @_yield_tree(tree))
            print(line)

    stringify_tree:(tree)=>
        result = {}
        for line in coroutine.wrap(-> @_yield_tree(tree))
            table.insert(result, line)
        return table.concat result, "\n"

    compile: (src)=>
        tree = @parse(src)
        assert tree, "Tree failed to compile: #{src}"
        code = @tree_to_lua(tree)
        return code

    test: (src, expected)=>
        i = 1
        while i != nil
            start,stop = src\find("\n\n", i)

            test = src\sub(i,start)
            i = stop
            start,stop = test\find"==="
            if not start or not stop then
                error("WHERE'S THE ===? in:\n#{test}")
            test_src, expected = test\sub(1,start-1), test\sub(stop+1,-1)
            expected = expected\match'[\n]*(.*[^\n])'
            tree = @parse(test_src)
            got = @stringify_tree(tree.value.body)
            if got != expected
                error"TEST FAILED!\nSource:\n#{test_src}\nExpected:\n#{expected}\n\nGot:\n#{got}"


return Game
