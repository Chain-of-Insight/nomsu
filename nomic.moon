re = require 're'
lpeg = require 'lpeg'
utils = require 'utils'
moon = require 'moon'

lpeg.setmaxstack 10000 -- whoa
{:P,:V,:S,:Cg,:C,:Cp,:B,:Cmt} = lpeg

wordchar = P(1)-S(' \t\n\r%:;,.{}[]()"')
spaces = S(" \t")^1

get_line_indentation = (line)->
    indent_amounts = {[" "]:1, ["\t"]:4}
    with sum = 0
        leading_space = line\gsub("([\t ]*).*", "%1")
        for c in leading_space\gmatch "[\t ]"
            sum += indent_amounts[c]

class Game
    new:(parent)=>
        @defs = setmetatable({}, {__index:parent and parent.defs})
        @debug = false

    call: (fn_name,...)=>
        fn_info = @defs[fn_name]
        if fn_info == nil
            error "Attempt to call undefined function: #{fn_name}"
        {:fn, :arg_names} = fn_info
        args = {name, select(i,...) for i,name in ipairs(arg_names)}
        if @debug
            print "Calling #{fn_name} with args: #{utils.repr(args)}"
        return fn(self, args)

    def: (spec, fn)=>
        invocations,arg_names = self\get_invocations spec
        fn_info = {:fn, :arg_names, :invocations, is_macro:false}
        for invocation in *invocations
            @defs[invocation] = fn_info

    get_invocations:(text)=>
        if type(text) == 'string' then text = {text}
        invocations = {}
        local arg_names
        for _text in *text
            name_bits = {}
            _arg_names = {}
            for chunk in _text\gmatch("%S+")
                if chunk\sub(1,1) == "%"
                    table.insert name_bits, "%"
                    table.insert _arg_names, chunk\sub(2,-1)
                else
                    table.insert name_bits, chunk
            invocation = table.concat name_bits, " "
            table.insert(invocations, invocation)
            if arg_names and not utils.equivalent(utils.set(arg_names), utils.set(_arg_names))
                error("Conflicting argument names #{utils.repr(arg_names)} and #{utils.repr(_arg_names)} for #{utils.repr(text)}")
            arg_names = _arg_names
        return invocations, arg_names

    defmacro: (spec, fn)=>
        assert fn, "No function supplied"
        invocations,arg_names = self\get_invocations spec
        fn_info = {:fn, :arg_names, :invocations, is_macro:true}
        for invocation in *invocations
            @defs[invocation] = fn_info

    simplemacro: (spec, replacement)=>
        spec = spec\gsub("\r", "")
        replacement = replacement\gsub("\r", "")
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

        nl = P("\n")
        blank_line = spaces^-1 * nl
        defs =
            eol: #(nl) + (P("")-P(1))
            ws: S(" \t")^1
            :wordchar
            :replacer
            :nl, :spaces
            word_boundary: S(" \t")^1 + B(P("..")) + B(S("\";)]")) + #S("\":([") + #P("..")
            indent: #(nl * blank_line^0 * Cmt(spaces^-1, check_indent))
            dedent: #(nl * blank_line^0 * Cmt(spaces^-1, check_dedent))
            new_line: nl * blank_line^0 * Cmt(spaces^-1, check_nodent)

        replace_grammar = [[
            stuff <- {~ (var / longstring / string / .)+ ~}
            var <- ("%" {%wordchar+}) -> replacer
            string <- '"' (("\" .) / [^"])* '"'
            longstring <- ('"..' %indent %nl {(!%dedent .)*} %new_line '.."')
        ]]
        fn = (vars, helpers, ftype)=>
            replacer = (varname)->
                ret = vars[varname].src
                return ret
            replacement_grammar = re.compile(replace_grammar, defs)
            replaced = replacement_grammar\match(replacement)
            tree = lingo\match replaced
            result = helpers.transform(tree.value.body)
            helpers.lua(result)
            return

        self\defmacro spec, fn
    
    run: (text)=>
        if @debug
            print("RUNNING TEXT:\n")
            print(text)
        code = self\compile(text)
        if @debug
            print("\nGENERATED LUA CODE:")
            print(code)
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
        ret = self\run(text)
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

            string <- ({ (!('"..' %ws? %nl)) '"' {(("\" .) / [^"])*} '"' }) -> String
            longstring <- ({ '"..' %ws? %indent %nl {(!%dedent .)* (%nl %ws? %eol)*} ((%new_line '.."') / errors) }) -> Longstring
            number <- ({ {'-'? [0-9]+ ("." [0-9]+)?} }) -> Number
            variable <- ({ ("%" {%wordchar+}) }) -> Var

            subexpression <-
                ("(" %ws? (functioncall / expression) %ws? ")")
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

        str = str\gsub("\r", "")
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

        nl = P("\n")
        blank_line = spaces^-1 * nl
        defs =
            eol: #(nl) + (P("")-P(1))
            ws: S(" \t")^1
            :wordchar
            :nl, :spaces
            word_boundary: S(" \t")^1 + B(P("..")) + B(S("\";)]")) + #S("\":([") + #P("..")
            indent: #(nl * blank_line^0 * Cmt(spaces^-1, check_indent))
            dedent: #(nl * blank_line^0 * Cmt(spaces^-1, check_dedent))
            new_line: nl * blank_line^0 * Cmt(spaces^-1, check_nodent)
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

                pointer = ("-")\rep(err_pos - start_of_err_line + 1) .. "^"
                error("\nParse error on line #{line_no}:\n|#{prev_line}\n|#{err_line}\n#{pointer}\n|#{next_line}")

        setmetatable(defs, {
            __index: (t,key)->
                --print("WORKING for #{key}")
                fn = (src, value, errors)->
                    token = {type: key, :src, :value, :errors}
                    return token
                t[key] = fn
                return fn
        })
        lingo = re.compile lingo, defs
        tree = lingo\match(str\gsub("\r","").."\n")
        if @debug
            print("\nPARSE TREE:")
            self\print_tree(tree)
        assert tree, "Failed to parse: #{str}"
        return tree
    
    transform: (tree, indent_level=0, parent=nil, src=nil)=>
        if src == nil then src = tree.src
        indented = (fn)->
            export indent_level
            indent_level += 1
            fn!
            indent_level -= 1
        transform = (t,parent)-> self\transform(t, indent_level, parent or tree, src)
        ind = (line) -> ("  ")\rep(indent_level)..line
        ded = (lines)->
            if not lines.match then error("WTF: #{utils.repr(lines)}")
            lines\match"^%s*(.*)"

        ret_lines = {}
        lua = (line, skip_indent=false)->
            unless skip_indent
                line = ind(ded(line))
            table.insert ret_lines, line
            return line
        
        comma_separated_items = (open, items, close)->
            buffer = open
            so_far = indent_level*2
            indented ->
                export buffer,so_far
                for i,item in ipairs(items)
                    if i < #items then item ..= ", "
                    if so_far + #item >= 80 and #buffer > 0
                        lua buffer
                        so_far -= #buffer
                        buffer = item
                    else
                        so_far += #item
                        buffer ..= item
                buffer ..= close
                lua buffer

        switch tree.type
            when "File"
                lua "return (function(game, vars)"
                indented ->
                    lua "local ret"
                    lua transform(tree.value.body)
                    lua "return ret"
                lua "end)"

            when "Block"
                for chunk in *tree.value
                    lua transform(chunk)
        
            when "Thunk"
                if not tree.value
                    error("Thunk without value: #{utils.repr(tree)}")
                lua "(function(game,vars)"
                indented ->
                    lua "local ret"
                    assert tree.value.type == "Block", "Non-block value in Thunk"
                    lua transform(tree.value)
                    lua "return ret"
                lua "end)"

            when "Statement"
                if tree.value.type == "FunctionCall"
                    name_bits = {}
                    for token in *tree.value.value
                        table.insert name_bits, if token.type == "Word" then token.value else "%"
                    name = table.concat(name_bits, " ")
                    if @defs[name] and @defs[name].is_macro
                        -- This case here is to prevent "ret =" from getting prepended when the macro might not want it
                        lua transform(tree.value)
                        ret = table.concat ret_lines, "\n"
                        return ret
                lua "ret = #{ded(transform(tree.value))}"

            when "Expression"
                lua transform(tree.value)

            when "FunctionCall"
                name_bits = {}
                for token in *tree.value
                    table.insert name_bits, if token.type == "Word" then token.value else "%"
                name = table.concat(name_bits, " ")
                if @defs[name] and @defs[name].is_macro
                    {:fn, :arg_names} = @defs[name]
                    helpers = {:indented, :transform, :ind, :ded, :lua, :comma_separated_items}
                    args = [a for a in *tree.value when a.type != "Word"]
                    args = {name,args[i] for i,name in ipairs(arg_names)}
                    helpers.var = (varname)->
                        ded(transform(args[varname]))
                    m = fn(self, args, helpers, parent.type)
                    if m != nil then return m
                else
                    args = [ded(transform(a)) for a in *tree.value when a.type != "Word"]
                    table.insert args, 1, utils.repr(name, true)
                    comma_separated_items("game:call(", args, ")")

            when "String"
                escapes = n:"\n", t:"\t", b:"\b", a:"\a", v:"\v", f:"\f", r:"\r"
                unescaped = tree.value\gsub("\\(.)", ((c)-> escapes[c] or c))
                lua utils.repr(unescaped, true)

            when "Longstring"
                first_nonblank_line = tree.value\match("[^\n]+")
                indent = first_nonblank_line\match("[ \t]*")
                result = {}
                for line in (tree.value.."\n")\gmatch("(.-)\n")
                    line = line\gsub("^"..indent, "", 1)
                    table.insert result, line
                lua utils.repr(table.concat(result, "\n"), true)

            when "Number"
                lua tree.value

            when "List"
                if #tree.value == 0
                    lua "{}"
                elseif #tree.value == 1
                    lua "{#{transform(tree.value)}}"
                else
                    comma_separated_items("{", [ded(transform(item)) for item in *tree.value], "}")

            when "Var"
                lua "vars[#{utils.repr(tree.value,true)}]"

            else
                error("Unknown/unimplemented thingy: #{tree.type}")
        
        ret = table.concat ret_lines, "\n"
        return ret

    _yield_tree: (tree, indent_level=0)=>
        ind = (s) -> ("  ")\rep(indent_level)..s
        switch tree.type
            when "File"
                coroutine.yield(ind"File:")
                self\_yield_tree(tree.value.body, indent_level+1)

            when "Errors"
                coroutine.yield(ind"Error:\n#{tree.value}")

            when "Block"
                for chunk in *tree.value
                    self\_yield_tree(chunk, indent_level)
        
            when "Thunk"
                coroutine.yield(ind"Thunk:")
                self\_yield_tree(tree.value, indent_level+1)

            when "Statement"
                self\_yield_tree(tree.value, indent_level)

            when "Expression"
                self\_yield_tree(tree.value, indent_level)

            when "FunctionCall"
                name_bits = {}
                for token in *tree.value
                    table.insert name_bits, if token.type == "Word" then token.value else "%"
                name = table.concat(name_bits, " ")
                if #[a for a in *tree.value when a.type != "Word"] == 0
                    coroutine.yield(ind"Call [#{name}]!")
                else
                    coroutine.yield(ind"Call [#{name}]:")
                    for a in *tree.value
                        if a.type != "Word"
                            self\_yield_tree(a, indent_level+1)

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
                        self\_yield_tree(item, indent_level+1)

            when "Var"
                coroutine.yield ind"Var[#{utils.repr(tree.value)}]"

            else
                error("Unknown/unimplemented thingy: #{tree.type}")
        return nil -- to prevent tail calls

    print_tree:(tree)=>
        for line in coroutine.wrap(-> self\_yield_tree(tree))
            print(line)

    stringify_tree:(tree)=>
        result = {}
        for line in coroutine.wrap(-> self\_yield_tree(tree))
            table.insert(result, line)
        return table.concat result, "\n"

    compile: (src)=>
        tree = self\parse(src)
        code = self\transform(tree,0)
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
            tree = self\parse(test_src)
            got = self\stringify_tree(tree.value.body)
            if got != expected
                error"TEST FAILED!\nSource:\n#{test_src}\nExpected:\n#{expected}\n\nGot:\n#{got}"


return Game
