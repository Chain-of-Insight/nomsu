#!/usr/bin/env moon
re = require 're'
lpeg = require 'lpeg'
utils = require 'utils'

-- TODO:
-- improve indentation of generated lua code
-- provide way to run precompiled nomsu -> lua code
-- comprehensions?
-- dicts?
-- better scoping?
-- first-class functions
-- better error reporting
-- type checking?

INDENT = "    "
lpeg.setmaxstack 10000 -- whoa
{:P,:V,:S,:Cg,:C,:Cp,:B,:Cmt} = lpeg

get_line_indentation = (line)->
    indent_amounts = {[" "]:1, ["\t"]:4}
    with sum = 0
        leading_space = line\match("[\t ]*")
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

    wordchar = P(1)-S(' \t\n\r%#:;,.{}[]()"\\')
    nl = P("\n")
    whitespace = S(" \t")^1
    blank_line = whitespace^-1 * nl
    line_comment = re.compile([=[ "#" [^%nl]* ]=], {:nl})
    block_comment = re.compile([=[
        "#.." (!%nl .)* (%indent (!%dedent %nl [^%nl]*)*)
    ]=], {:nl, :whitespace,
            indent:#(nl * blank_line^0 * Cmt(S(" \t")^0, check_indent)),
            dedent:#(nl * blank_line^0 * Cmt(S(" \t")^0, check_dedent)),
            new_line:nl * blank_line^0 * Cmt(S(" \t")^0, check_nodent)})
    blank_line = ((Cmt(whitespace^-1, check_nodent) * (block_comment + line_comment))^-1 + whitespace^-1) * nl
    defs =
        :wordchar, :nl, ws:whitespace, :blank_line, :block_comment, :line_comment
        eol: #nl + (P("")-P(1))
        word_boundary: whitespace^-1 + B(P("..")) + B(S("\";)]")) + #S("\":([") + #((whitespace + nl)^0 * P(".."))
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

class NomsuCompiler
    new:(parent)=>
        @defs = setmetatable({}, {__index:parent and parent.defs})
        @callstack = {}
        @debug = false
        @initialize_core!

    call: (fn_name,...)=>
        fn_info = @defs[fn_name]
        if fn_info == nil
            @error "Attempt to call undefined function: #{fn_name}"
        if fn_info.is_macro
            @error "Attempt to call macro at runtime: #{fn_name}"
        unless @check_permission(fn_name)
            @error "You do not have the authority to call: #{fn_name}"
        table.insert @callstack, fn_name
        {:fn, :arg_names} = fn_info
        args = {name, select(i,...) for i,name in ipairs(arg_names)}
        if @debug
            print "Calling #{fn_name} with args: #{utils.repr(args)}"
        ret = fn(self, args)
        table.remove @callstack
        return ret

    check_permission: (fn_name)=>
        fn_info = @defs[fn_name]
        if fn_info == nil
            @error "Undefined function: #{fn_name}"
        if fn_info.whiteset == nil then return true
        for caller in *@callstack
            if fn_info.whiteset[caller]
                return true
        return false

    def: (spec, fn)=>
        if @debug
            print "Defining rule: #{spec}"
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
                    @error("Conflicting argument names #{utils.repr(arg_names)} and #{utils.repr(_arg_names)} for #{utils.repr(text)}")
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
        -- This will execute each chunk as it goes along
        code = @compile(text)
        if @debug
            print "\nGENERATED LUA CODE:\n#{code}"
        return code
    
    parse: (str)=>
        if @debug
            print("PARSING:\n#{str}")
        lingo = [=[
            file <- ({ {| %blank_line* {:body: block :} %blank_line* (errors)? |} }) -> File
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
                ((".." %ws? %line_comment? (%indent %new_line indented_fn_bits %dedent) (%new_line ".." %ws? fn_bits)?)
                 / (%new_line ".." fn_bit (%word_boundary fn_bits)?)
                 / (fn_bit (%word_boundary fn_bits)?))
            indented_fn_bits <-
                fn_bit ((%new_line / %word_boundary) indented_fn_bits)?
            
            thunk <-
                ({ ":" %ws? %line_comment?
                   ((%indent %new_line block ((%dedent (%new_line "..")?) / errors))
                    / (one_liner (%ws? (%new_line? ".."))?)) }) -> Thunk

            word <- ({ !number {%wordchar+} }) -> Word
            expression <- ({ (longstring / string / number / variable / list / thunk / subexpression) }) -> Expression

            string <- ({ (!longstring) '"' {(("\" [^%nl]) / [^"%nl])*} '"' }) -> String
            longstring <- ({ '".."' %ws?
                {|
                    (("|" {| ({("\\" / (!string_interpolation [^%nl]))+} / string_interpolation)* |})
                     / %line_comment)?
                    (%indent
                        (%new_line "|" {|
                            ({("\\" / (!string_interpolation [^%nl]))+} / string_interpolation)*
                        |})+
                    ((%dedent (%new_line '..')?) / errors))?
                |}}) -> Longstring
            string_interpolation <- "\" %ws? (functioncall / expression) %ws? "\"
            number <- ({ {'-'? [0-9]+ ("." [0-9]+)?} }) -> Number
            variable <- ({ ("%" {%wordchar+}) }) -> Var

            subexpression <-
                ("(" %ws? (functioncall / expression) %ws? ")")
                / ("(..)" %ws? %line_comment? %indent %new_line ((({ {| indented_fn_bits |} }) -> FunctionCall) / expression) %dedent (%new_line "..")?)

            list <- ({ {|
                ("[..]" %ws? %line_comment? %indent %new_line indented_list ","? ((%dedent (%new_line "..")?) / errors))
                / ("[" %ws? (list_items ","?)?  %ws?"]")
              |} }) -> List
            list_items <- ((functioncall / expression) (list_sep list_items)?)
            list_sep <- %ws? "," %ws?
            indented_list <-
                (functioncall / expression) (((list_sep (%line_comment? %new_line)?) / (%line_comment? %new_line)) indented_list)?
        ]=]
        lingo = make_parser lingo

        tree = lingo\match(str\gsub("\r","").."\n")
        if @debug
            print("\nPARSE TREE:")
            @print_tree(tree)
        assert tree, "Failed to parse: #{str}"
        return tree

    tree_to_value: (tree, vars)=>
        -- TODO: clean up require utils
        code = "
        local utils = require('utils')
        return (function(compiler, vars)\nreturn #{@tree_to_lua(tree)}\nend)"
        lua_thunk, err = load(code)
        if not lua_thunk
            error("Failed to compile generated code:\n#{code}\n\n#{err}")
        return (lua_thunk!)(self, vars or {})

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
                add [[return (function(compiler, vars)
                        local ret]]
                vars = {}
                for statement in *tree.value.body.value
                    code = to_lua(statement)
                    -- Run the fuckers as we go
                    -- TODO: clean up repeated loading of utils?
                    lua_thunk, err = load("
                    local utils = require('utils')
                    return (function(compiler, vars)\n#{code}\nend)")
                    if not lua_thunk
                        error("Failed to compile generated code:\n#{code}\n\n#{err}\n\nProduced by statement:\n#{utils.repr(statement)}")
                    ok,err = pcall(lua_thunk)
                    if not ok then error(err)
                    ok,err = pcall(err, self, vars)
                    if not ok then @error(err)
                    add code
                add [[
                        return ret
                    end)
                ]]

            when "Block"
                for statement in *tree.value
                    add to_lua(statement)
        
            when "Thunk"
                assert tree.value.type == "Block", "Non-block value in Thunk"
                add [[
                    (function(compiler, vars)
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
                    add @@comma_separated_items("compiler:call(", args, ")")

            when "String"
                escapes = n:"\n", t:"\t", b:"\b", a:"\a", v:"\v", f:"\f", r:"\r"
                unescaped = tree.value\gsub("\\(.)", ((c)-> escapes[c] or c))
                add utils.repr(unescaped, true)

            when "Longstring"
                concat_parts = {}
                string_buffer = ""
                for i,line in ipairs(tree.value)
                    if i > 1 then string_buffer ..= "\n"
                    for bit in *line
                        if type(bit) == "string"
                            string_buffer ..= bit\gsub("\\\\","\\")
                        else
                            if string_buffer ~= ""
                                table.insert concat_parts, utils.repr(string_buffer, true)
                                string_buffer = ""
                            table.insert concat_parts, "utils.repr(#{to_lua(bit)})"

                if string_buffer ~= ""
                    table.insert concat_parts, utils.repr(string_buffer, true)

                if #concat_parts == 0
                    add "''"
                elseif #concat_parts == 1
                    add concat_parts[1]
                else
                    add "(#{table.concat(concat_parts, "..")})"

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
            @error("Macro not found: #{name}")
        unless @check_permission(name)
            @error "You do not have the authority to call: #{name}"
        {:fn, :arg_names} = @defs[name]
        args = [a for a in *tree.value when a.type != "Word"]
        args = {name,args[i] for i,name in ipairs(arg_names)}
        table.insert @callstack, name
        ret, manual_mode = fn(self, args, kind)
        table.remove @callstack
        if not ret
            @error("No return value for macro: #{name}")
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

    compile: (src, output_file=nil)=>
        if @debug
            print "COMPILING:\n#{src}"
        tree = @parse(src)
        assert tree, "Tree failed to compile: #{src}"
        code = @tree_to_lua(tree)
        if output_file
            output = io.open(output_file, "w")
            output\write(code)
        return code

    error: (...)=>
        print(...)
        print("Callstack:")
        for i=#@callstack,1,-1
            print "    #{@callstack[i]}"
        error!

    test: (src, expected)=>
        i = 1
        while i != nil
            start,stop = src\find("\n\n", i)

            test = src\sub(i,start)
            i = stop
            start,stop = test\find"==="
            if not start or not stop then
                @error("WHERE'S THE ===? in:\n#{test}")
            test_src, expected = test\sub(1,start-1), test\sub(stop+1,-1)
            expected = expected\match'[\n]*(.*[^\n])'
            tree = @parse(test_src)
            got = @stringify_tree(tree.value.body)
            if got != expected
                @error"TEST FAILED!\nSource:\n#{test_src}\nExpected:\n#{expected}\n\nGot:\n#{got}"


    initialize_core: =>
        -- Sets up some core functionality
        as_lua_code = (str, vars)=>
            switch str.type
                when "String"
                    return @tree_to_value(str, vars)
                when "Longstring"
                    return @tree_to_value(str, vars)
                else
                    return @tree_to_lua(str)

        @defmacro [[lua block %lua_code]], (vars, kind)=>
            if kind == "Expression" then error("Expected to be in statement.")
            lua_code = vars.lua_code.value
            switch lua_code.type
                when "List"
                    -- TODO: handle subexpressions
                    return table.concat([as_lua_code(@, i.value, vars) for i in *lua_code.value]), true
                else
                    return as_lua_code(@, lua_code, vars), true

        @defmacro [[lua expr %lua_code]], (vars, kind)=>
            lua_code = vars.lua_code.value
            switch lua_code.type
                when "List"
                    -- TODO: handle subexpressions
                    return table.concat([as_lua_code(@, i.value, vars) for i in *lua_code.value])
                else
                    return as_lua_code(@, lua_code, vars)

        @def "rule %spec %body", (vars)=>
            @def vars.spec, vars.body

        @defmacro [[macro %spec %body]], (vars, kind)=>
            if kind == "Expression"
                error("Macro definitions cannot be used as expressions.")
            @defmacro @tree_to_value(vars.spec, vars), @tree_to_value(vars.body, vars)
            return "", true

        @defmacro [[macro block %spec %body]], (vars, kind)=>
            if kind == "Expression"
                error("Macro definitions cannot be used as expressions.")
            invocation = @tree_to_value(vars.spec, vars)
            fn = @tree_to_value(vars.body, vars)
            @defmacro invocation, ((vars,kind)=>
                if kind == "Expression"
                    error("Macro: #{invocation} was defined to be a block, not an expression.")
                return fn(@,vars,kind), true)
            return "", true

        @def "run file %filename", (vars)=>
            file = io.open(vars.filename)
            return @run(file\read('*a'))


-- Run on the command line via "./nomsu.moon input_file.nom" to execute
-- and "./nomsu.moon input_file.nom output_file.lua" to compile (use "-" to compile to stdout)
if arg and arg[1]
    c = NomsuCompiler()
    --c.debug = true
    input = io.open(arg[1])\read("*a")
    -- Kinda hacky, if run via "./nomsu.moon file.nom -", then silence print and io.write
    -- during execution and re-enable them to print out the generated source code
    _print = print
    _io_write = io.write
    if arg[2] == "-"
        export print
        nop = ->
        print, io.write = nop, nop
    code = c\run(input)
    if arg[2]
        output = if arg[2] == "-"
            export print
            print, io.write = _print, _io_write
            io.output()
        else io.open(arg[2], 'w')

        output\write [[
    local utils = require('utils')
    local load = function()
    ]]
        output\write(code)
        output\write [[

    end
    local NomsuCompiler = require('nomsu')
    local c = NomsuCompiler()
    load()(c, {})
    ]]

return NomsuCompiler