#!/usr/bin/env moon
re = require 're'
lpeg = require 'lpeg'
utils = require 'utils'
insert = table.insert

-- TODO:
-- improve indentation of generated lua code
-- provide way to run precompiled nomsu -> lua code
-- better scoping?
-- first-class rules
-- better error reporting
-- add line numbers of function calls
-- versions of rules with auto-supplied arguments
-- type checking?

INDENT = "    "
lpeg.setmaxstack 10000 -- whoa
{:P,:V,:S,:Cg,:C,:Cp,:B,:Cmt} = lpeg
STRING_ESCAPES = n:"\n", t:"\t", b:"\b", a:"\a", v:"\v", f:"\f", r:"\r"


class NomsuCompiler
    new:(parent)=>
        @write = (...)=> io.write(...)
        @defs = setmetatable({}, {__index:parent and parent.defs})
        @callstack = {}
        @debug = false
        @initialize_core!
        @utils = utils
        @loaded_files = {}
    
    writeln:(...)=>
        @write(...)
        @write("\n")

    call: (fn_name,...)=>
        fn_info = @defs[fn_name]
        if fn_info == nil
            @error "Attempt to call undefined function: #{fn_name}"
        if fn_info.is_macro
            @error "Attempt to call macro at runtime: #{fn_name}\nThis can be caused by using a macro in a function that is defined before the macro."
        unless @check_permission(fn_name)
            @error "You do not have the authority to call: #{fn_name}"
        insert @callstack, fn_name
        {:fn, :arg_names} = fn_info
        args = {name, select(i,...) for i,name in ipairs(arg_names[fn_name])}
        if @debug
            @writeln "Calling #{fn_name} with args: #{utils.repr(args)}"
        ret = fn(self, args)
        table.remove @callstack
        return ret

    check_permission: (fn_name)=>
        fn_info = @defs[fn_name]
        if fn_info == nil
            @error "Undefined function: #{fn_name}"
        if fn_info.whiteset == nil then return true
        -- TODO: optimize this, maybe by making the callstack a Counter and having a move-to-front optimization on the whitelist
        for caller in *@callstack
            if fn_info.whiteset[caller]
                return true
        return false

    def: (spec, fn, src)=>
        if @debug
            @writeln "Defining rule: #{spec}"
        invocations,arg_names = @get_invocations spec
        for i=2,#invocations
            if not utils.equivalent(utils.set(arg_names[invocations[1]]), utils.set(arg_names[invocations[i]]))
                @error("Conflicting argument names #{utils.repr(invocations[1])} and #{utils.repr(invocations[i])} for #{utils.repr(spec)}")
        fn_info = {:fn, :arg_names, :invocations, :src, is_macro:false}
        for invocation in *invocations
            @defs[invocation] = fn_info
    
    get_invocations_from_definition:(def, vars)=>
        if def.type == "String"
            return @@unescape_string(def.value)

        if def.type != "List"
            @error "Trying to get invocations from #{def.type}, but expected List or String."

        invocations = {}
        for item in *def.value
            if item.type == "String"
                insert invocations, item.value
                continue
            if item.type != "FunctionCall"
                @error "Invalid list item: #{item.type}, expected FunctionCall or String"
            name_bits = {}
            for token in *item.value
                if token.type == "Word"
                    insert name_bits, token.value
                elseif token.type == "Var"
                    insert name_bits, token.src
                else
                    @error "Unexpected token type in definition: #{token.type} (expected Word or Var)"
            insert invocations, table.concat(name_bits, " ")
        return invocations

    get_invocations:(text)=>
        if not text
            @error "No text provided!"
        if type(text) == 'function'
            error "Function passed to get_invocations"
        if type(text) == 'string' then text = {text}
        invocations = {}
        arg_names = {}
        for _text in *text
            invocation = _text\gsub("'"," '")\gsub("%%%S+","%%")\gsub("%s+"," ")
            _arg_names = [arg for arg in _text\gmatch("%%(%S[^%s']*)")]
            insert invocations, invocation
            arg_names[invocation] = _arg_names
        return invocations, arg_names

    defmacro: (spec, lua_gen_fn, src)=>
        if @debug
            @writeln("DEFINING MACRO: #{spec}#{src or ""}")
        invocations,arg_names = @get_invocations spec
        for i=2,#invocations
            if not utils.equivalent(utils.set(arg_names[invocations[1]]), utils.set(arg_names[invocations[i]]))
                @error("Conflicting argument names #{utils.repr(invocations[1])} and #{utils.repr(invocations[i])} for #{utils.repr(spec)}")
        fn_info = {fn:lua_gen_fn, :arg_names, :invocations, :src, is_macro:true}
        for invocation in *invocations
            @defs[invocation] = fn_info

    parse: (str, filename)=>
        if @debug
            @writeln("PARSING:\n#{str}")

        -- NOTE: this treats tabs as equivalent to 1 space
        indent_stack = {0}
        check_indent = (subject,end_pos,spaces)->
            if #spaces > indent_stack[#indent_stack]
                insert(indent_stack, #spaces)
                return end_pos
        check_dedent = (subject,end_pos,spaces)->
            if #spaces < indent_stack[#indent_stack]
                table.remove(indent_stack)
                return end_pos
        check_nodent = (subject,end_pos,spaces)->
            if #spaces == indent_stack[#indent_stack]
                return end_pos

        lingo = [=[
            file <- ({ {| shebang? {:body: block :} %nl* (({.+} ("" -> "Unexpected end of file")) => error)? |} }) -> File

            shebang <- "#!" [^%nl]* %nl

            block <- ({ {|
                (ignored_line %nl)*
                line_of_statements (nodent line_of_statements)*
                (%nl ignored_line)* |} }) -> Block
            inline_block <- ({ {| inline_line_of_statements |} }) -> Block

            line_of_statements <- statement (%ws? ";" %ws? statement)*
            inline_line_of_statements <- inline_statement (%ws? ";" %ws? inline_statement)*

            statement <- ({ functioncall / expression }) -> Statement
            inline_statement <- ({ inline_functioncall / expression }) -> Statement

            expression <- (
                longstring / string / number / variable / list / thunk / block_functioncall
                / ("(" %ws? (inline_thunk / inline_functioncall) %ws? ")"))

            -- Function calls need at least one word in them
            functioncall <- ({ {|
                    (expression (dotdot / tok_gap))* word ((dotdot / tok_gap) (expression / word))*
                |} }) -> FunctionCall
            inline_functioncall <- ({ {|
                    (expression tok_gap)* word (tok_gap (expression / word))*
                |} }) -> FunctionCall
            block_functioncall <- "(..)" indent functioncall (dedent / (({.+} ("" -> "Error while parsing block function call")) => error))

            word <- ({ !number {%wordchar (!"'" %wordchar)*} }) -> Word
            
            thunk <- ({ ":" ((indent block (dedent / (({.+} ("" -> "Error while parsing thunk")) => error)))
                / (%ws? inline_block)) }) -> Thunk
            inline_thunk <- ({ ":" %ws? inline_block }) -> Thunk

            string <- ({ (!longstring) '"' {(("\" [^%nl]) / [^"%nl])*} '"' }) -> String

            longstring <- ({ '".."' %ws?
                {| (longstring_line (indent
                        longstring_line (nodent longstring_line)*
                    (dedent / longstring_error))?)
                  /(indent
                        longstring_line (nodent longstring_line)*
                    (dedent / longstring_error)) |} }) -> Longstring
            longstring_line <- "|" {| ({("\\" / (!string_interpolation [^%nl]))+} / string_interpolation)* |}
            longstring_error <- (({.+} ("" -> "Error while parsing Longstring")) => error)
            string_interpolation <- "\" %ws? (((inline_functioncall / expression) dotdot?) / dotdot) %ws? "\"

            number <- ({ {"-"? (([0-9]+ "." [0-9]+) / ("." [0-9]+) / ([0-9]+)) } }) -> Number

            -- Hack to allow %foo's to parse as "%foo" and "'s" separately
            variable <- ({ ("%" {%wordchar (!"'" %wordchar)*}) }) -> Var

            list <- ({ {|
                 ("[..]" indent
                        list_line (nodent list_line)*
                  (dedent / (({.+} ("" -> "Error while parsing list")) => error)))
                /("[" %ws? (list_line %ws?)? "]")
              |} }) -> List
            list_line <- list_bit (%ws? "," tok_gap list_bit)* (%ws? ",")?
            list_bit <- inline_functioncall / expression

            block_comment <- "#.." [^%nl]* indent [^%nl]* (%nl ((%ws? (!. / &%nl)) / (!%dedented [^%nl]*)))* 
            line_comment  <- "#" [^%nl]*

            eol <- %ws? line_comment? (!. / &%nl)
            ignored_line <- (%nodented (block_comment / line_comment)) / (%ws? (!. / &%nl))
            indent <- eol (%nl ignored_line)* %nl %indented
            nodent <- eol (%nl ignored_line)* %nl %nodented
            dedent <- eol (%nl ignored_line)* (((!.) &%dedented) / (&(%nl %dedented)))
            tok_gap <- %ws / %prev_edge / &("[" / [.,:;{("#%'])
            dotdot <- nodent ".." %ws?
        ]=]

        whitespace = S(" \t")^1
        defs =
            ws:whitespace, nl: P("\n")
            wordchar: P(1)-S(' \t\n\r%#:;,.{}[]()"\\')
            indented: Cmt(S(" \t")^0 * (#(P(1)-S(" \t\n") + (-P(1)))), check_indent)
            nodented: Cmt(S(" \t")^0 * (#(P(1)-S(" \t\n") + (-P(1)))), check_nodent)
            dedented: Cmt(S(" \t")^0 * (#(P(1)-S(" \t\n") + (-P(1)))), check_dedent)
            prev_edge: B(S(" \t\n.,:;}])\""))
            error: (src,pos,errors,err_msg)->
                line_no = 1
                for _ in src\sub(1,-#errors)\gmatch("\n") do line_no += 1
                err_pos = #src - #errors + 1
                if errors\sub(1,1) == "\n"
                    -- Indentation error
                    err_pos += #errors\match("[ \t]*", 2)
                start_of_err_line = err_pos
                while src\sub(start_of_err_line, start_of_err_line) != "\n" and start_of_err_line > 1
                    start_of_err_line -= 1
                start_of_prev_line = start_of_err_line - 1
                while src\sub(start_of_prev_line, start_of_prev_line) != "\n" and start_of_prev_line > 1
                    start_of_prev_line -= 1
                
                local prev_line,err_line,next_line
                prev_line,err_line,next_line = src\match("([^\n]*)\n([^\n]*)\n([^\n]*)", start_of_prev_line+1)

                pointer = ("-")\rep(err_pos - start_of_err_line + 0) .. "^"
                error("\n#{err_msg or "Parse error"} in #{filename} on line #{line_no}:\n\n#{prev_line}\n#{err_line}\n#{pointer}\n#{next_line}\n")

        setmetatable(defs, {
            __index: (t,key)->
                fn = (src, value, errors)->
                    token = {type: key, :src, :value, :errors}
                    return token
                t[key] = fn
                return fn
        })
        lingo = re.compile(lingo, defs)
        tree = lingo\match(str\gsub("\r","").."\n")
        if @debug
            @writeln("\nPARSE TREE:")
            @print_tree(tree)
        assert tree, "Failed to parse: #{str}"
        return tree

    tree_to_value: (tree, vars)=>
        code = "
        return (function(compiler, vars)\nreturn #{@tree_to_lua(tree)}\nend)"
        lua_thunk, err = load(code)
        if not lua_thunk
            error("Failed to compile generated code:\n#{code}\n\n#{err}")
        return (lua_thunk!)(self, vars or {})

    tree_to_lua: (tree)=>
        assert tree, "No tree provided."
        if not tree.type
            @error "Invalid tree: #{utils.repr(tree)}"
        switch tree.type
            when "File"
                buffer = {[[return (function(compiler, vars)
                        local ret]]}
                vars = {}
                for statement in *tree.value.body.value
                    ok,code = pcall(@tree_to_lua, self, statement)
                    if not ok
                        @writeln "Error occurred in statement:\n#{statement.src}"
                        error(code)
                    -- Run the fuckers as we go
                    lua_code = "
                    return (function(compiler, vars)\n#{code}\nend)"
                    lua_thunk, err = load(lua_code)
                    if not lua_thunk
                        error("Failed to compile generated code:\n#{code}\n\n#{err}\n\nProduced by statement:\n#{utils.repr(statement)}")
                    value = lua_thunk!
                    ok,return_value = pcall(value, self, vars)
                    if not ok
                        @writeln "Error occurred in statement:\n#{statement.src}"
                        error(return_value)
                    insert buffer, code
                insert buffer, [[
                        return ret
                    end)
                ]]
                return table.concat(buffer, "\n"), return_value

            when "Block"
                buffer = {}
                for statement in *tree.value
                    insert buffer, @tree_to_lua(statement)
                return table.concat(buffer, "\n")
        
            when "Thunk"
                assert tree.value.type == "Block", "Non-block value in Thunk"
                return [[
                    (function(compiler, vars)
                        local ret
                        ]]..@tree_to_lua(tree.value).."\n"..[[
                        return ret
                    end)
                ]]

            when "Statement"
                -- This case here is to prevent "ret =" from getting prepended when the macro might not want it
                if tree.value.type == "FunctionCall"
                    name = @fn_name_from_tree(tree.value)
                    if @defs[name] and @defs[name].is_macro
                        return @run_macro(tree.value, "Statement")
                return "ret = "..(@tree_to_lua(tree.value))

            when "FunctionCall"
                name = @fn_name_from_tree(tree)
                if @defs[name] and @defs[name].is_macro
                    return @run_macro(tree, "Expression")
                else
                    args = [@tree_to_lua(a) for a in *tree.value when a.type != "Word"]
                    insert args, 1, utils.repr(name)
                    return @@comma_separated_items("compiler:call(", args, ")")

            when "String"
                return utils.repr(@@unescape_string(tree.value))

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
                                insert concat_parts, utils.repr(string_buffer)
                                string_buffer = ""
                            insert concat_parts, "compiler.utils.repr_if_not_string(#{@tree_to_lua(bit)})"

                if string_buffer ~= ""
                    insert concat_parts, utils.repr(string_buffer)

                if #concat_parts == 0
                    return "''"
                elseif #concat_parts == 1
                    return concat_parts[1]
                else
                    return "(#{table.concat(concat_parts, "..")})"

            when "Number"
                return tree.value

            when "List"
                if #tree.value == 0
                    return "{}"
                elseif #tree.value == 1
                    return "{#{@tree_to_lua(tree.value[1])}}"
                else
                    return @@comma_separated_items("{", [@tree_to_lua(item) for item in *tree.value], "}")

            when "Var"
                return "vars[#{utils.repr(tree.value)}]"

            else
                @error("Unknown/unimplemented thingy: #{tree.type}")

    @unescape_string: (str)=>
        str\gsub("\\(.)", ((c)-> STRING_ESCAPES[c] or c))

    @comma_separated_items: (open, items, close)=>
        bits = {open}
        so_far = 0
        for i,item in ipairs(items)
            if i < #items then item ..= ", "
            insert bits, item
            so_far += #item
            if so_far >= 80
                insert bits, "\n"
                so_far = 0
        insert bits, close
        return table.concat(bits)
    
    fn_name_from_tree: (tree)=>
        assert(tree.type == "FunctionCall", "Attempt to get fn name from non-functioncall tree: #{tree.type}")
        name_bits = {}
        for token in *tree.value
            insert name_bits, if token.type == "Word" then token.value else "%"
        table.concat(name_bits, " ")

    var_to_lua_identifier: (var)=>
        if var.type != "Var"
            @error("Tried to convert something that wasn't a Var into a lua identifier: it was not a Var, it was: "..label.type)
        "var"..(var.value\gsub "%W", (verboten)->
            if verboten == "_" then "__" else ("_%x")\format(verboten\byte!))
    
    run_macro: (tree, kind="Expression")=>
        name = @fn_name_from_tree(tree)
        unless @defs[name] and @defs[name].is_macro
            @error("Macro not found: #{name}")
        unless @check_permission(name)
            @error "You do not have the authority to call: #{name}"
        {:fn, :arg_names} = @defs[name]
        args = [a for a in *tree.value when a.type != "Word"]
        args = {name,args[i] for i,name in ipairs(arg_names[name])}
        insert @callstack, name
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
                coroutine.yield(ind(utils.repr(tree.value)))

            when "Longstring"
                -- TODO: Better implement
                coroutine.yield(ind(utils.repr(tree.value)))

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
            @writeln(line)

    stringify_tree:(tree)=>
        result = {}
        for line in coroutine.wrap(-> @_yield_tree(tree))
            insert(result, line)
        return table.concat result, "\n"

    run: (src, filename, output_file=nil)=>
        if @debug
            @writeln "COMPILING:\n#{src}"
        tree = @parse(src, filename)
        assert tree, "Tree failed to compile: #{src}"
        code, retval = @tree_to_lua(tree)
        if output_file
            output = io.open(output_file, "w")
            output\write(code)
        return retval, code

    error: (...)=>
        @writeln "ERROR!"
        @writeln(...)
        @writeln("Callstack:")
        for i=#@callstack,1,-1
            @writeln "    #{@callstack[i]}"
        @writeln "    <top level>"
        @callstack = {}
        error!

    test: (src, filename, expected)=>
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
            tree = @parse(test_src, filename)
            got = @stringify_tree(tree.value.body)
            if got != expected
                @error"TEST FAILED!\nSource:\n#{test_src}\nExpected:\n#{expected}\n\nGot:\n#{got}"


    initialize_core: =>
        -- Sets up some core functionality
        @defmacro [[lua block %lua_code]], (vars, kind)=>
            if kind == "Expression" then error("Expected to be in statement.")
            inner_vars = setmetatable({}, {__index:(_,key)-> error"vars[#{utils.repr(key)}]"})
            return "do\n"..@tree_to_value(vars.lua_code, inner_vars).."\nend", true

        @defmacro [[lua expr %lua_code]], (vars, kind)=>
            lua_code = vars.lua_code.value
            inner_vars = setmetatable({}, {__index:(_,key)-> error"vars[#{utils.repr(key)}]"})
            return @tree_to_value(vars.lua_code, inner_vars)

        @def "require %filename", (vars)=>
            if not @loaded_files[vars.filename]
                file = io.open(vars.filename)
                if not file
                    @error "File does not exist: #{vars.filename}"
                @loaded_files[vars.filename] = @run(file\read('*a'), vars.filename)
            return @loaded_files[vars.filename]

        @def "run file %filename", (vars)=>
            file = io.open(vars.filename)
            if not file
                @error "File does not exist: #{vars.filename}"
            return @run(file\read('*a'), vars.filename)


-- Run on the command line via "./nomsu.moon input_file.nom" to execute
-- and "./nomsu.moon input_file.nom output_file.lua" to compile (use "-" to compile to stdout)
if arg and arg[1]
    c = NomsuCompiler()
    input = io.open(arg[1])\read("*a")
    -- If run via "./nomsu.moon file.nom -", then silence output and print generated
    -- source code instead.
    _write = c.write
    if arg[2] == "-"
        c.write = ->
    retval, code = c\run(input, arg[1])
    c.write = _write -- put it back
    if arg[2]
        output = if arg[2] == "-"
            io.output()
        else io.open(arg[2], 'w')

        output\write [[
    local load = function()
    ]]
        output\write(code)
        output\write [[

    end
    local NomsuCompiler = require('nomsu')
    local c = NomsuCompiler()
    return load()(c, {})
    ]]

elseif arg
    -- REPL:
    c = NomsuCompiler()
    c\run('require "lib/core.nom"')
    while true
        buff = ""
        while true
            io.write(">> ")
            line = io.read("*L")
            if line == "\n" or not line
                break
            buff ..= line
        if #buff == 0
            break
        ok, ret = pcall(-> c\run(buff))
        if ok and ret != nil
            print "= "..utils.repr(ret)

return NomsuCompiler
