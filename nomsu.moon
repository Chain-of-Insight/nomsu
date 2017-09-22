#!/usr/bin/env moon
re = require 're'
lpeg = require 'lpeg'
utils = require 'utils'
repr = utils.repr
{:insert, :remove, :concat} = table
pcall = (fn,...)-> true, fn(...)

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

-- Helper "classes"
parsetree_mt = {__tostring:=> "#{@type}(#{repr(@value)})"}
ParseTree = (type, src, value, errors)->
    setmetatable({:type, :src, :value, :errors}, parsetree_mt)

functiondef_mt = {__tostring:=> "FunctionDef(#{repr(@aliases)}"}
FunctionDef = (fn, aliases, src, is_macro)->
    setmetatable({:fn, :aliases, :src, :is_macro}, functiondef_mt)

class NomsuCompiler
    new:(parent)=>
        @write = (...)=> io.write(...)
        @defs = setmetatable({}, {__index:parent and parent.defs})
        @callstack = {}
        @debug = false
        @initialize_core!
        @utils = utils
        @repr = (...)=> repr(...)
        @loaded_files = {}
    
    writeln:(...)=>
        @write(...)
        @write("\n")
    
    def: (aliases, fn, src, is_macro=false)=>
        if type(aliases) == 'string'
            aliases = @get_aliases aliases
        if @debug
            @writeln "Defining rule: #{aliases}"
        fn_def = FunctionDef(fn, {}, src, is_macro)
        @add_aliases aliases, fn_def

    defmacro: (aliases, fn, src)=> @def(aliases, fn, src, true)

    add_aliases: (aliases, fn_def)=>
        first_alias,first_args = next(fn_def.aliases)
        if not first_alias
            first_alias,first_args = next(aliases)
        for alias,args in pairs(aliases)
            if fn_def[alias] then continue
            if @defs[alias] then @remove_alias(alias)
            if alias != first_alias and not utils.equivalent(utils.set(args), utils.set(first_args))
                @error "Conflicting argument names between #{first_alias} and #{alias}"
            fn_def.aliases[alias] = args
            @defs[alias] = fn_def
    
    remove_alias: (alias)=>
        fn_def = @defs[alias]
        if not fn_def then return
        fn_def.aliases[alias] = nil
        @defs[alias] = nil

    remove_aliases: (aliases)=>
        for alias in pairs(aliases) do @remove_alias(alias)

    get_fn_def: (x)=>
        if not x then @error "Nothing to get function def from"
        aliases = @get_aliases x
        alias,_ = next(aliases)
        return @defs[alias]

    call: (alias,...)=>
        fn_def = @defs[alias]
        if fn_def == nil
            @error "Attempt to call undefined function: #{alias}"
        if fn_def.is_macro and @callstack[#@callstack] != "__macro__"
            @error "Attempt to call macro at runtime: #{alias}\nThis can be caused by using a macro in a function that is defined before the macro."
        unless @check_permission(alias)
            @error "You do not have the authority to call: #{alias}"
        insert @callstack, alias
        {:fn, :aliases} = fn_def
        args = {name, select(i,...) for i,name in ipairs(aliases[alias])}
        if @debug
            @writeln "Calling #{alias} with args: #{repr(args)}"
        -- TODO: optimize, but still allow multiple return values?
        rets = {fn(self,args)}
        remove @callstack
        return unpack(rets)
    
    run_macro: (tree, kind="Expression")=>
        args = [a for a in *tree.value when a.type != "Word"]
        alias,_ = @get_alias tree
        insert @callstack, "__macro__"
        ret, manual_mode = @call(alias, unpack(args))
        remove @callstack
        if not ret
            @error("No return value for macro: #{name}")
        if kind == "Statement" and not manual_mode
            if ret\match("^do\n")
                error "Attempting to use macro return value as an expression, when it looks like a block:\n#{ret}"
            ret = "ret = "..ret
        return ret

    check_permission: (fn_name)=>
        fn_def = @defs[fn_name]
        if fn_def == nil
            @error "Undefined function: #{fn_name}"
        if fn_def.whiteset == nil then return true
        -- TODO: optimize this, maybe by making the callstack a Counter and having a move-to-front optimization on the whitelist
        for caller in *@callstack
            if fn_def.whiteset[caller]
                return true
        return false

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
                remove(indent_stack)
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

        tree_mt = {__tostring:=> "#{@type}(#{repr(@value)})"}
        setmetatable(defs, {
            __index: (t,key)->
                fn = (src, value, errors)-> setmetatable({type: key, :src, :value, :errors}, tree_mt)
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
            @error "Invalid tree: #{repr(tree)}"
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
                        error("Failed to compile generated code:\n#{code}\n\n#{err}\n\nProduced by statement:\n#{repr(statement)}")
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
                return concat(buffer, "\n"), return_value

            when "Block"
                buffer = {}
                for statement in *tree.value
                    insert buffer, @tree_to_lua(statement)
                return concat(buffer, "\n")
        
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
                    alias = @get_alias(tree.value)
                    if @defs[alias] and @defs[alias].is_macro
                        return @run_macro(tree.value, "Statement")
                return "ret = "..(@tree_to_lua(tree.value))

            when "FunctionCall"
                alias = @get_alias(tree)
                if @defs[alias] and @defs[alias].is_macro
                    return @run_macro(tree, "Expression")
                else
                    args = [@tree_to_lua(a) for a in *tree.value when a.type != "Word"]
                    insert args, 1, repr(alias)
                    return @@comma_separated_items("compiler:call(", args, ")")

            when "String"
                return repr(@@unescape_string(tree.value))

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
                                insert concat_parts, repr(string_buffer)
                                string_buffer = ""
                            insert concat_parts, "compiler.utils.repr_if_not_string(#{@tree_to_lua(bit)})"

                if string_buffer ~= ""
                    insert concat_parts, repr(string_buffer)

                if #concat_parts == 0
                    return "''"
                elseif #concat_parts == 1
                    return concat_parts[1]
                else
                    return "(#{concat(concat_parts, "..")})"

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
                return "vars[#{repr(tree.value)}]"

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
        return concat(bits)

    get_alias: (x)=>
        if not x then @error "Nothing to get alias from"
        -- Returns a single alias ("say %"), and list of args ({msg}) from a single rule def
        --   (e.g. "say %msg") or function call (e.g. FunctionCall({Word("say"), Var("msg")))
        if type(x) == 'string'
            -- TODO
            alias = x\gsub("'"," '")\gsub("%%%S+","%%")\gsub("%s+"," ")
            args = [arg for arg in x\gmatch("%%(%S[^%s']*)")]
            return alias, args
        switch x.type
            when "String" then return @get_alias(x.value)
            when "Statement" then return @get_alias(x.value)
            when "FunctionCall"
                alias, args = {}, {}, {}
                for token in *x.value
                    switch token.type
                        when "Word"
                            insert alias, token.value
                        when "Var"
                            insert alias, "%"
                            insert args, token.value
                        else
                            insert alias, "%"
                            insert args, token
                return concat(alias," "), args

    get_aliases:(x)=>
        if self.value
            print self
            error "WTF"
        if not x then @error "Nothing to get aliases from"
        if type(x) == 'string'
            alias, args = @get_alias(x)
            return {[alias]: args}
        switch x.type
            when "String" then return @get_aliases({x.value})
            when "Statement" then return @get_aliases({x.value})
            when "FunctionCall" then return @get_aliases({x})
            when "List" then x = x.value
            when "Block" then x = x.value
        with {}
            for y in *x
                alias,args = @get_alias(y)
                [alias] = args

    var_to_lua_identifier: (var)=>
        -- Converts arbitrary nomsu vars to valid lua identifiers by replacing illegal
        -- characters with escape sequences
        if var.type != "Var"
            @error("Tried to convert something that wasn't a Var into a lua identifier: it was not a Var, it was: "..label.type)
        "var"..(var.value\gsub "%W", (verboten)->
            if verboten == "_" then "__" else ("_%x")\format(verboten\byte!))

    _yield_tree: (tree, indent_level=0)=>
        ind = (s) -> INDENT\rep(indent_level)..s
        switch tree.type
            when "File"
                coroutine.yield(ind"File:")
                @_yield_tree(tree.value.body, indent_level+1)
            when "Errors" then coroutine.yield(ind"Error:\n#{tree.value}")
            when "Block"
                for chunk in *tree.value
                    @_yield_tree(chunk, indent_level)
            when "Thunk"
                coroutine.yield(ind"Thunk:")
                @_yield_tree(tree.value, indent_level+1)
            when "Statement" then @_yield_tree(tree.value, indent_level)
            when "FunctionCall"
                alias = @get_alias tree
                args = [a for a in *tree.value when a.type != "Word"]
                if #args == 0
                    coroutine.yield(ind"Call [#{alias}]!")
                else
                    coroutine.yield(ind"Call [#{alias}]:")
                    for a in *args
                        @_yield_tree(a, indent_level+1)
            when "String" then coroutine.yield(ind(repr(tree.value)))
            when "Longstring" then coroutine.yield(ind(repr(tree.value)))
            when "Number" then coroutine.yield(ind(tree.value))
            when "Var" then coroutine.yield ind"Var[#{repr(tree.value)}]"
            when "List"
                if #tree.value == 0
                    coroutine.yield(ind("<Empty List>"))
                else
                    coroutine.yield(ind"List:")
                    for item in *tree.value
                        @_yield_tree(item, indent_level+1)
            else error("Unknown/unimplemented thingy: #{tree.type}")

    print_tree:(tree)=>
        for line in coroutine.wrap(-> @_yield_tree(tree))
            @writeln(line)

    stringify_tree:(tree)=>
        result = {}
        for line in coroutine.wrap(-> @_yield_tree(tree))
            insert(result, line)
        return concat result, "\n"

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
        @defmacro "lua block %lua_code", (vars, kind)=>
            if kind == "Expression" then error("Expected to be in statement.")
            inner_vars = setmetatable({}, {__index:(_,key)-> error"vars[#{repr(key)}]"})
            return "do\n"..@tree_to_value(vars.lua_code, inner_vars).."\nend", true

        @defmacro "lua expr %lua_code", (vars, kind)=>
            lua_code = vars.lua_code.value
            inner_vars = setmetatable({}, {__index:(_,key)-> error"vars[#{repr(key)}]"})
            return @tree_to_value(vars.lua_code, inner_vars)

        @def "require %filename", (vars)=>
            if not @loaded_files[vars.filename]
                file = io.open(vars.filename)
                if not file
                    @error "File does not exist: #{vars.filename}"
                @loaded_files[vars.filename] = (@run(file\read('*a'), vars.filename)) or true
            return @loaded_files[vars.filename]

        @def "run file %filename", (vars)=>
            file = io.open(vars.filename)
            if not file
                @error "File does not exist: #{vars.filename}"
            return @run(file\read('*a'), vars.filename)


-- Run on the command line via "./nomsu.moon input_file.nom" to execute
-- and "./nomsu.moon input_file.nom output_file.lua" to compile (use "-" to compile to stdout)
if arg and arg[1]
    --ProFi = require 'ProFi'
    --ProFi\start()
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
    --ProFi\stop()
    --ProFi\writeReport( 'MyProfilingReport.txt' )

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
            print "= "..repr(ret)

return NomsuCompiler
