#!/usr/bin/env moon
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
--        lua nomsu.lua [input_file [output_file or -]]

re = require 're'
lpeg = require 'lpeg'
utils = require 'utils'
repr = utils.repr
{:insert, :remove, :concat} = table
pcall = (fn,...)-> true, fn(...)

-- TODO:
-- use actual variables instead of a vars table
-- have macros return (statements, expression)
-- consider non-linear codegen, like with moonscript's comprehensions, rather than doing thunks
-- improve indentation of generated lua code
-- provide way to run precompiled nomsu -> lua code from nomsu
-- better scoping?
-- better error reporting
-- add line numbers of function calls
-- type checking?

lpeg.setmaxstack 10000 -- whoa
{:P,:V,:S,:Cg,:C,:Cp,:B,:Cmt} = lpeg
STRING_ESCAPES = n:"\n", t:"\t", b:"\b", a:"\a", v:"\v", f:"\f", r:"\r"

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

-- TYPES:
-- Number 1, "String", %Var, [List], (Block), \(Nomsu), FunctionCall, File

nomsu = [=[
    file <- ({ {| shebang?
        (ignored_line %nl)*
        statements (nodent statements)*
        (%nl ignored_line)* %nl?
        (({.+} ("" -> "Unexpected end of file")) => error)? |} }) -> File

    shebang <- "#!" [^%nl]* %nl

    inline_statements <- inline_statement (semicolon inline_statement)*
    noeol_statements <- (inline_statement semicolon)* noeol_statement
    statements <- (inline_statement semicolon)* statement

    statement <- functioncall / expression
    noeol_statement <- noeol_functioncall / noeol_expression
    inline_statement <- inline_functioncall / inline_expression

    inline_block <- ({ {| "(" inline_statements ")" |} }) -> Block
    eol_block <- ({ {| ":" %ws? noeol_statements eol |} }) -> Block
    indented_block <- ({ {| (":" / "(..)") indent
                statements
            (dedent / (({.+} ("" -> "Error while parsing block")) => error))
        |} }) -> Block

    inline_nomsu <- ({ ("\" inline_block ) }) -> Nomsu
    eol_nomsu <- ({ ("\" eol_block ) }) -> Nomsu
    indented_nomsu <- ({ ("\" {indented_block} ) }) -> Nomsu

    inline_expression <- number / variable / inline_string / inline_list / inline_block / inline_nomsu
    noeol_expression <- indented_string / indented_block / indented_nomsu / indented_list / inline_expression
    expression <- eol_block / eol_nomsu / noeol_expression

    -- Function calls need at least one word in them
    inline_functioncall <- ({ {|
            (inline_expression tok_gap)* word (tok_gap (inline_expression / word))*
        |} }) -> FunctionCall
    noeol_functioncall <- ({ {|
            (noeol_expression tok_gap)* word (tok_gap (noeol_expression / word))*
        |} }) -> FunctionCall
    functioncall <- ({ {|
            (expression (dotdot / tok_gap))* word ((dotdot / tok_gap) (expression / word))*
        |} }) -> FunctionCall

    word <- ({ !number {%wordchar (!"'" %wordchar)*} }) -> Word
    
    inline_string <- ({ '"' {|
        ({~ (("\\" -> "\") / ('\"' -> '"') / (!string_interpolation [^%nl"]))+ ~}
        / string_interpolation)* |} '"' }) -> String
    indented_string <- ({ '".."' indent {|
            indented_string_line (nodent {~ "" -> "
" ~} indented_string_line)*
          |} (dedent / (({.+} ("" -> "Error while parsing String")) => error))
        }) -> String
    indented_string_line <- "|" ({~ (("\\" -> "\") / (!string_interpolation [^%nl]))+ ~} / string_interpolation)*
    string_interpolation <- "\" (inline_block / indented_block / dotdot)

    number <- ({ (("-"? (([0-9]+ "." [0-9]+) / ("." [0-9]+) / ([0-9]+)))-> tonumber) }) -> Number

    -- Variables can be nameless (i.e. just %) and can't contain apostrophes
    -- which is a hack to allow %foo's to parse as "%foo" and "'s" separately
    variable <- ({ ("%" { (!"'" %wordchar)* }) }) -> Var

    inline_list <- ({ {|
         ("[" %ws? ((inline_list_item comma)* inline_list_item comma?)? %ws? "]")
      |} }) -> List
    indented_list <- ({ {|
         ("[..]" indent
                list_line (nodent list_line)*
          (dedent / (({.+} ("" -> "Error while parsing list")) => error)))
      |} }) -> List
    list_line <- (inline_list_item comma)* ((inline_list_item %ws? ",") / (functioncall / expression))
    inline_list_item <- inline_functioncall / inline_expression

    block_comment <- "#.." [^%nl]* indent [^%nl]* (%nl ((%ws? (!. / &%nl)) / (!%dedented [^%nl]*)))* 
    line_comment  <- "#" [^%nl]*

    eol <- %ws? line_comment? (!. / &%nl)
    ignored_line <- (%nodented (block_comment / line_comment)) / (%ws? (!. / &%nl))
    indent <- eol (%nl ignored_line)* %nl %indented
    nodent <- eol (%nl ignored_line)* %nl %nodented
    dedent <- eol (%nl ignored_line)* (((!.) &%dedented) / (&(%nl %dedented)))
    tok_gap <- %ws / %prev_edge / &("[" / "\" / [.,:;{("#%'])
    comma <- %ws? "," %ws?
    semicolon <- %ws? ";" %ws?
    dotdot <- nodent ".." %ws?
]=]

whitespace = S(" \t")^1
defs =
    ws:whitespace, nl: P("\n"), :tonumber
    wordchar: P(1)-S(' \t\n\r%#:;,.{}[]()"\\')
    indented: Cmt(S(" \t")^0 * (#(P(1)-S(" \t\n") + (-P(1)))), check_indent)
    nodented: Cmt(S(" \t")^0 * (#(P(1)-S(" \t\n") + (-P(1)))), check_nodent)
    dedented: Cmt(S(" \t")^0 * (#(P(1)-S(" \t\n") + (-P(1)))), check_dedent)
    prev_edge: B(S(" \t\n.,:;}])\"\\"))
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
        with t[key] = (src, value, errors)-> {type: key, :src, :value, :errors} do nil
})
nomsu = re.compile(nomsu, defs)

class NomsuCompiler
    new:(parent)=>
        @write = (...)=> io.write(...)
        @defs = setmetatable({}, {__index:parent and parent.defs})
        @callstack = {}
        @debug = false
        @utils = utils
        @repr = (...)=> repr(...)
        @loaded_files = {}
        @initialize_core!
    
    writeln:(...)=>
        @write(...)
        @write("\n")
    
    def: (invocation, thunk, src)=>
        if type(invocation) != 'string' then @error "Invocation should be string, not: #{repr invocation}"
        if @debug then @writeln "Defining rule: #{repr invocation}"
        stub = invocation\gsub("'"," '")\gsub("%%%S+","%%")\gsub("%s+"," ")
        args = [arg for arg in invocation\gmatch("%%(%S[^%s']*)")]
        for i=1,#args-1 do for j=i+1,#args
            if args[i] == args[j] then @error "Duplicate argument in function def: #{args[i]}"
        with @defs[invocation] = {:thunk, :invocation, :args, :src, is_macro:false} do nil

    defmacro: (invocation, thunk, src)=>
        with @def(invocation, thunk, src) do .is_macro = true

    call: (alias,...)=>
        def = @defs[alias]
        if def == nil
            @error "Attempt to call undefined function: #{alias}"
        -- This is a little bit hacky, but having this check is handy for catching mistakes
        -- I use a hash sign in "#macro" so it's guaranteed to not be a valid function name
        if def.is_macro and @callstack[#@callstack] != "#macro"
            @error "Attempt to call macro at runtime: #{alias}\nThis can be caused by using a macro in a function that is defined before the macro."
        unless @check_permission(def)
            @error "You do not have the authority to call: #{alias}"
        {:thunk, :args} = def
        args = {name, select(i,...) for i,name in ipairs(args)}
        if @debug
            @writeln "Calling #{repr alias} with args: #{repr(args)}"
        insert @callstack, alias
        -- TODO: optimize, but still allow multiple return values?
        rets = {thunk(self,args)}
        remove @callstack
        return unpack(rets)
    
    run_macro: (tree, kind="Expression")=>
        local args, alias
        alias,args = @get_alias tree
        insert @callstack, "#macro"
        expr, statement = @call(alias, unpack(args))
        remove @callstack
        return expr, statement

    check_permission: (fn_def)=>
        if getmetatable(fn_def) != functiondef_mt
            fn_name = fn_def
            fn_def = @defs[fn_name]
            if fn_def == nil
                @error "Undefined function: #{fn_name}"
        whiteset = fn_def.whiteset
        if whiteset == nil then return true
        -- TODO: maybe optimize this by making the callstack a Counter and using a 
        --    move-to-front optimization on the whitelist to check most likely candidates sooner
        for caller in *@callstack
            if whiteset[caller] then return true
        return false

    parse: (str, filename)=>
        if @debug
            @writeln("PARSING:\n#{str}")
        str = str\gsub("\r","")
        export indent_stack
        old_indent_stack, indent_stack = indent_stack, {0}
        tree = nomsu\match(str)
        indent_stack = old_indent_stack -- Put it back, just in case.
        assert tree, "Failed to parse: #{str}"
        if @debug
            @writeln "PARSE TREE:"
            @print_tree tree, "    "
        return tree

    run: (src, filename)=>
        tree = @parse(src, filename)
        assert tree, "Tree failed to compile: #{src}"
        assert tree.type == "File"

        buffer = {}
        vars = {}
        return_value = nil
        for statement in *tree.value
            ok,expr,statements = pcall(@tree_to_lua, self, statement)
            if not ok
                @writeln "Error occurred in statement:\n#{statement.src}"
                @error(expr)
            code_for_statement = ([[
                return (function(nomsu, vars)
                    %s
                    return %s
                end)]])\format(statements or "", expr or "")
            if @debug
                @writeln "RUNNING LUA:\n#{code_for_statement}"
            lua_thunk, err = load(code_for_statement)
            if not lua_thunk
                error("Failed to compile generated code:\n#{code_for_statement}\n\n#{err}\n\nProduced by statement:\n#{statement.src}")
            run_statement = lua_thunk!
            ok,ret = pcall(run_statement, self, vars)
            if expr then return_value = ret
            if not ok
                @writeln "Error occurred in statement:\n#{statement.src}"
                @error(return_value)
            insert buffer, "#{statements or ''}\n#{expr and "ret = #{expr}" or ''}"
        
        lua_code = ([[
            return function(nomsu, vars)
                local ret
                %s
                return ret
            end]])\format(concat(buffer, "\n"))
        return return_value, lua_code
    
    tree_to_value: (tree, vars)=>
        code = "
        return (function(nomsu, vars)\nreturn #{@tree_to_lua(tree)}\nend)"
        lua_thunk, err = load(code)
        if not lua_thunk
            error("Failed to compile generated code:\n#{code}\n\n#{err}")
        return (lua_thunk!)(self, vars or {})

    tree_to_lua: (tree)=>
        -- Return <lua code for value>, <additional lua code>
        assert tree, "No tree provided."
        if not tree.type
            @error "Invalid tree: #{repr(tree)}"
        switch tree.type
            when "File"
                error("Should not be converting File to lua through this function.")
            
            when "Nomsu"
                return repr(tree.value), nil

            when "Block"
                lua_bits = {}
                for arg in *tree.value
                    expr,statement = @tree_to_lua arg
                    -- Optimization for common case
                    if expr and not statement and #tree.value == 1
                        return expr, nil
                    if statement then insert lua_bits, statement
                    if expr then insert lua_bits, "ret = #{expr}"
                return ([[
                    function(nomsu, vars)
                        local ret
                        %s
                        return ret
                    end]])\format(concat lua_bits, "\n")

            when "FunctionCall"
                alias = @get_alias(tree)
                if @defs[alias] and @defs[alias].is_macro
                    return @run_macro(tree, "Expression")
                args = {repr(alias)}
                for arg in *tree.value
                    if arg.type == 'Word' then continue
                    expr,statement = @tree_to_lua arg
                    if statement
                        @error "Cannot use [[#{arg.src}]] as a function argument, since it's not an expression."
                    insert args, expr
                return @@comma_separated_items("nomsu:call(", args, ")"), nil

            when "String"
                concat_parts = {}
                string_buffer = ""
                for bit in *tree.value
                    if type(bit) == "string"
                        string_buffer ..= bit
                        continue
                    if string_buffer ~= ""
                        insert concat_parts, repr(string_buffer)
                        string_buffer = ""
                    expr, statement = @tree_to_lua bit
                    if statement
                        @error "Cannot use [[#{bit.src}]] as a string interpolation value, since it's not an expression."
                    insert concat_parts, "nomsu.utils.repr_if_not_string(#{expr})"

                if string_buffer ~= ""
                    insert concat_parts, repr(string_buffer)

                return "(#{concat(concat_parts, "..")})", nil

            when "List"
                items = {}
                for item in *tree.value
                    expr,statement = @tree_to_lua item
                    if statement
                        @error "Cannot use [[#{item.src}]] as a list item, since it's not an expression."
                    insert items, expr
                return @@comma_separated_items("{", items, "}"), nil

            when "Number"
                return repr(tree.value)

            when "Var"
                return "vars[#{repr tree.value}]"

            else
                @error("Unknown/unimplemented thingy: #{tree.type}")
    
    print_tree: (tree, ind="")=>
        if type(tree) ~= 'table' or not tree.type
            @writeln "#{ind}#{repr tree}"
            return
        @writeln "#{ind}#{tree.type}:"
        switch tree.type
            when "List", "File", "Block", "FunctionCall", "String"
                for v in *tree.value
                    @print_tree(v, ind.."    ")
            else @print_tree(tree.value, ind.."    ")

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

    replaced_vars: (tree, vars)=>
        -- TODO: consider making a pure function version of this that copies instead of modifying
        if type(tree) != 'table' then return tree
        switch tree.type
            when "Var"
                if vars[tree.value]
                    tree = vars[tree.value]
            when "File", "Thunk", "Statement", "Block", "List", "FunctionCall", "String"
                new_value = @replaced_vars tree.value
                if new_value != tree.value
                    tree = {k,v for k,v in pairs(tree)}
                    tree.value = new_value
            when nil -- Raw table, probably from one of the .value of a multi-value tree (e.g. List)
                new_values = {}
                any_different = false
                for k,v in pairs tree
                    new_values[k] = @replaced_vars v
                    any_different or= (new_values[k] != tree[k])
                if any_different
                    tree = new_values
        return tree

    get_alias: (x)=>
        if not x then @error "Nothing to get alias from"
        -- Returns a single alias ("say %"), and list of args ({msg}) from a single rule def
        --   (e.g. "say %msg") or function call (e.g. FunctionCall({Word("say"), Var("msg")))
        if type(x) == 'string'
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
        if type(var) == 'table' and var.type == "Var"
            var = var.value
        (var\gsub "%W", (verboten)->
            if verboten == "_" then "__" else ("_%x")\format(verboten\byte!))

    error: (...)=>
        @writeln "ERROR!"
        @writeln(...)
        @writeln("Callstack:")
        for i=#@callstack,1,-1
            @writeln "    #{@callstack[i]}"
        @writeln "    <top level>"
        @callstack = {}
        error!

    initialize_core: =>
        -- Sets up some core functionality
        @defmacro "lua code %statements with value %value", (vars)=>
            inner_vars = setmetatable({}, {__index:(_,key)-> "vars[#{repr(key)}]"})
            statements = @tree_to_value(vars.statements, inner_vars)
            value = @tree_to_value(vars.value, inner_vars)
            return value, statements
        
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


if arg and arg[1]
    --ProFi = require 'ProFi'
    --ProFi\start()
    c = NomsuCompiler()
    c.debug = true
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
        output\write ([[
    local NomsuCompiler = require('nomsu')
    local c = NomsuCompiler()
    local run = %s
    return run(c, {})
    ]])\format(code)
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
