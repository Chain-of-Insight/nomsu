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
colors = setmetatable({}, {__index:->""})
colored = setmetatable({}, {__index:(_,color)-> ((msg)-> colors[color]..msg..colors.reset)})
{:insert, :remove, :concat} = table
--pcall = (fn,...)-> true, fn(...)

-- TODO:
-- Maybe get GOTOs working at file scope.
-- use actual variables instead of a vars table
-- consider non-linear codegen, rather than doing thunks for things like comprehensions
-- improve indentation of generated lua code
-- provide way to run precompiled nomsu -> lua code from nomsu
-- better scoping?
-- better error reporting
-- fix propagation of filename for error reporting
-- add line numbers of function calls
-- type checking?
-- Fix compiler bug that breaks when file ends with a block comment

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
-- Number 1, "String", %Var, [List], (expression), {Thunk}, \Nomsu, FunctionCall, File

nomsu = [=[
    file <- ({{| shebang?
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

    inline_thunk <- ({ {| "{" %ws? inline_statements %ws? "}" |} }) -> Thunk
    eol_thunk <- ({ {| ":" %ws? noeol_statements eol |} }) -> Thunk
    indented_thunk <- ({ {| (":" / "{..}") indent
                statements (nodent statements)*
            (dedent / (({.+} ("" -> "Error while parsing thunk")) => error))
        |} }) -> Thunk

    inline_nomsu <- ({("\" inline_expression) }) -> Nomsu
    eol_nomsu <- ({("\" noeol_expression) }) -> Nomsu
    indented_nomsu <- ({("\" expression) }) -> Nomsu

    inline_expression <- number / variable / inline_string / inline_list / inline_nomsu
        / inline_thunk / ("(" %ws? inline_statement %ws? ")")
    noeol_expression <- indented_string / indented_nomsu / indented_list / indented_thunk
        / ("(..)" indent
            statement
        (dedent / (({.+} ("" -> "Error while parsing indented expression"))))
        ) / inline_expression
    expression <- eol_thunk / eol_nomsu / noeol_expression

    -- Function calls need at least one word in them
    inline_functioncall <- ({(''=>line_no) {|
            (inline_expression tok_gap)* word (tok_gap (inline_expression / word))*
        |} }) -> FunctionCall
    noeol_functioncall <- ({(''=>line_no) {|
            (noeol_expression tok_gap)* word (tok_gap (noeol_expression / word))*
        |} }) -> FunctionCall
    functioncall <- ({(''=>line_no) {|
            (expression (dotdot / tok_gap))* word ((dotdot / tok_gap) (expression / word))*
        |} }) -> FunctionCall

    word <- ({ { (%wordbreaker+) / (!number %wordchar+) } }) -> Word
    
    inline_string <- ({ '"' {|
        ({~ (("\\" -> "\") / ('\"' -> '"') / ("\n" -> "
") / (!string_interpolation [^%nl"]))+ ~}
        / string_interpolation)* |} '"' }) -> String
    indented_string <- ({ '".."' indent {|
            indented_string_line (nodent {~ "" -> "
" ~} indented_string_line)*
          |} (dedent / (({.+} ("" -> "Error while parsing String")) => error))
        }) -> String
    indented_string_line <- "|" ({~ (("\\" -> "\") / (!string_interpolation [^%nl]))+ ~} / string_interpolation)*
    string_interpolation <- "\" ((noeol_expression dotdot?) / dotdot)

    number <- ({ (("-"? (([0-9]+ "." [0-9]+) / ("." [0-9]+) / ([0-9]+)))-> tonumber) }) -> Number

    -- Variables can be nameless (i.e. just %) and can't contain apostrophes
    -- which is a hack to allow %foo's to parse as "%foo" and "'s" separately
    variable <- ({ ("%" { ((%wordbreaker+) / (%wordchar+))? }) }) -> Var

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

    block_comment <- "#.." [^%nl]* (%nl (%ws? &%nl))* %nl %indented [^%nl]+ (%nl ((%ws? (!. / &%nl)) / (!%dedented [^%nl]+)))*
    line_comment  <- "#" [^%nl]*

    eol <- %ws? line_comment? (!. / &%nl)
    ignored_line <- (%nodented (block_comment / line_comment)) / (%ws? (!. / &%nl))
    indent <- eol (%nl ignored_line)* %nl %indented ((block_comment/line_comment) (%nl ignored_line)* nodent)?
    nodent <- eol (%nl ignored_line)* %nl %nodented
    dedent <- eol (%nl ignored_line)* (((!.) &%dedented) / (&(%nl %dedented)))
    tok_gap <- %ws / %prev_edge / &("[" / "\" / [.,:;{("#%] / &%wordbreaker)
    comma <- %ws? "," %ws?
    semicolon <- %ws? ";" %ws?
    dotdot <- nodent ".." %ws?
]=]

CURRENT_FILE = nil
whitespace = S(" \t")^1
wordbreaker = ("'~`!@$^&*-+=|<>?/")
defs =
    ws:whitespace, nl: P("\n"), :tonumber, wordbreaker:S(wordbreaker)
    wordchar: P(1)-S(' \t\n\r%#:;,.{}[]()"\\'..wordbreaker)
    indented: Cmt(S(" \t")^0 * (#(P(1)-S(" \t\n") + (-P(1)))), check_indent)
    nodented: Cmt(S(" \t")^0 * (#(P(1)-S(" \t\n") + (-P(1)))), check_nodent)
    dedented: Cmt(S(" \t")^0 * (#(P(1)-S(" \t\n") + (-P(1)))), check_dedent)
    prev_edge: B(S(" \t\n.,:;}])\"\\"..wordbreaker))
    line_no: (src, pos)->
        line_no = 1
        for _ in src\sub(1,pos)\gmatch("\n") do line_no += 1
        return pos, "#{CURRENT_FILE}:#{line_no}"
    FunctionCall: (src, line_no, value, errors)->
        {type: "FunctionCall", :src, :line_no, :value, :errors}
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
        error("\n#{err_msg or "Parse error"} in #{CURRENT_FILE} on line #{line_no}:\n\n#{prev_line}\n#{err_line}\n#{pointer}\n#{next_line}\n")

setmetatable(defs, {
    __index: (t,key)->
        with t[key] = (src, value, errors)-> {type: key, :src, :value, :errors} do nil
})
nomsu = re.compile(nomsu, defs)

class NomsuCompiler
    new:(parent)=>
        @write = (...)=> io.write(...)
        @write_err = (...)=> io.stderr\write(...)
        @defs = setmetatable({}, {__index:parent and parent.defs})
        @callstack = {}
        @debug = false
        @utils = utils
        @repr = (...)=> repr(...)
        @stringify = (...)=> utils.stringify(...)
        @loaded_files = setmetatable({}, {__index:parent and parent.loaded_files})
        if not parent
            @initialize_core!
    
    writeln:(...)=>
        @write(...)
        @write("\n")
    
    errorln:(...)=>
        @write_err(...)
        @write_err("\n")
    
    def: (signature, thunk, src, is_macro=false)=>
        if type(signature) == 'string'
            signature = @get_stubs {signature}
        elseif type(signature) == 'table' and type(signature[1]) == 'string'
            signature = @get_stubs signature
        assert type(thunk) == 'function', "Bad thunk: #{repr thunk}"
        canonical_args = nil
        aliases = {}
        for {stub, arg_names} in *signature
            assert stub, "NO STUB FOUND: #{repr signature}"
            if @debug then @writeln "#{colored.bright "DEFINING RULE:"} #{colored.underscore colored.magenta repr(stub)} #{colored.bright "WITH ARGS"} #{colored.dim repr(arg_names)}"
            for i=1,#arg_names-1 do for j=i+1,#arg_names
                if arg_names[i] == arg_names[j] then @error "Duplicate argument in function #{stub}: '#{arg_names[i]}'"
            if canonical_args
                assert utils.equivalent(utils.set(arg_names), canonical_args), "Mismatched args"
            else canonical_args = utils.set(arg_names)
            insert aliases, stub
            @defs[stub] = {:thunk, :stub, :arg_names, :src, :is_macro, :aliases}

    defmacro: (signature, thunk, src)=>
        @def(signature, thunk, src, true)

    call: (stub,line_no,...)=>
        def = @defs[stub]
        -- This is a little bit hacky, but having this check is handy for catching mistakes
        -- I use a hash sign in "#macro" so it's guaranteed to not be a valid function name
        if def and def.is_macro and @callstack[#@callstack] != "#macro"
            @error "Attempt to call macro at runtime: #{stub}\nThis can be caused by using a macro in a function that is defined before the macro."
        insert @callstack, {stub, line_no}
        if def == nil
            @error "Attempt to call undefined function: #{stub}"
        unless def.is_macro
            @assert_permission(stub)
        {:thunk, :arg_names} = def
        args = {name, select(i,...) for i,name in ipairs(arg_names)}
        if @debug
            @write "#{colored.bright "CALLING"} #{colored.magenta(colored.underscore stub)} "
            @writeln "#{colored.bright "WITH ARGS:"} #{colored.dim repr(args)}"
        -- TODO: optimize, but still allow multiple return values?
        rets = {thunk(self,args)}
        remove @callstack
        return unpack(rets)
    
    run_macro: (tree)=>
        stub = @get_stub tree
        args = [arg for arg in *tree.value when arg.type != "Word"]
        if @debug
            @write "#{colored.bright "RUNNING MACRO"} #{colored.underscore colored.magenta(stub)} "
            @writeln "#{colored.bright "WITH ARGS:"} #{colored.dim repr args}"
        insert @callstack, "#macro"
        expr, statement = @call(stub, tree.line_no, unpack(args))
        remove @callstack
        return expr, statement

    assert_permission: (stub)=>
        fn_def = @defs[stub]
        unless fn_def
            @error "Undefined function: #{fn_name}"
        whiteset = fn_def.whiteset
        if whiteset == nil then return true
        -- TODO: maybe optimize this by making the callstack a Counter and using a 
        --    move-to-front optimization on the whitelist to check most likely candidates sooner
        for caller in *@callstack
            if whiteset[caller[1]] then return true
        @error "You do not have the authority to call: #{stub}"

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
            if whiteset[caller[1]] then return true
        return false

    parse: (str, filename)=>
        if @debug
            @writeln("#{colored.bright "PARSING:"}\n#{str}")
        str = str\gsub("\r","")
        export indent_stack, CURRENT_FILE
        old_file = CURRENT_FILE
        old_indent_stack, indent_stack = indent_stack, {0}
        CURRENT_FILE = filename
        tree = nomsu\match(str)
        indent_stack = old_indent_stack -- Put it back, just in case.
        CURRENT_FILE = old_file
        assert tree, "Failed to parse: #{str}"
        if @debug
            @writeln "PARSE TREE:"
            @print_tree tree, "    "
        return tree

    run: (src, filename, vars={}, max_operations=nil)=>
        if max_operations
            timeout = ->
                debug.sethook!
                @error "Execution quota exceeded. Your code took too long."
            debug.sethook timeout, "", max_operations
        tree = @parse(src, filename)
        assert tree, "Tree failed to compile: #{src}"
        assert tree.type == "File", "Attempt to run non-file: #{tree.type}"

        buffer = {}
        return_value = nil
        for statement in *tree.value
            if @debug
                @writeln "#{colored.bright "RUNNING NOMSU:"}\n#{colored.bright colored.yellow statement.src}"
                @writeln colored.bright("PARSED TO TREE:")
                @print_tree statement
            ok,expr,statements = pcall(@tree_to_lua, self, statement)
            if not ok
                @errorln "#{colored.red "Error occurred in statement:"}\n#{colored.bright colored.yellow statement.src}"
                @error(expr)
            code_for_statement = ([[
return (function(nomsu, vars)
%s
return %s;
end);]])\format(statements or "", expr or "ret")
            if @debug
                @writeln "#{colored.bright "RUNNING LUA:"}\n#{colored.blue colored.bright(code_for_statement)}"
            lua_thunk, err = load(code_for_statement)
            if not lua_thunk
                n = 1
                fn = ->
                    n = n + 1
                    ("\n%-3d|")\format(n)
                code = "1  |"..code_for_statement\gsub("\n", fn)
                error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack code}\n\n#{err}\n\nProduced by statement:\n#{colored.bright colored.yellow statement.src}")
            run_statement = lua_thunk!
            ok,ret = pcall(run_statement, self, vars)
            if expr then return_value = ret
            if not ok
                @errorln "#{colored.red "Error occurred in statement:"}\n#{colored.yellow statement.src}"
                @errorln debug.traceback!
                @error(ret)
            insert buffer, "#{statements or ''}\n#{expr and "ret = #{expr};" or ''}"
        
        if max_operations
            debug.sethook!
        lua_code = ([[
return (function(nomsu, vars)
local ret;
%s
return ret;
end);]])\format(concat(buffer, "\n"))
        return return_value, lua_code, vars
    
    tree_to_value: (tree, vars)=>
        code = "return (function(nomsu, vars)\nreturn #{@tree_to_lua(tree)};\nend);"
        if @debug
            @writeln "#{colored.bright "RUNNING LUA TO GET VALUE:"}\n#{colored.blue colored.bright(code)}"
        lua_thunk, err = load(code)
        if not lua_thunk
            @error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack code}\n\n#{colored.red err}")
        return (lua_thunk!)(self, vars or {})

    tree_to_lua: (tree)=>
        -- Return <lua code for value>, <additional lua code>
        assert tree, "No tree provided."
        if not tree.type
            @errorln debug.traceback()
            @error "Invalid tree: #{repr(tree)}"
        switch tree.type
            when "File"
                error("Should not be converting File to lua through this function.")
            
            when "Nomsu"
                return repr(tree.value), nil

            when "Thunk"
                lua_bits = {}
                for arg in *tree.value
                    expr,statement = @tree_to_lua arg
                    if statement then insert lua_bits, statement
                    if expr then insert lua_bits, "ret = #{expr};"
                return ([[
(function(nomsu, vars)
local ret;
%s
return ret;
end)]])\format(concat(lua_bits, "\n"))

            when "FunctionCall"
                stub = @get_stub(tree)
                def = @defs[stub]
                if def and def.is_macro
                    expr, statement = @run_macro(tree)
                    if def.whiteset
                        if expr
                            expr = "(nomsu:assert_permission(#{repr stub}) and #{expr})"
                        if statement
                            statement = "nomsu:assert_permission(#{repr stub});\n"..statement
                    return expr, statement
                args = {repr(stub), repr(tree.line_no)}
                for arg in *tree.value
                    if arg.type == 'Word' then continue
                    expr,statement = @tree_to_lua arg
                    if statement
                        @error "Cannot use [[#{arg.src}]] as a function argument, since it's not an expression."
                    insert args, expr
                return @@comma_separated_items("nomsu:call(", args, ")"), nil

            when "String"
                if @debug
                    @writeln (colored.bright "STRING:")
                    @print_tree tree
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
                    if @debug
                        @writeln (colored.bright "INTERP:")
                        @print_tree bit
                        @writeln "#{colored.bright "EXPR:"} #{expr}, #{colored.bright "STATEMENT:"} #{statement}"
                    if statement
                        @error "Cannot use [[#{bit.src}]] as a string interpolation value, since it's not an expression."
                    insert concat_parts, "nomsu:stringify(#{expr})"

                if string_buffer ~= ""
                    insert concat_parts, repr(string_buffer)

                if #concat_parts == 0
                    return "''", nil
                elseif #concat_parts == 1
                    return concat_parts[1], nil
                else return "(#{concat(concat_parts, "..")})", nil

            when "List"
                items = {}
                for item in *tree.value
                    expr,statement = @tree_to_lua item
                    if statement
                        @error "Cannot use [[#{item.src}]] as a list item, since it's not an expression."
                    insert items, expr
                return @@comma_separated_items("{", items, "}"), nil

            when "Number"
                return repr(tree.value), nil

            when "Var"
                if tree.value\match("^[a-zA-Z_][a-zA-Z0-9_]*$")
                    return "vars.#{tree.value}", nil
                else
                    return "vars[#{repr tree.value}]", nil

            else
                @error("Unknown/unimplemented thingy: #{tree.type}")
    
    walk_tree: (tree, depth=0)=>
        coroutine.yield(tree, depth)
        if type(tree) != 'table' or not tree.type
            return
        switch tree.type
            when "List", "File", "Thunk", "FunctionCall", "String"
                for v in *tree.value
                    @walk_tree(v, depth+1)
            else @walk_tree(tree.value, depth+1)
        return nil

    print_tree: (tree)=>
        @write colors.bright..colors.green
        for node,depth in coroutine.wrap(-> @walk_tree tree)
            if type(node) != 'table' or not node.type
                @writeln(("    ")\rep(depth)..repr(node))
            else
                @writeln("#{("    ")\rep(depth)}#{node.type}:")
        @write colors.reset
    
    tree_to_str: (tree)=>
        bits = {}
        for node,depth in coroutine.wrap(-> @walk_tree tree)
            if type(node) != 'table' or not node.type
                insert bits, (("    ")\rep(depth)..repr(node))
            else
                insert bits, ("#{("    ")\rep(depth)}#{node.type}:")
        return concat(bits, "\n")

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
        if type(tree) != 'table' then return tree
        switch tree.type
            when "Var"
                if vars[tree.value] ~= nil
                    tree = vars[tree.value]
            when "File", "Nomsu", "Thunk", "List", "FunctionCall", "String"
                new_value = @replaced_vars tree.value, vars
                if new_value != tree.value
                    tree = {k,v for k,v in pairs(tree)}
                    tree.value = new_value
            when nil -- Raw table, probably from one of the .value of a multi-value tree (e.g. List)
                new_values = {}
                any_different = false
                for k,v in pairs tree
                    new_values[k] = @replaced_vars v, vars
                    any_different or= (new_values[k] != tree[k])
                if any_different
                    tree = new_values
        return tree

    get_stub: (x)=>
        if not x
            @error "Nothing to get stub from"
        -- Returns a single stub ("say %"), and list of arg names ({"msg"}) from a single rule def
        --   (e.g. "say %msg") or function call (e.g. FunctionCall({Word("say"), Var("msg")))
        if type(x) == 'string'
            stub = x\gsub("([#{wordbreaker}]+)"," %1 ")\gsub("%%%S+","%%")\gsub("%s+"," ")\gsub("^%s*","")\gsub("%s*$","")
            arg_names = [arg for arg in x\gmatch("%%([^%s']*)")]
            return stub, arg_names
        switch x.type
            when "String" then return @get_stub(x.value)
            when "FunctionCall"
                stub, arg_names = {}, {}, {}
                for token in *x.value
                    switch token.type
                        when "Word"
                            insert stub, token.value
                        when "Var"
                            insert stub, "%"
                            if arg_names then insert arg_names, token.value
                        else
                            insert stub, "%"
                            arg_names = nil
                return concat(stub," "), arg_names
            else @error "Unsupported get stub type: #{x.type}"
    
    get_stubs: (x)=>
        if type(x) != 'table' then return {{@get_stub(x)}}
        switch x.type
            when nil
                return [{@get_stub(i)} for i in *x]
            when "List"
                return [{@get_stub(i)} for i in *x.value]
        return {{@get_stub(x)}}

    var_to_lua_identifier: (var)=>
        -- Converts arbitrary nomsu vars to valid lua identifiers by replacing illegal
        -- characters with escape sequences
        if type(var) == 'table' and var.type == "Var"
            var = var.value
        (var\gsub "%W", (verboten)->
            if verboten == "_" then "__" else ("_%x")\format(verboten\byte!))

    error: (msg)=>
        @errorln (colored.red "ERROR!")
        if msg
            @errorln(colored.bright colored.yellow colored.onred msg)
        @errorln("Callstack:")
        maxlen = utils.max([#c[2] for c in *@callstack])
        for i=#@callstack,1,-1
            if @callstack[i] != "#macro"
                @errorln "    #{"%-#{maxlen}s"\format @callstack[i][2]}| #{@callstack[i][1]}"
        @errorln "    <top level>"
        @callstack = {}
        error!
    
    typecheck: (vars, varname, desired_type)=>
        x = vars[varname]
        if type(x) == desired_type then return x
        if type(x) == 'table' and x.type == desired_type then return x
        @error "Invalid type for %#{varname}. Expected #{desired_type}, but got #{x.type}."

    initialize_core: =>
        -- Sets up some core functionality
        -- Uses named local functions to help out callstack readability
        lua_code = (vars)=>
            inner_vars = setmetatable({}, {__index:(_,key)-> "vars[#{repr(key)}]"})
            lua = @tree_to_value(vars.code, inner_vars)
            return nil, lua
        @defmacro "lua > %code", lua_code

        lua_value = (vars)=>
            inner_vars = setmetatable({}, {__index:(_,key)-> "vars[#{repr(key)}]"})
            lua = @tree_to_value(vars.code, inner_vars)
            return lua, nil
        @defmacro "= lua %code", lua_value

        run_file = (vars)=>
            if vars.filename\match(".*%.lua")
                return dofile(vars.filename)(@, vars)
            if vars.filename\match(".*%.nom")
                if not @skip_precompiled -- Look for precompiled version
                    file = io.open(vars.filename..".lua", "r")
                    if file
                        contents = file\read('*a')
                        file\close!
                        return load(contents)!(@, vars)
                file = io.open(vars.filename)
                if not file
                    @error "File does not exist: #{vars.filename}"
                contents = file\read('*a')
                file\close!
                return @run(contents, vars.filename)
            else
                @error "Invalid filetype for #{vars.filename}"
        @def "run file %filename", run_file

        _require = (vars)=>
            if not @loaded_files[vars.filename]
                @loaded_files[vars.filename] = run_file(self, {filename:vars.filename}) or true
            return @loaded_files[vars.filename]
        @def "require %filename", _require


if arg
    export colors
    colors = require 'consolecolors'
    parser = re.compile([[
        args <- {| {:flags: flags? :} ({:input: input :} ";" ("-o;"{:output: output :} ";")?)? (";")? |} !.
        flags <- (({| ({flag} ";")* |}) -> set)
        flag <- "-c" / "-i" / "-p" / "-f" / "--help" / "-h"
        input <- "-" / [^;]+
        output <- "-" / [^;]+
    ]], {set: utils.set})
    args = concat(arg, ";")..";"
    args = parser\match(args) or {}
    if not args or not args.flags or args.flags["--help"] or args.flags["-h"]
        print "Usage: lua nomsu.lua [-c] [-i] [-p] [-f] [--help] [input [-o output]]"
        os.exit!

    c = NomsuCompiler()
    c.skip_precompiled = args.flags["-f"]
    if args.input
        -- Read a file or stdin and output either the printouts or the compiled lua
        if args.flags["-c"] and not args.output
            args.output = args.input..".lua"
        compiled_output = nil
        if args.flags["-p"]
            _write = c.write
            c.write = ->
            compiled_output = io.output()
        elseif args.output
            compiled_output = io.open(args.output, 'w')

        if args.input\match(".*%.lua")
            retval = dofile(args.input)(c, {})
        else
            input = if args.input == '-'
                io.read('*a')
            else io.open(args.input)\read("*a")
            retval, code = c\run(input, args.input)
            -- Output compile lua code
            if compiled_output
                compiled_output\write code
        if args.flags["-p"]
            c.write = _write

    if not args.input or args.flags["-i"]
        -- REPL
        c\run('require "lib/core.nom"', "stdin")
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
            ok, ret = pcall(-> c\run(buff, "stdin"))
            if ok and ret != nil
                print "= "..repr(ret)

return NomsuCompiler
