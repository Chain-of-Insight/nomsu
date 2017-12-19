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
{:repr, :stringify, :min, :max, :equivalent, :set, :is_list, :sum} = utils
colors = setmetatable({}, {__index:->""})
colored = setmetatable({}, {__index:(_,color)-> ((msg)-> colors[color]..msg..colors.reset)})
{:insert, :remove, :concat} = table
--pcall = (fn,...)-> true, fn(...)
if _VERSION == "Lua 5.1"
    xp = xpcall
    xpcall = (f, errhandler, ...)->
        args = {n:select("#", ...), ...}
        return xp((...)-> f(unpack(args,1,args.n))), errhandler
--pcall = (fn, ...) -> xpcall(fn, debug.traceback, ...)

-- TODO:
-- Maybe get GOTOs working at file scope.
-- use actual variables instead of a vars table
-- consider non-linear codegen, rather than doing thunks for things like comprehensions
-- improve indentation of generated lua code
-- better scoping?
-- better error reporting
-- fix propagation of filename for error reporting
-- type checking?
-- Fix compiler bug that breaks when file ends with a block comment
-- Add compiler options for optimization level (compile-fast vs. run-fast, etc.)

lpeg.setmaxstack 10000 -- whoa
{:P,:R,:V,:S,:Cg,:C,:Cp,:B,:Cmt} = lpeg
STRING_ESCAPES = n:"\n", t:"\t", b:"\b", a:"\a", v:"\v", f:"\f", r:"\r"

-- NOTE: this treats tabs as equivalent to 1 space
indent_stack = {0}
indent_patt = P (start)=>
    spaces = @match("[ \t]*", start)
    if #spaces > indent_stack[#indent_stack]
        insert(indent_stack, #spaces)
        return start + #spaces
dedent_patt = P (start)=>
    spaces = @match("[ \t]*", start)
    if #spaces < indent_stack[#indent_stack]
        remove(indent_stack)
        return start
nodent_patt = P (start)=>
    spaces = @match("[ \t]*", start)
    if #spaces == indent_stack[#indent_stack]
        return start + #spaces
gt_nodent_patt = P (start)=>
    -- Note! This assumes indent is 4 spaces!!!
    spaces = @match("[ \t]*", start)
    if #spaces >= indent_stack[#indent_stack] + 4
        return start + indent_stack[#indent_stack] + 4

-- TYPES:
-- Number 1, "String", %Var, [List], (expression), {Thunk}, \Nomsu, FunctionCall, File

nomsu = [=[
    file <- ({{| shebang?
        (ignored_line %nl)*
        statements (nodent statements)*
        (%nl ignored_line)* %nl?
        (({.+} ("" -> "Parse error")) => error)? |} }) -> File

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
            (inline_expression %ws?)* word (%ws? (inline_expression / word))*
        |} }) -> FunctionCall
    noeol_functioncall <- ({(''=>line_no) {|
            (noeol_expression %ws?)* word (%ws? (noeol_expression / word))*
        |} }) -> FunctionCall
    functioncall <- ({(''=>line_no) {|
            (expression (dotdot / %ws?))* word ((dotdot / %ws?) (expression / word))*
        |} }) -> FunctionCall

    word <- ({ { %operator / (!number %plain_word) } }) -> Word
    
    inline_string <- ({ '"' {|
        ({~ (("\\" -> "\") / ('\"' -> '"') / ("\n" -> "
") / (!string_interpolation [^%nl"]))+ ~}
        / string_interpolation)* |} '"' }) -> String

    indented_string <- ({ '".."' %ws? line_comment? %nl %gt_nodented? {|
        ({~ (("\\" -> "\") / (%nl+ {~ %gt_nodented -> "" ~}) / [^%nl\]) ~} / string_interpolation)*
    |} ((!.) / (&(%nl+ !%gt_nodented)) / (({.+} ("" -> "Error while parsing String")) => error))
        }) -> String

    string_interpolation <- "\" ((noeol_expression dotdot?) / dotdot)

    number <- ({ (("-"? (([0-9]+ "." [0-9]+) / ("." [0-9]+) / ([0-9]+)))-> tonumber) }) -> Number

    -- Variables can be nameless (i.e. just %) and can't contain operators like apostrophe
    -- which is a hack to allow %'s to parse as "%" and "' s" separately
    variable <- ({ ("%" { %plain_word? }) }) -> Var

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
    comma <- %ws? "," %ws?
    semicolon <- %ws? ";" %ws?
    dotdot <- nodent ".." %ws?
]=]

CURRENT_FILE = nil
whitespace = S(" \t")^1
operator = S("'~`!@$^&*-+=|<>?/")^1
utf8_continuation = R("\128\191")
utf8_char = (
    R("\194\223")*utf8_continuation +
    R("\224\239")*utf8_continuation*utf8_continuation +
    R("\240\244")*utf8_continuation*utf8_continuation*utf8_continuation)
plain_word = (R('az','AZ','09') + S("_") + utf8_char)^1
defs =
    ws:whitespace, nl: P("\n"), :tonumber, :operator, :plain_word
    indented: indent_patt, nodented: nodent_patt, dedented: dedent_patt, gt_nodented: gt_nodent_patt
    line_no: (src, pos)->
        line_no = 1
        for _ in src\sub(1,pos)\gmatch("\n") do line_no += 1
        return pos, "#{CURRENT_FILE}:#{line_no}"
    FunctionCall: (src, line_no, value, errors)->
        stub = concat([(t.type == "Word" and t.value or "%") for t in *value], " ")
        {type: "FunctionCall", :src, :line_no, :value, :errors, :stub}
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
    @def_number: 0
    new:(parent)=>
        @write = (...)=> io.write(...)
        @write_err = (...)=> io.stderr\write(...)
        -- Use # to prevent someone from defining a function that has a namespace collision.
        @defs = {["#vars"]:{}, ["#loaded_files"]:{}}
        if parent
            setmetatable(@defs, {__index:parent.defs})
            setmetatable(@defs["#vars"], {__index:parent["#vars"]})
            setmetatable(@defs["#loaded_files"], {__index:parent["#loaded_files"]})
        @callstack = {}
        @compilestack = {}
        @debug = false
        @utils = utils
        @repr = (...)=> repr(...)
        @stringify = (...)=> stringify(...)
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
        if @debug
            @write colored.magenta "Defined rule #{repr signature}"
        assert type(thunk) == 'function', "Bad thunk: #{repr thunk}"
        canonical_args = nil
        canonical_escaped_args = nil
        aliases = {}
        @@def_number += 1
        def = {:thunk, :src, :is_macro, aliases:{}, def_number:@@def_number, defs:@defs}
        where_defs_go = (getmetatable(@defs) or {}).__newindex or @defs
        for {stub, arg_names, escaped_args} in *signature
            assert stub, "NO STUB FOUND: #{repr signature}"
            if @debug then @writeln "#{colored.bright "DEFINING RULE:"} #{colored.underscore colored.magenta repr(stub)} #{colored.bright "WITH ARGS"} #{colored.dim repr(arg_names)}"
            for i=1,#arg_names-1 do for j=i+1,#arg_names
                if arg_names[i] == arg_names[j] then @error "Duplicate argument in function #{stub}: '#{arg_names[i]}'"
            if canonical_args
                assert equivalent(set(arg_names), canonical_args), "Mismatched args"
            else canonical_args = set(arg_names)
            if canonical_escaped_args
                assert equivalent(escaped_args, canonical_escaped_args), "Mismatched escaped args"
            else
                canonical_escaped_args = escaped_args
                def.escaped_args = escaped_args
            insert def.aliases, stub
            stub_def = setmetatable({:stub, :arg_names, :escaped_args}, {__index:def})
            rawset(where_defs_go, stub, stub_def)

    defmacro: (signature, thunk, src)=>
        @def(signature, thunk, src, true)

    scoped: (thunk)=>
        old_defs = @defs
        new_defs =
            ["#vars"]: setmetatable({}, {__index:@defs["#vars"]})
            ["#loaded_files"]: setmetatable({}, {__index:@defs["#loaded_files"]})
        @defs = setmetatable(new_defs, {__index:old_defs})
        ok, ret1, ret2 = pcall thunk, @
        @defs = old_defs
        if not ok then @error(ret1)
        return ret1, ret2

    serialize_defs: (scope=nil, after=nil)=>
        after or= @core_defs or 0
        scope or= @defs
        defs_by_num = {}
        for stub, def in pairs(scope)
            if def and stub\sub(1,1) != "#"
                defs_by_num[def.def_number] = def
        keys = [k for k,v in pairs(defs_by_num)]
        table.sort(keys)

        buff = {}
        k_i = 1
        _using = nil
        _using_do = {}
        for k_i,i in ipairs(keys)
            if i <= after then continue
            def = defs_by_num[i]
            if def.defs == scope
                if def.src
                    insert buff, def.src
                continue
            if _using == def.defs
                if def.src
                    insert _using_do, def.src
            else
                _using = def.defs
                _using_do = {def.src}
            if k_i == #keys or defs_by_num[keys[k_i+1]].defs != _using
                insert buff, "using:\n    #{@indent @serialize_defs(_using)}\n..do:\n    #{@indent concat(_using_do, "\n")}"

        for k,v in pairs(scope["#vars"] or {})
            insert buff, "<%#{k}> = #{@value_to_nomsu v}"

        return concat buff, "\n"

    call: (stub,line_no,...)=>
        def = @defs[stub]
        -- This is a little bit hacky, but having this check is handy for catching mistakes
        -- I use a hash sign in "#macro" so it's guaranteed to not be a valid function name
        if def and def.is_macro and @callstack[#@callstack] != "#macro"
            @error "Attempt to call macro at runtime: #{stub}\nThis can be caused by using a macro in a function that is defined before the macro."
        insert @callstack, {stub, line_no}
        unless def
            @error "Attempt to call undefined function: #{stub}"
        unless def.is_macro
            @assert_permission(stub)
        {:thunk, :arg_names} = def
        args = {name, select(i,...) for i,name in ipairs(arg_names)}
        if @debug
            @write "#{colored.bright "CALLING"} #{colored.magenta(colored.underscore stub)} "
            @writeln "#{colored.bright "WITH ARGS:"} #{colored.dim repr(args)}"
        old_defs, @defs = @defs, def.defs
        rets = {thunk(self,args)}
        @defs = old_defs
        remove @callstack
        return unpack(rets)
    
    run_macro: (tree)=>
        args = [arg for arg in *tree.value when arg.type != "Word"]
        if @debug
            @write "#{colored.bright "RUNNING MACRO"} #{colored.underscore colored.magenta(tree.stub)} "
            @writeln "#{colored.bright "WITH ARGS:"} #{colored.dim repr args}"
        insert @callstack, "#macro"
        expr, statement = @call(tree.stub, tree.line_no, unpack(args))
        remove @callstack
        return expr, statement

    dedent: (code)=>
        unless code\find("\n")
            return code
        spaces, indent_spaces = math.huge, math.huge
        for line in code\gmatch("\n([^\n]*)")
            if line\match("^%s*#.*")
                continue
            elseif s = line\match("^(%s*)%.%..*")
                spaces = math.min(spaces, #s)
            elseif s = line\match("^(%s*)%S.*")
                indent_spaces = math.min(indent_spaces, #s)
        if spaces != math.huge and spaces < indent_spaces
            return (code\gsub("\n"..(" ")\rep(spaces), "\n"))
        else
            return (code\gsub("\n"..(" ")\rep(indent_spaces), "\n    "))

    indent: (code)=>
        (code\gsub("\n","\n    "))

    assert_permission: (stub)=>
        fn_def = @defs[stub]
        unless fn_def
            @error "Undefined function: #{fn_name}"
        whiteset = fn_def.whiteset
        if whiteset == nil then return true
        -- TODO: maybe optimize this by making the callstack a Counter and using a 
        --    move-to-front optimization on the whitelist to check most likely candidates sooner
        for caller in *@callstack
            if caller != "#macro" and whiteset[caller[1]] then return true
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
            if caller != "#macro" and whiteset[caller[1]] then return true
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

    run: (src, filename, vars={}, max_operations=nil, output_file=nil)=>
        if src == "" then return nil, "", vars
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
            ok,expr,statements = pcall(@tree_to_lua, self, statement, filename)
            if not ok
                @errorln "#{colored.red "Error occurred in statement:"}\n#{colored.bright colored.yellow statement.src}"
                error(expr)
            code_for_statement = ([[
return (function(nomsu, vars)
%s
return %s;
end);]])\format(statements or "", expr or "ret")
            if output_file
                if statements and #statements > 0
                    output_file\write "lua> \"..\"\n    #{@indent statements\gsub("\\","\\\\")}\n"
                if expr and #expr > 0
                    output_file\write "=lua \"..\"\n    #{@indent expr\gsub("\\","\\\\")}\n"
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
                error(ret)
            if statements
                insert buffer, statements
            if expr
                insert buffer, "ret = #{expr};"
        
        if max_operations
            debug.sethook!
        lua_code = ([[
return (function(nomsu, vars)
local ret;
%s
return ret;
end);]])\format(concat(buffer, "\n"))
        return return_value, lua_code, vars
    
    tree_to_value: (tree, vars, filename)=>
        code = "return (function(nomsu, vars)\nreturn #{@tree_to_lua(tree, filename)};\nend);"
        if @debug
            @writeln "#{colored.bright "RUNNING LUA TO GET VALUE:"}\n#{colored.blue colored.bright(code)}"
        lua_thunk, err = load(code)
        if not lua_thunk
            @error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack code}\n\n#{colored.red err}")
        return (lua_thunk!)(self, vars or {})

    tree_to_nomsu: (tree, force_inline=false)=>
        -- Return <nomsu code>, <is safe for inline use>
        assert tree, "No tree provided."
        if not tree.type
            @errorln debug.traceback()
            @error "Invalid tree: #{repr(tree)}"
        switch tree.type
            when "File"
                return concat([@tree_to_nomsu(v, force_inline) for v in *tree.value], "\n"), false
            
            when "Nomsu"
                inside, inline = @tree_to_nomsu(tree.value, force_inline)
                return "\\#{inside}", inline

            when "Thunk"
                if force_inline
                    return "{#{concat([@tree_to_nomsu(v, true) for v in *tree.value], "; ")}", true
                else
                    return ":"..@indent("\n"..concat([@tree_to_nomsu v for v in *tree.value], "\n")), false

            when "FunctionCall"
                buff = ""
                sep = ""
                inline = true
                line_len = 0
                for arg in *tree.value
                    nomsu, arg_inline = @tree_to_nomsu(arg, force_inline)
                    if sep == " " and line_len + #nomsu > 80
                        sep = "\n.."
                    unless sep == " " and not arg_inline and nomsu\sub(1,1) == ":"
                        buff ..= sep
                    if arg_inline
                        sep = " "
                        line_len += 1 + #nomsu
                    else
                        line_len = 0
                        inline = false
                        sep = "\n.."
                    if arg.type == 'FunctionCall'
                        if arg_inline
                            buff ..= "(#{nomsu})"
                        else
                            buff ..= "(..)\n    #{@indent nomsu}"
                    else
                        buff ..= nomsu
                return buff, inline

            when "String"
                buff = "\""
                longbuff = "\"..\"\n    |"
                inline = true
                for bit in *tree.value
                    if type(bit) == "string"
                        bit = bit\gsub("\\","\\\\")
                        buff ..= bit\gsub("\n","\\n")\gsub("\"","\\\"")
                        longbuff ..= bit\gsub("\n","\n    |")
                    else
                        inside, bit_inline = @tree_to_nomsu(bit, force_inline)
                        inline and= bit_inline
                        buff ..= "\\(#{inside})"
                        longbuff ..= "\\(#{inside})"
                buff ..= "\""
                if force_inline or (inline and #buff <= 90)
                    return buff, true
                else
                    return longbuff, false

            when "List"
                buff = "["
                longbuff = "[..]\n    "
                longsep = ""
                longline = 0
                inline = true
                for i,bit in ipairs tree.value
                    nomsu, bit_inline = @tree_to_nomsu(bit, force_inline)
                    inline and= bit_inline
                    if inline
                        if i > 1
                            buff ..= ", "
                        buff ..= nomsu
                    longbuff ..= longsep .. nomsu
                    longline += #nomsu
                    longsep = if bit_inline and longline <= 90
                        ", "
                    else "\n    "
                buff ..= "]"
                if force_inline or (inline and #buff <= 90)
                    return buff, true
                else
                    return longbuff, false

            when "Number"
                return repr(tree.value), true

            when "Var"
                return "%#{tree.value}", true

            when "Word"
                return tree.value, true

            else
                @error("Unknown/unimplemented thingy: #{tree.type}")

    value_to_nomsu: (value)=>
        switch type(value)
            when "nil"
                return "(nil)"
            when "bool"
                return value and "(yes)" or "(no)"
            when "number"
                return repr(value)
            when "table"
                if is_list(value)
                    return "[#{concat [@value_to_nomsu(v) for v in *value], ", "}]"
                else
                    return "(d{#{concat ["#{@value_to_nomsu(k)}=#{@value_to_nomsu(v)}" for k,v in pairs(value)], "; "}})"
            when "string"
                if value == "\n"
                    return "'\\n'"
                elseif not value\find[["]] and not value\find"\n" and not value\find"\\"
                    return "\""..value.."\""
                else
                    -- TODO: This might fail if it's being put inside a list or something
                    return '".."\n    '..(@indent value)
            else
                error("Unsupported value_to_nomsu type: #{type(value)}")

    tree_to_lua: (tree, filename)=>
        -- Return <lua code for value>, <additional lua code>
        assert tree, "No tree provided."
        if not tree.type
            @errorln debug.traceback()
            @error "Invalid tree: #{repr(tree)}"
        switch tree.type
            when "File"
                lua_bits = {}
                for line in *tree.value
                    expr,statement = @tree_to_lua line, filename
                    if statement then insert lua_bits, statement
                    if expr then insert lua_bits, "ret = #{expr};"
                return nil, concat(lua_bits, "\n")
            
            when "Nomsu"
                return "nomsu:parse(#{repr tree.value.src}, #{repr tree.line_no}).value[1]", nil

            when "Thunk"
                lua_bits = {}
                for arg in *tree.value
                    expr,statement = @tree_to_lua arg, filename
                    if statement then insert lua_bits, statement
                    if expr then insert lua_bits, "ret = #{expr};"
                return ([[
(function(nomsu, vars)
local ret;
%s
return ret;
end)]])\format(concat(lua_bits, "\n"))

            when "FunctionCall"
                insert @compilestack, tree

                def = @defs[tree.stub]
                if def and def.is_macro
                    expr, statement = @run_macro(tree)
                    if def.whiteset
                        if expr
                            expr = "(nomsu:assert_permission(#{repr tree.stub}) and #{expr})"
                        if statement
                            statement = "nomsu:assert_permission(#{repr tree.stub});\n"..statement
                    remove @compilestack
                    return expr, statement
                args = {repr(tree.stub), repr(tree.line_no)}
                local arg_names, escaped_args
                if def
                    arg_names, escaped_args = def.arg_names, def.escaped_args
                else
                    arg_names, escaped_args = [w.value for w in *tree.value when w.type == "Word"], {}
                arg_num = 1
                for arg in *tree.value
                    if arg.type == 'Word' then continue
                    if escaped_args[arg_names[arg_num]]
                        arg = {type:"Nomsu", value:arg, line_no:tree.line_no}
                    expr,statement = @tree_to_lua arg, filename
                    if statement
                        @error "Cannot use [[#{arg.src}]] as a function argument, since it's not an expression."
                    insert args, expr
                    arg_num += 1

                remove @compilestack
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
                    expr, statement = @tree_to_lua bit, filename
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
                    expr,statement = @tree_to_lua item, filename
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
        -- Returns a single stub ("say %"), list of arg names ({"msg"}), and set of arg
        -- names that should not be evaluated from a single rule def
        --   (e.g. "say %msg") or function call (e.g. FunctionCall({Word("say"), Var("msg")))
        if type(x) == 'string'
            -- Standardize format to stuff separated by spaces
            x = x\gsub("\n%s*%.%.", " ")
            x = lpeg.Cs((operator / ((op)->" #{op} ") + 1)^0)\match(x)
            x = x\gsub("%s+"," ")\gsub("^%s*","")\gsub("%s*$","")
            stub = x\gsub("%%%S+","%%")\gsub("\\","")
            arg_names = [arg for arg in x\gmatch("%%([^%s]*)")]
            escaped_args = set [arg for arg in x\gmatch("\\%%([^%s]*)")]
            return stub, arg_names, escaped_args
        if type(x) != 'table'
            @error "Invalid type for getting stub: #{type(x)} for:\n#{repr x}"
        switch x.type
            when "String" then return @get_stub(x.value)
            when "FunctionCall" then return @get_stub(x.src)
            else @error "Unsupported get stub type: #{x.type} for #{repr x}"
    
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
        error_msg = colored.red "ERROR!"
        if msg
            error_msg ..= "\n" .. (colored.bright colored.yellow colored.onred msg)
        error_msg ..= "\nCallstack:"
        maxlen = max([#c[2] for c in *@callstack when c != "#macro"])
        for i=#@callstack,1,-1
            if @callstack[i] != "#macro"
                line_no = @callstack[i][2]
                if line_no
                    nums = [tonumber(n) for n in line_no\gmatch(":([0-9]+)")]
                    line_no = line_no\gsub(":.*$", ":#{sum(nums) - #nums + 1}")
                error_msg ..= "\n    #{"%-#{maxlen}s"\format line_no}| #{@callstack[i][1]}"
        error_msg ..= "\n    <top level>"
        @callstack = {}
        error error_msg, 3
    
    typecheck: (vars, varname, desired_type)=>
        x = vars[varname]
        if type(x) == desired_type then return x
        if type(x) == 'table' and x.type == desired_type then return x
        @error "Invalid type for %#{varname}. Expected #{desired_type}, but got #{repr x}."
    
    source_code: (level=0)=>
        @dedent @compilestack[#@compilestack-level].src

    initialize_core: =>
        -- Sets up some core functionality
        nomsu_string_as_lua = (code)=>
            concat_parts = {}
            for bit in *code.value
                if type(bit) == "string"
                    insert concat_parts, bit
                else
                    expr, statement = @tree_to_lua bit, filename
                    if statement
                        @error "Cannot use [[#{bit.src}]] as a string interpolation value, since it's not an expression."
                    insert concat_parts, expr
            return concat(concat_parts)
        
        -- Uses named local functions to help out callstack readability
        lua_code = (vars)=>
            lua = nomsu_string_as_lua(@, vars.code)
            return nil, lua
        @defmacro "lua> %code", lua_code

        lua_value = (vars)=>
            lua = nomsu_string_as_lua(@, vars.code)
            return lua, nil
        @defmacro "=lua %code", lua_value

        @defmacro "__src__ %level", (vars)=>
            @repr @source_code @tree_to_value vars.level

        run_file = (vars)=>
            if vars.filename\match(".*%.lua")
                return dofile(vars.filename)(@, vars)
            if vars.filename\match(".*%.nom")
                if not @skip_precompiled -- Look for precompiled version
                    file = io.open(vars.filename\gsub("%.nom", ".compiled.nom"), "r")
                file = file or io.open(vars.filename)
                if not file
                    @error "File does not exist: #{vars.filename}"
                contents = file\read('*a')
                file\close!
                return @run(contents, vars.filename)
            else
                @error "Invalid filetype for #{vars.filename}"
        @def "run file %filename", run_file

        _require = (vars)=>
            loaded = @defs["#loaded_files"]
            if not loaded[vars.filename]
                loaded[vars.filename] = run_file(self, {filename:vars.filename}) or true
            return loaded[vars.filename]
        @def "require %filename", _require


if arg
    export colors
    colors = require 'consolecolors'
    parser = re.compile([[
        args <- {| {:flags: flags? :} ({:input: input :} ";" ("-o;"{:output: output :} ";")?)? (";")? |} !.
        flags <- (({| ({flag} ";")* |}) -> set)
        flag <- "-c" / "-i" / "-p" / "-O" / "--help" / "-h"
        input <- "-" / [^;]+
        output <- "-" / [^;]+
    ]], {:set})
    args = concat(arg, ";")..";"
    args = parser\match(args) or {}
    if not args or not args.flags or args.flags["--help"] or args.flags["-h"]
        print "Usage: lua nomsu.lua [-c] [-i] [-p] [-O] [--help] [input [-o output]]"
        os.exit!

    c = NomsuCompiler()
    c.skip_precompiled = not args.flags["-O"]
    if args.input
        -- Read a file or stdin and output either the printouts or the compiled lua
        if args.flags["-c"] and not args.output
            args.output = args.input\gsub("%.nom", ".compiled.nom")
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
            vars = {}
            retval, code = c\run(input, args.input, vars, nil, compiled_output)
        if args.flags["-p"]
            c.write = _write

    if args.flags["-i"]
        -- REPL
        vars = {}
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
            ok, ret = pcall(-> c\run(buff, "stdin", vars))
            if ok and ret != nil
                print "= "..repr(ret)

return NomsuCompiler
