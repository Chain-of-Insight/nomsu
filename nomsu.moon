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
colored = setmetatable({}, {__index:(_,color)-> ((msg)-> colors[color]..(msg or '')..colors.reset)})
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
DIGIT, HEX = R('09'), R('09','af','AF')
ESCAPE_CHAR = (P("\\")*S("xX")*C(HEX*HEX)) / => string.char(tonumber(@, 16))
ESCAPE_CHAR += (P("\\")*C(DIGIT*(DIGIT^-2))) / => string.char(tonumber @)
ESCAPE_CHAR += (P("\\")*C(S("ntbavfr"))) / STRING_ESCAPES
OPERATOR_CHAR = S("'~`!@$^&*-+=|<>?/")
UTF8_CHAR = (
    R("\194\223")*R("\128\191") +
    R("\224\239")*R("\128\191")*R("\128\191") +
    R("\240\244")*R("\128\191")*R("\128\191")*R("\128\191"))
IDENT_CHAR = R("az","AZ","09") + P("_") + UTF8_CHAR

local parse
do
    export parse
    ctx = {}
    indent_patt = P (start)=>
        spaces = @match("[ \t]*", start)
        if #spaces > ctx.indent_stack[#ctx.indent_stack]
            insert(ctx.indent_stack, #spaces)
            return start + #spaces
    dedent_patt = P (start)=>
        spaces = @match("[ \t]*", start)
        if #spaces < ctx.indent_stack[#ctx.indent_stack]
            remove(ctx.indent_stack)
            return start
    nodent_patt = P (start)=>
        spaces = @match("[ \t]*", start)
        if #spaces == ctx.indent_stack[#ctx.indent_stack]
            return start + #spaces
    gt_nodent_patt = P (start)=>
        -- Note! This assumes indent is 4 spaces!!!
        spaces = @match("[ \t]*", start)
        if #spaces >= ctx.indent_stack[#ctx.indent_stack] + 4
            return start + ctx.indent_stack[#ctx.indent_stack] + 4

    defs =
        nl: P("\n"), ws: S(" \t"), :tonumber, operator: OPERATOR_CHAR
        print: (src,pos,msg)-> print(msg, pos, repr(src\sub(math.max(0,pos-16),math.max(0,pos-1)).."|"..src\sub(pos,pos+16))) or true
        utf8_char: (
            R("\194\223")*R("\128\191") +
            R("\224\239")*R("\128\191")*R("\128\191") +
            R("\240\244")*R("\128\191")*R("\128\191")*R("\128\191"))
        indented: indent_patt, nodented: nodent_patt, dedented: dedent_patt
        gt_nodented: gt_nodent_patt, escape_char:ESCAPE_CHAR
        error: (src,pos,err_msg)->
            if ctx.source_code\sub(pos,pos) == "\n"
                pos += #ctx.source_code\match("[ \t\n]*", pos)
            line_no = 1
            while (ctx.line_starts[line_no+1] or math.huge) < pos do line_no += 1
            prev_line = line_no > 1 and ctx.source_code\match("[^\n]*", ctx.line_starts[line_no-1]) or ""
            err_line = ctx.source_code\match("[^\n]*", ctx.line_starts[line_no])
            next_line = line_no < #ctx.line_starts and ctx.source_code\match("[^\n]*", ctx.line_starts[line_no+1]) or ""
            pointer = ("-")\rep(pos-ctx.line_starts[line_no]) .. "^"
            err_msg = (err_msg or "Parse error").." in #{ctx.filename} on line #{line_no}:\n"
            err_msg ..="\n#{prev_line}\n#{err_line}\n#{pointer}\n#{next_line}\n"
            error(err_msg)
        FunctionCall: (start, value, stop)->
            stub = concat([(t.type == "Word" and t.value or "%") for t in *value], " ")
            line_no = 1
            while (ctx.line_starts[line_no+1] or math.huge) < start do line_no += 1
            src = ctx.source_code\sub(start,stop-1)
            return {type: "FunctionCall", :src, line_no: "#{ctx.filename}:#{line_no}", :value, :stub}

    setmetatable(defs, {__index:(key)=>
        fn = (start, value, stop)->
            {:start, :stop, :value, src:ctx.source_code\sub(start,stop-1), type: key}
        self[key] = fn
        return fn
    })

    -- Just for cleanliness, I put the language spec in its own file using a slightly modified
    -- version of the lpeg.re syntax.
    peg_tidier = re.compile [[
    file <- {~ %nl* (def/comment) (%nl+ (def/comment))* %nl* ~}
    def <- anon_def / captured_def
    anon_def <- ({ident} (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- %2"
    captured_def <- ({ident} (" "*) "(" {ident} ")" (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- ({} %3 {}) -> %2"
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    comment <- "--" [^%nl]*
    ]]

    nomsu = peg_tidier\match(io.open("nomsu.peg")\read("*a"))
    nomsu = re.compile(nomsu, defs)

    parse = (source_code, filename)->
        old_ctx = ctx
        export ctx
        ctx = {:source_code, :filename, indent_stack: {0}}
        ctx.line_starts = re.compile("lines <- {| line ('\n' line)* |} line <- {} [^\n]*")\match(source_code)
        tree = nomsu\match(source_code)
        ctx = old_ctx
        return tree

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
            @writeln("#{colored.bright "PARSING:"}\n#{colored.yellow str}")
        str = str\gsub("\r","")
        tree = parse(str, filename)
        assert tree, "In file #{colored.blue filename} failed to parse:\n#{colored.onyellow colored.black str}"
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

            when "Dict"
                error("Sorry, not yet implemented.")

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
                        insert args, "nomsu:parse(#{repr arg.src}, #{repr tree.line_no}).value[1]"
                    else
                        expr,statement = @tree_to_lua arg, filename
                        if statement
                            @error "Cannot use [[#{arg.src}]] as a function argument, since it's not an expression."
                        insert args, expr
                    arg_num += 1

                remove @compilestack
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

            when "Dict"
                items = {}
                for entry in *tree.value
                    local key_expr,key_statement
                    if entry.dict_key.type == "Word"
                        key_expr,key_statement = repr(entry.dict_key.value),nil
                    else
                        key_expr,key_statement = @tree_to_lua entry.dict_key, filename
                    if key_statement
                        @error "Cannot use [[#{entry.dict_key.src}]] as a dict key, since it's not an expression."
                    value_expr,value_statement = @tree_to_lua entry.dict_value, filename
                    if value_statement
                        @error "Cannot use [[#{entry.dict_value.src}]] as a dict value, since it's not an expression."
                    key_str = key_expr\match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
                    if key_str
                        insert items, "#{key_str}=#{value_expr}"
                    else
                        insert items, "[#{key_expr}]=#{value_expr}"
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
            when "Dict"
                for e in *tree.value
                    @walk_tree(e.dict_key, depth+1)
                    @walk_tree(e.dict_value, depth+1)
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
        Cs(((P("\\\\")/"\\") + (P("\\\"")/'"') + ESCAPE_CHAR + P(1))^0)\match(str)

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
            when "Dict"
                dirty = false
                replacements = {}
                for i,e in ipairs tree.value
                    new_key = @replaced_vars e.dict_key, vars
                    new_value = @replaced_vars e.dict_value, vars
                    dirty or= new_key != e.dict_key or new_value != e.dict_value
                    replacements[i] = {dict_key:new_key, dict_value:new_value}
                if dirty
                    tree = {k,v for k,v in pairs(tree)}
                    tree.value = replacements
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
            patt = re.compile "{|(' '+ / '\n..' / {'\\'? '%' %id*} / {%id+} / {%op+})*|}",
                id:IDENT_CHAR, op:OPERATOR_CHAR
            spec = concat patt\match(x), " "
            stub = spec\gsub("%%%S+","%%")\gsub("\\","")
            arg_names = [arg for arg in spec\gmatch("%%([^%s]*)")]
            escaped_args = set [arg for arg in spec\gmatch("\\%%(%S*)")]
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
        x_type = type(x)
        if x_type == desired_type then return x
        if x_type == 'table'
            x_type = x.type or x_type
            if x_type == desired_type then return x
        @error "Invalid type for %#{varname}. Expected #{desired_type}, but got #{x_type}:\n#{repr x}"
    
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

        @def "derp \\%foo derp \\%bar", (vars)=>
            lua = "local x = "..repr([t.stub for t in *vars.foo.value])..";\nlocal y = "..@tree_to_lua(vars.bar)
            print(colored.green lua)

        run_file = (vars)=>
            if vars.filename\match(".*%.lua")
                return dofile(vars.filename)(@, vars)
            if vars.filename\match(".*%.nom")
                if not @skip_precompiled -- Look for precompiled version
                    file = io.open(vars.filename\gsub("%.nom", ".nomc"), "r")
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
            args.output = args.input\gsub("%.nom", ".nomc")
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
