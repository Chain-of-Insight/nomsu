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
new_uuid = require 'uuid'
{:repr, :stringify, :min, :max, :equivalent, :set, :is_list, :sum} = utils
colors = setmetatable({}, {__index:->""})
colored = setmetatable({}, {__index:(_,color)-> ((msg)-> colors[color]..(msg or '')..colors.reset)})
{:insert, :remove, :concat} = table

-- TODO:
-- consider non-linear codegen, rather than doing thunks for things like comprehensions
-- improve indentation of generated lua code
-- better scoping?
-- better error reporting
-- type checking?
-- Fix compiler bug that breaks when file ends with a block comment
-- Add compiler options for optimization level (compile-fast vs. run-fast, etc.)
-- Do a pass on all actions to enforce parameters-are-nouns heuristic
-- Maybe do some sort of lazy definitions of actions that defer until they're used in code
-- Do automatic "local" detection of new variables and declare them as locals like moonscript does

lpeg.setmaxstack 10000 -- whoa
{:P,:R,:V,:S,:Cg,:C,:Cp,:B,:Cmt} = lpeg

NOMSU_DEFS = with {}
    .nl = P("\n")
    .ws = S(" \t")
    .tonumber = tonumber
    .print = (src,pos,msg)->
        print(msg, pos, repr(src\sub(math.max(0,pos-16),math.max(0,pos-1)).."|"..src\sub(pos,pos+16)))
        return true
    string_escapes = n:"\n", t:"\t", b:"\b", a:"\a", v:"\v", f:"\f", r:"\r"
    digit, hex = R('09'), R('09','af','AF')
    .escaped_char = (P("\\")*S("xX")*C(hex*hex)) / => string.char(tonumber(@, 16))
    .escaped_char += (P("\\")*C(digit*(digit^-2))) / => string.char(tonumber @)
    .escaped_char += (P("\\")*C(S("ntbavfr"))) / string_escapes
    .operator_char = S("'~`!@$^&*-+=|<>?/")
    .operator = .operator_char^1
    .utf8_char = (
        R("\194\223")*R("\128\191") +
        R("\224\239")*R("\128\191")*R("\128\191") +
        R("\240\244")*R("\128\191")*R("\128\191")*R("\128\191"))
    .ident_char = R("az","AZ","09") + P("_") + .utf8_char

    -- If the number of leading space characters is greater than number in the top of the
    -- stack, this pattern matches and pushes the number onto the stack.
    .indent = P (start)=>
        spaces = @match("[ \t]*", start)
        if #spaces > lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack]
            insert(lpeg.userdata.indent_stack, #spaces)
            return start + #spaces
    -- If the number of leading space characters is less than number in the top of the
    -- stack, this pattern matches and pops off the top of the stack exactly once.
    .dedent = P (start)=>
        spaces = @match("[ \t]*", start)
        if #spaces < lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack]
            remove(lpeg.userdata.indent_stack)
            return start
    -- If the number of leading space characters is equal to the number on the top of the
    -- stack, this pattern matches and does not modify the stack.
    .nodent = P (start)=>
        spaces = @match("[ \t]*", start)
        if #spaces == lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack]
            return start + #spaces
    -- If the number of leading space characters is 4+ more than the number on the top of the
    -- stack, this pattern matches the first n+4 spaces and does not modify the stack.
    .gt_nodent = P (start)=>
        -- Note! This assumes indent is exactly 4 spaces!!!
        spaces = @match("[ \t]*", start)
        if #spaces >= lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack] + 4
            return start + lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack] + 4

    .error = (src,pos,err_msg)->
        if lpeg.userdata.source_code\sub(pos,pos) == "\n"
            pos += #lpeg.userdata.source_code\match("[ \t\n]*", pos)
        line_no = 1
        while (lpeg.userdata.line_starts[line_no+1] or math.huge) < pos do line_no += 1
        prev_line = if line_no > 1
            lpeg.userdata.source_code\match("[^\n]*", lpeg.userdata.line_starts[line_no-1])
        else ""
        err_line = lpeg.userdata.source_code\match("[^\n]*", lpeg.userdata.line_starts[line_no])
        next_line = if line_no < #lpeg.userdata.line_starts
            lpeg.userdata.source_code\match("[^\n]*", lpeg.userdata.line_starts[line_no+1])
        else ""
        pointer = ("-")\rep(pos-lpeg.userdata.line_starts[line_no]) .. "^"
        err_msg = (err_msg or "Parse error").." in #{lpeg.userdata.filename} on line #{line_no}:\n"
        err_msg ..="\n#{prev_line}\n#{err_line}\n#{pointer}\n#{next_line}\n"
        error(err_msg)

    .FunctionCall = (start, value, stop)->
        stub = concat([(t.type == "Word" and t.value or "%") for t in *value], " ")
        src = lpeg.userdata.source_code\sub(start,stop-1)
        return {:start, :stop, type: "FunctionCall", :src, get_line_no:lpeg.userdata.get_line_no, :value, :stub}

setmetatable(NOMSU_DEFS, {__index:(key)=>
    make_node = (start, value, stop)->
        {:start, :stop, :value, src:lpeg.userdata.source_code\sub(start,stop-1), get_line_no:lpeg.userdata.get_line_no, type: key}
    self[key] = make_node
    return make_node
})

NOMSU = do
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
    nomsu_peg = peg_tidier\match(io.open("nomsu.peg")\read("*a"))
    re.compile(nomsu_peg, NOMSU_DEFS)

class NomsuCompiler
    @def_number: 0
    new:()=>
        @write = (...)=> io.write(...)
        @write_err = (...)=> io.stderr\write(...)
        -- Weak-key mapping from objects to randomly generated unique IDs
        @ids = setmetatable({}, {
            __mode: "k"
            __index: (key)=>
                id = new_uuid!
                @[key] = id
                return id
        })
        @compilestack = {}
        @debug = false

        @environment = {
            -- Discretionary/convenience stuff
            nomsu:self, repr:repr, stringify:stringify, utils:utils, lpeg:lpeg, re:re,
            -- Lua stuff:
            :next, :unpack, :setmetatable, :coroutine, :rawequal, :getmetatable, :pcall,
            :error, :package, :os, :require, :tonumber, :tostring, :string, :xpcall, :module,
            :print, :loadfile, :rawset, :_VERSION, :collectgarbage, :rawget, :bit32, :rawlen,
            :table, :assert, :dofile, :loadstring, :type, :select, :debug, :math, :io, :pairs,
            :load, :ipairs,
        }
        @environment.ACTIONS = setmetatable({}, {__index:(key)=>
            error("Attempt to run undefined action: #{key}", 0)
        })
        @action_metadata = setmetatable({}, {__mode:"k"})
        @environment.ACTION_METADATA = @action_metadata
        @environment.LOADED = {}
        @initialize_core!
    
    writeln:(...)=>
        @write(...)
        @write("\n")
    
    errorln:(...)=>
        @write_err(...)
        @write_err("\n")
    
    define_action: (signature, line_no, fn, src, compile_time=false)=>
        if type(signature) == 'string'
            signature = @get_stubs {signature}
        elseif type(signature) == 'table' and type(signature[1]) == 'string'
            signature = @get_stubs signature
        assert type(fn) == 'function', "Bad fn: #{repr fn}"
        aliases = {}
        @@def_number += 1

        fn_info = debug.getinfo(fn, "u")
        local fn_arg_positions, arg_orders
        unless fn_info.isvararg
            fn_arg_positions = {debug.getlocal(fn, i), i for i=1,fn_info.nparams}
            arg_orders = {} -- Map from stub -> index where each arg in the stub goes in the function call
        for sig_i=1,#signature
            stub, arg_names = unpack(signature[sig_i])
            assert stub, "NO STUB FOUND: #{repr signature}"
            if @debug
                @writeln "#{colored.bright "DEFINING ACTION:"} #{colored.underscore colored.magenta repr(stub)} #{colored.bright "WITH ARGS"} #{colored.dim repr(arg_names)} ON: #{@environment.ACTIONS}"
            -- TODO: use debug.getupvalue instead of @environment.ACTIONS?
            @environment.ACTIONS[stub] = fn
            unless fn_info.isvararg
                arg_positions = [fn_arg_positions[@var_to_lua_identifier(a)] for a in *arg_names]
                -- TODO: better error checking?
                assert(#arg_positions == #arg_names,
                    "Mismatch in args between lua function's #{repr fn_arg_positions} and stub's #{repr arg_names}")
                arg_orders[stub] = arg_positions
        
        @action_metadata[fn] = {
            :fn, :src, :line_no, :aliases, :arg_orders, arg_positions:fn_arg_positions, def_number:@@def_number,
        }

    define_compile_action: (signature, line_no, fn, src)=>
        @define_action(signature, line_no, fn, src, true)
        @action_metadata[fn].compile_time = true
        if @debug
            @writeln "#{colored.bright colored.green "(it was compile time)"}"

    serialize_defs: (scope=nil, after=nil)=>
        -- TODO: repair
        error("Not currently functional.")
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

    indent: (code, levels=1)=>
        return code\gsub("\n","\n"..("    ")\rep(levels))

    parse: (nomsu_code, filename)=>
        assert type(filename) == "string", "Bad filename type: #{type filename}"
        if @debug
            @writeln("#{colored.bright "PARSING:"}\n#{colored.yellow nomsu_code}")
        nomsu_code = nomsu_code\gsub("\r","")

        userdata = with {source_code:nomsu_code, :filename, indent_stack: {0}}
            .line_starts = re.compile("lines <- {| line ('\n' line)* |} line <- {} [^\n]*")\match(nomsu_code)
            .get_line_no = =>
                unless @_line_no
                    line_no = 1
                    while (.line_starts[line_no+1] or math.huge) < @start do line_no += 1
                    @_line_no = "#{.filename}:#{line_no}"
                return @_line_no

        old_userdata, lpeg.userdata = lpeg.userdata, userdata
        tree = NOMSU\match(nomsu_code)
        lpeg.userdata = old_userdata
        
        assert tree, "In file #{colored.blue filename} failed to parse:\n#{colored.onyellow colored.black nomsu_code}"
        if @debug
            @writeln "PARSE TREE:"
            @print_tree tree, "    "
        return tree

    run: (src, filename, max_operations=nil, output_file=nil)=>
        if src == "" then return nil, ""
        if max_operations
            timeout = ->
                debug.sethook!
                error "Execution quota exceeded. Your code took too long."
            debug.sethook timeout, "", max_operations
        tree = @parse(src, filename)
        assert tree, "Failed to parse: #{src}"
        assert tree.type == "File", "Attempt to run non-file: #{tree.type}"

        lua = @tree_to_lua(tree)
        lua_code = lua.statements or (lua.expr..";")
        lua_code = "-- File: #{filename}\n"..lua_code
        ret = @run_lua(lua_code)
        if max_operations
            debug.sethook!
        if output_file
            output_file\write(lua_code)
        return ret, lua_code

    run_file: (filename)=>
        if filename\match(".*%.lua")
            file = io.open(filename)
            contents = file\read("*a")
            file\close!
            return assert(load(contents, nil, nil, @environment))!
        if filename\match(".*%.nom")
            if not @skip_precompiled -- Look for precompiled version
                file = io.open(filename\gsub("%.nom", ".lua"), "r")
                if file
                    lua_code = file\read("*a")
                    file\close!
                    return @run_lua(lua_code)
            file = file or io.open(filename)
            if not file
                error "File does not exist: #{filename}"
            nomsu_code = file\read('*a')
            file\close!
            return @run(nomsu_code, filename)
        else
            error "Invalid filetype for #{filename}"
    
    require_file: (filename)=>
        loaded = @environment.LOADED
        if not loaded[filename]
            loaded[filename] = @run_file(filename) or true
        return loaded[filename]

    run_lua: (lua_code)=>
        run_lua_fn, err = load(lua_code, nil, nil, @environment)
        if @debug
            @writeln "#{colored.bright "RUNNING LUA:"}\n#{colored.blue colored.bright(lua_code)}"
        if not run_lua_fn
            n = 1
            fn = ->
                n = n + 1
                ("\n%-3d|")\format(n)
            code = "1  |"..lua_code\gsub("\n", fn)
            error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack code}\n\n#{err}")
        return run_lua_fn!
    
    tree_to_value: (tree, filename)=>
        code = "return #{@tree_to_lua(tree).expr};"
        if @debug
            @writeln "#{colored.bright "RUNNING LUA TO GET VALUE:"}\n#{colored.blue colored.bright(code)}"
        lua_thunk, err = load(code, nil, nil, @environment)
        if not lua_thunk
            error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack code}\n\n#{colored.red err}")
        return lua_thunk!

    tree_to_nomsu: (tree, indentation="", max_line=80, expr_type=nil)=>
        -- Convert a tree into nomsu code that satisfies the max line requirement or nil
        -- if that's not possible
        -- expr_type is either:
        --   nil for code that goes at the top level and can contain anything
        --   "noeol" for code that can contain anything except an end-of-line component
        --       like a colon (i.e. it already occurs after a colon on the same line)
        --   "inline" for code that cannot contain indented code or an end-of-line component
        --       e.g. code that is meant to go inside parentheses
        assert tree, "No tree provided to tree_to_nomsu."
        assert tree.type, "Invalid tree: #{repr(tree)}"
        join_lines = (lines)->
            for line in *lines
                if #indentation + #line > max_line
                    return nil
            return concat(lines, "\n"..indentation)

        is_operator = (tok)-> tok and tok.type == "Word" and NOMSU_DEFS.operator\match(tok.value)

        local inline_expression, noeol_expression, expression
        inline_expression = (tok)->
            switch tok.type
                when "Block"
                    if #tok.value > 1 then return nil
                    nomsu = inline_expression tok.value
                    return nomsu and "(: #{nomsu})"
                when "FunctionCall"
                    buff = ""
                    for i,bit in ipairs tok.value
                        if bit.type == "Word"
                            if i == 1 or (is_operator(bit) and is_operator(tok.value[i-1]))
                                buff ..= bit.value
                            else buff ..= " "..bit.value
                        else
                            nomsu = inline_expression bit
                            return nil unless nomsu
                            unless i == 1 or bit.type == "Block"
                                buff ..= " "
                            buff ..= if bit.type == "FunctionCall"
                                "("..nomsu..")"
                            else nomsu
                    return buff
                when "List"
                    bits = {}
                    for bit in *tok.value
                        nomsu = inline_expression bit
                        return nil unless nomsu
                        insert bits, nomsu
                    return "["..concat(bits, ", ").."]"
                when "Dict"
                    bits = {}
                    for bit in *tok.value
                        key_nomsu = if bit.dict_key.type == "Word"
                            bit.dict_key.value
                        else inline_expression bit.dict_key
                        return nil unless key_nomsu
                        if bit.dict_key.type == "FunctionCall"
                            key_nomsu = "("..key_nomsu..")"
                        value_nomsu = inline_expression bit.dict_value
                        return nil unless value_nomsu
                        insert bits, key_nomsu.."="..value_nomsu
                    return "{"..concat(bits, ", ").."}"
                when "Text"
                    buff = '"'
                    for bit in *tok.value
                        if type(bit) == 'string'
                            -- Force indented text
                            return nil if bit\find("\n")
                            buff ..= bit\gsub("\\","\\\\")\gsub("\n","\\n")
                        else
                            nomsu = inline_expression(bit)
                            return nil unless nomsu
                            buff ..= if bit.type == "Var" or bit.type == "List" or bit.type == "Dict"
                                "\\"..nomsu
                            else "\\("..nomsu..")"
                        if #buff > max_line then return nil
                    return buff..'"'
                when "Nomsu"
                    nomsu = inline_expression(tok.value)
                    return nil if not nomsu
                    return "\\("..nomsu..")"
                when "Number" then tostring(tok.value)
                when "Var" then "%"..tok.value
                else return nil

        noeol_expression = (tok)->
            nomsu = inline_expression(tok)
            if nomsu and #nomsu < max_line
                return nomsu
            switch tok.type
                when "Block"
                    buff = ":"
                    for line in *tok.value
                        nomsu = expression(line)
                        return nil unless nomsu
                        buff ..= "\n    "..@indent(nomsu)
                    return buff
                when "FunctionCall"
                    nomsu = expression(tok)
                    return nil unless nomsu
                    return "(..)\n    "..@indent(nomsu)
                when "List"
                    buff = "[..]"
                    line = "\n    "
                    for bit in *tok.value
                        nomsu = inline_expression bit
                        if line != "\n    " and #line + #", " + #nomsu > max_line
                            buff ..= line
                            line = "\n    "
                        sep = line == "\n    " and "" or ", "
                        if nomsu
                            line ..= sep..nomsu
                            if #line >= max_line
                                buff ..= line
                                line = "\n    "
                        else
                            line ..= sep..expression(bit)
                            buff ..= line
                            line = "\n    "
                    if line ~= "\n    "
                        buff ..= line
                    return buff
                when "Dict"
                    buff = "{..}"
                    line = "\n    "
                    for bit in *tok.value
                        key_nomsu = inline_expression bit.dict_key
                        return nil unless key_nomsu
                        if bit.dict_key.type == "FunctionCall"
                            key_nomsu = "("..key_nomsu..")"
                        value_nomsu = inline_expression bit.dict_value
                        if value_nomsu and #key_nomsu + #value_nomsu < max_line
                            line ..= key_nomsu.."="..value_nomsu..","
                            if #line >= max_line
                                buff ..= line
                                line = "\n    "
                        else
                            line ..= key_nomsu.."="..expression(bit.dict_value)
                            buff ..= line
                            line = "\n    "
                    if line ~= "\n    "
                        buff ..= line
                    return buff
                when "Text"
                    buff = '".."\n    '
                    for bit in *tok.value
                        if type(bit) == 'string'
                            buff ..= bit\gsub("\\","\\\\")\gsub("\n","\n    ")
                        else
                            nomsu = inline_expression(bit)
                            return nil unless nomsu
                            buff ..= if bit.type == "Var" or bit.type == "List" or bit.type == "Dict"
                                "\\"..nomsu
                            else "\\("..nomsu..")"
                    return buff
                when "Nomsu"
                    nomsu = expression(tok.value)
                    return nil if not nomsu
                    return "\\(..)\n    "..@indent(nomsu)
                when "Comment"
                    if tok.value\find("\n")
                        return "#.."..tok.value\gsub("\n","\n    ")
                    else
                        return "#"..tok.value
                else return inline_expression(tok)

        expression = (tok)->
            nomsu = inline_expression(tok)
            if nomsu and #nomsu < max_line
                return nomsu
            switch tok.type
                when "Block"
                    if #tok.value == 1
                        nomsu = if tok.value[1].type == "FunctionCall"
                            inline_expression(tok.value[1])
                        else
                            noeol_expression(tok.value[1])
                        if nomsu and #(nomsu\match("[^\n]*")) < max_line
                            return ": "..nomsu
                    return noeol_expression(tok)
                when "FunctionCall"
                    -- The hard task
                    buff = ""
                    for i,bit in ipairs tok.value
                        if bit.type == "Word"
                            if i == 1 or (is_operator(bit) and is_operator(tok.value[i-1])) or buff\sub(-2,-1) == ".."
                                buff ..= bit.value
                            else
                                buff ..= " "..bit.value
                        else
                            nomsu = inline_expression(bit)
                            if nomsu and #nomsu < max_line
                                if bit.type == "FunctionCall"
                                    nomsu = "("..nomsu..")"
                            else
                                nomsu = expression(bit)
                                return nil unless nomsu
                                if bit.type == "FunctionCall"
                                    nomsu = "(..)\n    "..@indent(nomsu)
                                if i < #tok.value
                                    nomsu ..= "\n.."
                            unless i == 1 or bit.type == "Block"
                                buff ..= " "
                            buff ..= nomsu
                    return buff
                when "File"
                    lines = {}
                    for line in *tree.value
                        nomsu = expression(line)
                        assert nomsu, "Failed to produce output for:\n#{colored.yellow line.src}"
                        
                        insert lines, nomsu
                    return concat lines, "\n"
                when "Comment"
                    if tok.value\find("\n")
                        return "#.."..tok.value\gsub("\n","\n    ")
                    else
                        return "#"..tok.value
                else return noeol_expression(tok)

        return expression(tree)


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
                    return "{#{concat ["#{@value_to_nomsu(k)}=#{@value_to_nomsu(v)}" for k,v in pairs(value)], ", "}}"
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

    @math_patt: re.compile [[ "%" (" " [*/^+-] " %")+ ]]
    tree_to_lua: (tree)=>
        -- Return <lua code for value>, <additional lua code>
        assert tree, "No tree provided."
        if not tree.type
            error "Invalid tree: #{repr(tree)}"
        switch tree.type
            when "File"
                if #tree.value == 1
                    return @tree_to_lua(tree.value[1])
                lua_bits = {}
                for line in *tree.value
                    lua = @tree_to_lua line
                    if not lua
                        error "No lua produced by #{repr line}"
                    if lua.statements then insert lua_bits, lua.statements
                    if lua.expr then insert lua_bits, "#{lua.expr};"
                return statements:concat(lua_bits, "\n")
            
            when "Comment"
                return statements:"--"..tree.value\gsub("\n","\n--")
            
            when "Nomsu"
                return expr:"nomsu:parse(#{repr tree.value.src}, #{repr tree\get_line_no!}).value[1]"

            when "Block"
                lua_bits = {}
                for arg in *tree.value
                    lua = @tree_to_lua arg
                    if #tree.value == 1 and lua.expr and not lua.statements
                        return expr:lua.expr
                    if lua.statements then insert lua_bits, lua.statements
                    if lua.expr then insert lua_bits, "#{lua.expr};"
                return statements:concat(lua_bits, "\n")

            when "FunctionCall"
                insert @compilestack, tree

                -- Rawget here to avoid triggering an error for accessing an undefined action
                fn = rawget(@environment.ACTIONS, tree.stub)
                metadata = @environment.ACTION_METADATA[fn]
                if metadata and metadata.compile_time
                    args = [arg for arg in *tree.value when arg.type != "Word"]
                    if metadata and metadata.arg_orders
                        new_args = [args[p] for p in *metadata.arg_orders[tree.stub]]
                        args = new_args
                    if @debug
                        @write "#{colored.bright "RUNNING MACRO"} #{colored.underscore colored.magenta(tree.stub)} "
                        @writeln "#{colored.bright "WITH ARGS:"} #{colored.dim repr [(repr a)\sub(1,50) for a in *args]}"
                    lua = fn(unpack(args))
                    remove @compilestack
                    return lua
                elseif not metadata and @@math_patt\match(tree.stub)
                    -- This is a bit of a hack, but this code handles arbitrarily complex
                    -- math expressions like 2*x + 3^2 without having to define a single
                    -- action for every possibility.
                    bits = {}
                    for tok in *tree.value
                        if tok.type == "Word"
                            insert bits, tok.value
                        else
                            lua = @tree_to_lua(tok)
                            assert(lua.statements == nil, "non-expression value inside math expression")
                            insert bits, lua.expr
                    remove @compilestack
                    return expr:"(#{concat bits, " "})"

                args = {}
                for tok in *tree.value
                    if tok.type == "Word" then continue
                    lua = @tree_to_lua(tok)
                    assert(lua.expr, "Cannot use #{tok.src} as an argument, since it's not an expression, it produces: #{repr lua}")
                    insert args, lua.expr

                if metadata and metadata.arg_orders
                    new_args = [args[p] for p in *metadata.arg_orders[tree.stub]]
                    args = new_args
                
                remove @compilestack
                return expr:@@comma_separated_items("ACTIONS[#{repr tree.stub}](", args, ")")

            when "Text"
                concat_parts = {}
                string_buffer = ""
                for bit in *tree.value
                    if type(bit) == "string"
                        string_buffer ..= bit
                        continue
                    if string_buffer ~= ""
                        insert concat_parts, repr(string_buffer)
                        string_buffer = ""
                    lua = @tree_to_lua bit
                    if @debug
                        @writeln (colored.bright "INTERP:")
                        @print_tree bit
                        @writeln "#{colored.bright "EXPR:"} #{lua.expr}, #{colored.bright "STATEMENT:"} #{lua.statements}"
                    if lua.statements
                        error "Cannot use [[#{bit.src}]] as a string interpolation value, since it's not an expression."
                    insert concat_parts, "stringify(#{lua.expr})"

                if string_buffer ~= ""
                    insert concat_parts, repr(string_buffer)

                if #concat_parts == 0
                    return expr:"''"
                elseif #concat_parts == 1
                    return expr:concat_parts[1]
                else return expr:"(#{concat(concat_parts, "..")})"

            when "List"
                items = {}
                for item in *tree.value
                    lua = @tree_to_lua item
                    if lua.statements
                        error "Cannot use [[#{item.src}]] as a list item, since it's not an expression."
                    insert items, lua.expr
                return expr:@@comma_separated_items("{", items, "}")

            when "Dict"
                items = {}
                for entry in *tree.value
                    key_lua = if entry.dict_key.type == "Word"
                        {expr:repr(entry.dict_key.value)}
                    else
                        @tree_to_lua entry.dict_key
                    if key_lua.statements
                        error "Cannot use [[#{entry.dict_key.src}]] as a dict key, since it's not an expression."
                    value_lua = @tree_to_lua entry.dict_value
                    if value_lua.statements
                        error "Cannot use [[#{entry.dict_value.src}]] as a dict value, since it's not an expression."
                    key_str = key_lua.expr\match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
                    if key_str
                        insert items, "#{key_str}=#{value_lua.expr}"
                    elseif key_lua.expr\sub(1,1) == "["
                        insert items, "[ #{key_lua.expr}]=#{value_lua.expr}"
                    else
                        insert items, "[#{key_lua.expr}]=#{value_lua.expr}"
                return expr:@@comma_separated_items("{", items, "}")

            when "Number"
                return expr:repr(tree.value)

            when "Var"
                return expr:@var_to_lua_identifier(tree.value)

            else
                error("Unknown/unimplemented thingy: #{tree.type}")
    
    walk_tree: (tree, depth=0)=>
        coroutine.yield(tree, depth)
        if type(tree) != 'table' or not tree.type
            return
        switch tree.type
            when "List", "File", "Block", "FunctionCall", "Text"
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
        Cs(((P("\\\\")/"\\") + (P("\\\"")/'"') + NOMSU_DEFS.escaped_char + P(1))^0)\match(str)

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

    tree_with_replaced_vars: (tree, replacements)=>
        if type(tree) != 'table' then return tree
        switch tree.type
            when "Var"
                if replacements[tree.value] ~= nil
                    tree = replacements[tree.value]
            when "File", "Nomsu", "Block", "List", "FunctionCall", "Text"
                new_value = @tree_with_replaced_vars tree.value, replacements
                if new_value != tree.value
                    tree = {k,v for k,v in pairs(tree)}
                    tree.value = new_value
            when "Dict"
                dirty = false
                replacements = {}
                for i,e in ipairs tree.value
                    new_key = @tree_with_replaced_vars e.dict_key, replacements
                    new_value = @tree_with_replaced_vars e.dict_value, replacements
                    dirty or= new_key != e.dict_key or new_value != e.dict_value
                    replacements[i] = {dict_key:new_key, dict_value:new_value}
                if dirty
                    tree = {k,v for k,v in pairs(tree)}
                    tree.value = replacements
            when nil -- Raw table, probably from one of the .value of a multi-value tree (e.g. List)
                new_values = {}
                any_different = false
                for k,v in pairs tree
                    new_values[k] = @tree_with_replaced_vars v, replacements
                    any_different or= (new_values[k] != tree[k])
                if any_different
                    tree = new_values
        return tree

    @stub_patt: re.compile "{|(' '+ / '\n..' / {'%' %id*} / {%id+} / {%op})*|}",
        id:NOMSU_DEFS.ident_char, op:NOMSU_DEFS.operator
    get_stub: (x)=>
        if not x
            error "Nothing to get stub from"
        -- Returns a single stub ("say %"), list of arg names ({"msg"}), and set of arg
        -- names that should not be evaluated from a single action def
        --   (e.g. "say %msg") or function call (e.g. FunctionCall({Word("say"), Var("msg")))
        if type(x) == 'string'
            -- Standardize format to stuff separated by spaces
            spec = concat @@stub_patt\match(x), " "
            arg_names = {}
            stub = spec\gsub "%%(%S*)", (arg)->
                insert(arg_names, arg)
                return "%"
            return stub, arg_names
        if type(x) != 'table'
            error "Invalid type for getting stub: #{type(x)} for:\n#{repr x}"
        switch x.type
            when "Text" then return @get_stub(x.value)
            when "FunctionCall" then return @get_stub(x.src)
            else error "Unsupported get stub type: #{x.type} for #{repr x}"

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
        "_"..(var\gsub "%W", (verboten)->
            if verboten == "_" then "__" else ("_%x")\format(verboten\byte!))
    
    source_code: (level=0)=>
        @dedent @compilestack[#@compilestack-level].src

    initialize_core: =>
        -- Sets up some core functionality
        nomsu = self
        nomsu_string_as_lua = (code)->
            concat_parts = {}
            for bit in *code.value
                if type(bit) == "string"
                    insert concat_parts, bit
                else
                    lua = nomsu\tree_to_lua bit
                    if lua.statements
                        error "Cannot use [[#{bit.src}]] as a string interpolation value, since it's not an expression."
                    insert concat_parts, lua.expr
            return concat(concat_parts)
        
        @define_compile_action "immediately %block", "nomsu.moon", (_block)->
            lua = nomsu\tree_to_lua(_block)
            lua_code = lua.statements or (lua.expr..";")
            lua_code = "-- Immediately:\n"..lua_code
            nomsu\run_lua(lua_code)
            return statements:lua_code

        @define_compile_action "lua> %code", "nomsu.moon", (_code)->
            lua = nomsu_string_as_lua(_code)
            return statements:lua

        @define_compile_action "=lua %code", "nomsu.moon", (_code)->
            lua = nomsu_string_as_lua(_code)
            return expr:lua

        @define_compile_action "__line_no__", "nomsu.moon", ->
            expr: repr(nomsu.compilestack[#nomsu.compilestack]\get_line_no!)

        @define_compile_action "__src__ %level", "nomsu.moon", (_level)->
            expr: repr(nomsu\source_code(nomsu\tree_to_value(_level)))

        @define_action "run file %filename", "nomsu.moon", (_filename)->
            nomsu\run_file(_filename)

        @define_compile_action "use %filename", "nomsu.moon", (_filename)->
            filename = nomsu\tree_to_value(_filename)
            nomsu\require_file(filename)
            return statements:"nomsu:require_file(#{repr filename});"

if arg
    export colors
    colors = require 'consolecolors'
    parser = re.compile([[
        args <- {| {:flags: flags? :} ({:input: input :} ";" ("-o;"{:output: output :} ";")?)? (";")? |} !.
        flags <- (({| ({flag} ";")* |}) -> set)
        flag <- "-c" / "-i" / "-p" / "-O" / "--help" / "-h" / "-v"
        input <- "-" / [^;]+
        output <- "-" / [^;]+
    ]], {:set})
    args = concat(arg, ";")..";"
    args = parser\match(args) or {}
    if not args or not args.flags or args.flags["--help"] or args.flags["-h"]
        print "Usage: lua nomsu.lua [-c] [-i] [-p] [-O] [--help] [input [-o output]]"
        os.exit!

    nomsu = NomsuCompiler()
    
    run = ->
        if args.flags["-v"]
            nomsu.debug = true

        nomsu.skip_precompiled = not args.flags["-O"]
        if args.input
            -- Read a file or stdin and output either the printouts or the compiled lua
            if args.flags["-c"] and not args.output
                args.output = args.input\gsub("%.nom", ".lua")
            compiled_output = nil
            if args.flags["-p"]
                _write = nomsu.write
                nomsu.write = ->
                compiled_output = io.output()
            elseif args.output
                compiled_output = io.open(args.output, 'w')

            if args.input\match(".*%.lua")
                retval = dofile(args.input)(nomsu, {})
            else
                input = if args.input == '-'
                    io.read('*a')
                else io.open(args.input)\read("*a")
                retval, code = nomsu\run(input, args.input)
                if args.output
                    compiled_output\write(code)

            if args.flags["-p"]
                nomsu.write = _write

        if args.flags["-i"]
            -- REPL
            nomsu\run('use "lib/core.nom"', "stdin")
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
                ok, ret = pcall(-> nomsu\run(buff, "stdin"))
                if ok and ret != nil
                    print "= "..repr(ret)
    
    err_hand = (error_message)->
        -- TODO: write properly to stderr
        print("#{colored.red "ERROR:"} #{colored.bright colored.yellow colored.onred (error_message or "")}")
        print("stack traceback:")

        import to_lua from require "moonscript.base"
        nomsu_file = io.open("nomsu.moon")
        nomsu_source = nomsu_file\read("*a")
        _, line_table = to_lua(nomsu_source)
        nomsu_file\close!

        level = 2
        while true
            calling_fn = debug.getinfo(level)
            if not calling_fn then break
            if calling_fn.func == run then break
            level += 1
            name = calling_fn.name
            if name == "run_lua_fn" then continue
            line = nil
            if metadata = nomsu.action_metadata[calling_fn.func]
                line = colored.yellow(metadata.line_no)
                name = colored.bright(colored.yellow(metadata.aliases[1]))
            else
                if calling_fn.istailcall and not name
                    name = "<tail call>"
                if calling_fn.short_src == "./nomsu.moon"
                    char = line_table[calling_fn.currentline]
                    line_num = 1
                    for _ in nomsu_source\sub(1,char)\gmatch("\n") do line_num += 1
                    line = colored.cyan("#{calling_fn.short_src}:#{line_num}")
                    name = colored.bright(colored.cyan(name or "???"))
                else
                    line = colored.blue("#{calling_fn.short_src}:#{calling_fn.currentline}")
                    name = colored.bright(colored.blue(name or "???"))
            _from = colored.dim colored.white "|"
            print(("%32s %s %s")\format(name, _from, line))

        os.exit(false, true)

    -- Note: xpcall has a slightly different API in Lua <=5.1 vs. >=5.2, but this works
    -- for both APIs
    xpcall(run, err_hand)

return NomsuCompiler
