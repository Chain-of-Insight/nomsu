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
--pcall = (fn,...)-> true, fn(...)
if _VERSION == "Lua 5.1"
    xp = xpcall
    xpcall = (f, errhandler, ...)->
        args = {n:select("#", ...), ...}
        return xp((...)-> f(unpack(args,1,args.n))), errhandler
--pcall = (fn, ...) -> xpcall(fn, debug.traceback, ...)

-- TODO:
-- consider non-linear codegen, rather than doing thunks for things like comprehensions
-- improve indentation of generated lua code
-- better scoping?
-- better error reporting
-- type checking?
-- Fix compiler bug that breaks when file ends with a block comment
-- Add compiler options for optimization level (compile-fast vs. run-fast, etc.)
-- Do a pass on all actions to enforce parameters-are-nouns heuristic

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
            src = ctx.source_code\sub(start,stop-1)
            return {:start, :stop, type: "FunctionCall", :src, get_line_no:ctx.get_line_no, :value, :stub}

    setmetatable(defs, {__index:(key)=>
        make_node = (start, value, stop)->
            {:start, :stop, :value, src:ctx.source_code\sub(start,stop-1), get_line_no:ctx.get_line_no, type: key}
        self[key] = make_node
        return make_node
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
        _ctx = {:source_code, :filename, indent_stack: {0}}
        _ctx.line_starts = re.compile("lines <- {| line ('\n' line)* |} line <- {} [^\n]*")\match(source_code)
        _ctx.get_line_no = =>
            unless @_line_no
                line_no = 1
                while (_ctx.line_starts[line_no+1] or math.huge) < @start do line_no += 1
                @_line_no = "#{_ctx.filename}:#{line_no}"
            return @_line_no

        old_ctx = ctx
        export ctx
        ctx = _ctx
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
        @ids = setmetatable({}, {
            __mode: "k"
            __index: (key)=>
                id = new_uuid!
                @[key] = id
                return id
        })
        if parent
            setmetatable(@defs, {__index:parent.defs})
            setmetatable(@defs["#vars"], {__index:parent["#vars"]})
            setmetatable(@defs["#loaded_files"], {__index:parent["#loaded_files"]})
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
    
    define_action: (signature, line_no, fn, src, compile_time=false)=>
        if type(signature) == 'string'
            signature = @get_stubs {signature}
        elseif type(signature) == 'table' and type(signature[1]) == 'string'
            signature = @get_stubs signature
        @assert type(fn) == 'function', "Bad fn: #{repr fn}"
        aliases = {}
        @@def_number += 1
        def = {:fn, :src, :line_no, :compile_time, aliases:{}, def_number:@@def_number, defs:@defs}
        where_defs_go = (getmetatable(@defs) or {}).__newindex or @defs
        for sig_i=1,#signature
            stub, arg_names, escaped_args = unpack(signature[sig_i])
            arg_positions = {}
            @assert stub, "NO STUB FOUND: #{repr signature}"
            if @debug then @writeln "#{colored.bright "DEFINING ACTION:"} #{colored.underscore colored.magenta repr(stub)} #{colored.bright "WITH ARGS"} #{colored.dim repr(arg_names)}"
            for i=1,#arg_names-1 do for j=i+1,#arg_names
                if arg_names[i] == arg_names[j] then @error "Duplicate argument in function #{stub}: '#{arg_names[i]}'"
            
            if sig_i == 1
                arg_positions = [i for i=1,#arg_names]
                def.args = arg_names
                def.escaped_args = escaped_args
            else
                @assert equivalent(set(def.args), set(arg_names)), "Mismatched args"
                @assert equivalent(def.escaped_args, escaped_args), "Mismatched escaped args"
                for j,a in ipairs(arg_names)
                    for i,c_a in ipairs(def.args)
                        if a == c_a
                            arg_positions[j] = i
            insert def.aliases, stub
            stub_def = setmetatable({:stub, :arg_names, :arg_positions}, {__index:def})
            rawset(where_defs_go, stub, stub_def)

    define_compile_action: (signature, line_no, fn, src)=>
        @define_action(signature, line_no, fn, src, true)

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

    parse: (str, filename)=>
        @assert type(filename) == "string", "Bad filename type: #{type filename}"
        if @debug
            @writeln("#{colored.bright "PARSING:"}\n#{colored.yellow str}")
        str = str\gsub("\r","")
        tree = parse(str, filename)
        @assert tree, "In file #{colored.blue filename} failed to parse:\n#{colored.onyellow colored.black str}"
        if @debug
            @writeln "PARSE TREE:"
            @print_tree tree, "    "
        return tree

    run: (src, filename, max_operations=nil, output_file=nil)=>
        if src == "" then return nil, ""
        if max_operations
            timeout = ->
                debug.sethook!
                @error "Execution quota exceeded. Your code took too long."
            debug.sethook timeout, "", max_operations
        tree = @parse(src, filename)
        @assert tree, "Failed to parse: #{src}"
        @assert tree.type == "File", "Attempt to run non-file: #{tree.type}"

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
            return dofile(filename)(@)
        if filename\match(".*%.nom")
            if not @skip_precompiled -- Look for precompiled version
                file = io.open(filename\gsub("%.nom", ".lua"), "r")
                if file
                    lua_code = file\read("*a")
                    file\close!
                    return @run_lua(lua_code)
            file = file or io.open(filename)
            if not file
                @error "File does not exist: #{filename}"
            nomsu_code = file\read('*a')
            file\close!
            return @run(nomsu_code, filename)
        else
            @error "Invalid filetype for #{filename}"
    
    require_file: (filename)=>
        loaded = @defs["#loaded_files"]
        if not loaded[filename]
            loaded[filename] = @run_file(filename) or true
        return loaded[filename]

    run_lua: (lua_code)=>
        load_lua_fn, err = load([[
return function(nomsu)
    %s
end]]\format(lua_code))
        if @debug
            @writeln "#{colored.bright "RUNNING LUA:"}\n#{colored.blue colored.bright(lua_code)}"
        if not load_lua_fn
            n = 1
            fn = ->
                n = n + 1
                ("\n%-3d|")\format(n)
            code = "1  |"..lua_code\gsub("\n", fn)
            @error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack code}\n\n#{err}")
        run_lua_fn = load_lua_fn!
        run_lua_fn(self)
        return ret
    
    tree_to_value: (tree, filename)=>
        code = "return (function(nomsu)\nreturn #{@tree_to_lua(tree).expr};\nend);"
        code = "-- Tree to value: #{filename}\n"..code
        if @debug
            @writeln "#{colored.bright "RUNNING LUA TO GET VALUE:"}\n#{colored.blue colored.bright(code)}"
        lua_thunk, err = load(code)
        if not lua_thunk
            @error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack code}\n\n#{colored.red err}")
        return (lua_thunk!)(self)

    tree_to_nomsu: (tree, force_inline=false)=>
        -- Return <nomsu code>, <is safe for inline use>
        @assert tree, "No tree provided."
        if not tree.type
            --@errorln debug.traceback()
            @error "Invalid tree: #{repr(tree)}"
        switch tree.type
            when "File"
                return concat([@tree_to_nomsu(v, force_inline) for v in *tree.value], "\n"), false
            
            when "Nomsu"
                inside, inline = @tree_to_nomsu(tree.value, force_inline)
                return "\\#{inside}", inline

            when "Block"
                if force_inline
                    return "(:#{concat([@tree_to_nomsu(v, true) for v in *tree.value], "; ")})", true
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

            when "Text"
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
                -- TODO: Implement
                @error("Sorry, not yet implemented.")

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
        @assert tree, "No tree provided."
        if not tree.type
            --@errorln debug.traceback()
            @error "Invalid tree: #{repr(tree)}"
        switch tree.type
            when "File"
                if #tree.value == 1
                    return @tree_to_lua(tree.value[1])
                lua_bits = {}
                for line in *tree.value
                    lua = @tree_to_lua line
                    if not lua
                        @error "No lua produced by #{repr line}"
                    if lua.statements then insert lua_bits, lua.statements
                    if lua.expr then insert lua_bits, "#{lua.expr};"
                return statements:concat(lua_bits, "\n")
            
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

                def = @defs[tree.stub]
                if def and def.compile_time
                    args = [arg for arg in *tree.value when arg.type != "Word"]
                    if @debug
                        @write "#{colored.bright "RUNNING MACRO"} #{colored.underscore colored.magenta(tree.stub)} "
                        @writeln "#{colored.bright "WITH ARGS:"} #{colored.dim repr args}"
                    lua = @defs[tree.stub].fn(self, unpack(args))
                    remove @compilestack
                    return lua
                elseif not def and @@math_patt\match(tree.stub)
                    -- This is a bit of a hack, but this code handles arbitrarily complex
                    -- math expressions like 2*x + 3^2 without having to define a single
                    -- action for every possibility.
                    bits = {}
                    for tok in *tree.value
                        if tok.type == "Word"
                            insert bits, tok.value
                        else
                            lua = @tree_to_lua(tok)
                            @assert(lua.statements == nil, "non-expression value inside math expression")
                            insert bits, lua.expr
                    remove @compilestack
                    return expr:"(#{concat bits, " "})"

                arg_positions = def and def.arg_positions or {}
                args = {}
                for tok in *tree.value
                    if tok.type == "Word" then continue
                    lua = @tree_to_lua(tok)
                    @assert(lua.expr, "Cannot use #{tok.src} as an argument, since it's not an expression.")
                    insert args, lua.expr

                if def
                    new_args = [args[p] for p in *def.arg_positions]
                    args = new_args
                
                insert args, 1, "nomsu"
                remove @compilestack
                return expr:@@comma_separated_items("nomsu.defs[#{repr tree.stub}].fn(", args, ")")

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
                        @error "Cannot use [[#{bit.src}]] as a string interpolation value, since it's not an expression."
                    insert concat_parts, "nomsu:stringify(#{lua.expr})"

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
                        @error "Cannot use [[#{item.src}]] as a list item, since it's not an expression."
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
                        @error "Cannot use [[#{entry.dict_key.src}]] as a dict key, since it's not an expression."
                    value_lua = @tree_to_lua entry.dict_value
                    if value_lua.statements
                        @error "Cannot use [[#{entry.dict_value.src}]] as a dict value, since it's not an expression."
                    key_str = key_lua.expr\match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
                    if key_str
                        insert items, "#{key_str}=#{value_lua.expr}"
                    else
                        insert items, "[#{key_lua.expr}]=#{value_lua.expr}"
                return expr:@@comma_separated_items("{", items, "}")

            when "Number"
                return expr:repr(tree.value)

            when "Var"
                return expr:("_"..@var_to_lua_identifier(tree.value))

            else
                @error("Unknown/unimplemented thingy: #{tree.type}")
    
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
            when "File", "Nomsu", "Block", "List", "FunctionCall", "Text"
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

    @stub_patt: re.compile "{|(' '+ / '\n..' / {'\\'? '%' %id*} / {%id+} / {%op})*|}",
        id:IDENT_CHAR, op:OPERATOR_CHAR
    get_stub: (x)=>
        if not x
            @error "Nothing to get stub from"
        -- Returns a single stub ("say %"), list of arg names ({"msg"}), and set of arg
        -- names that should not be evaluated from a single action def
        --   (e.g. "say %msg") or function call (e.g. FunctionCall({Word("say"), Var("msg")))
        if type(x) == 'string'
            -- Standardize format to stuff separated by spaces
            spec = concat @@stub_patt\match(x), " "
            stub = spec\gsub("%%%S+","%%")\gsub("\\","")
            arg_names = [arg for arg in spec\gmatch("%%(%S*)")]
            escaped_args = {arg, true for arg in spec\gmatch("\\%%(%S*)")}
            return stub, arg_names, escaped_args
        if type(x) != 'table'
            @error "Invalid type for getting stub: #{type(x)} for:\n#{repr x}"
        switch x.type
            when "Text" then return @get_stub(x.value)
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
    
    assert: (condition, msg='')=>
        if not condition
            @error("Assertion failed: "..msg)
        return condition

    error: (msg)=>
        error msg, 0
    
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
                    lua = @tree_to_lua bit
                    if lua.statements
                        @error "Cannot use [[#{bit.src}]] as a string interpolation value, since it's not an expression."
                    insert concat_parts, lua.expr
            return concat(concat_parts)

        @define_compile_action "do %block", "nomsu.moon", (_block)=>
            make_line = (lua)-> lua.expr and (lua.expr..";") or lua.statements
            if _block.type == "Block"
                return @tree_to_lua(_block)
            else
                return expr:"#{@tree_to_lua _block}(nomsu)"
        
        @define_compile_action "immediately %block", "nomsu.moon", (_block)=>
            lua = @tree_to_lua(_block)
            lua_code = lua.statements or (lua.expr..";")
            lua_code = "-- Immediately:\n"..lua_code
            @run_lua(lua_code)
            return statements:lua_code

        @define_compile_action "lua> %code", "nomsu.moon", (_code)=>
            lua = nomsu_string_as_lua(@, _code)
            return statements:lua

        @define_compile_action "=lua %code", "nomsu.moon", (_code)=>
            lua = nomsu_string_as_lua(@, _code)
            return expr:lua

        @define_compile_action "__line_no__", "nomsu.moon", ()=>
            expr: repr(@compilestack[#@compilestack]\get_line_no!)

        @define_compile_action "__src__ %level", "nomsu.moon", (_level)=>
            expr: repr(@source_code(@tree_to_value(_level)))

        @define_action "run file %filename", "nomsu.moon", (_filename)=>
            @run_file(_filename)

        @define_compile_action "use %filename", "nomsu.moon", (_filename)=>
            filename = @tree_to_value(_filename)
            @require_file(filename)
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
            nomsu\run('require "lib/core.nom"', "stdin")
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
        print("#{colored.red "ERROR:"} #{colored.bright colored.yellow colored.onred (error_message or "")}")
        print("stack traceback:")

        import to_lua from require "moonscript.base"
        nomsu_file = io.open("nomsu.moon")
        nomsu_source = nomsu_file\read("*a")
        _, line_table = to_lua(nomsu_source)
        nomsu_file\close!

        function_defs = {def.fn, def for _,def in pairs(nomsu.defs) when def.fn}
        level = 2
        while true
            calling_fn = debug.getinfo(level)
            if not calling_fn then break
            if calling_fn.func == run then break
            level += 1
            name = calling_fn.name
            if name == "run_lua_fn" then continue
            line = nil
            if def = function_defs[calling_fn.func]
                line = colored.yellow(def.line_no)
                name = colored.bright(colored.yellow(def.aliases[1]))
            else
                if calling_fn.istailcall and not name
                    name = "<tail call>"
                if calling_fn.short_src == "./nomsu.moon"
                    -- Ugh, magic numbers, but this works
                    char = line_table[calling_fn.linedefined-2]
                    line_num = 3
                    for _ in nomsu_source\sub(1,char)\gmatch("\n") do line_num += 1
                    line = colored.cyan("#{calling_fn.short_src}:#{line_num}")
                    name = colored.bright(colored.cyan(name or "???"))
                else
                    line = colored.blue("#{calling_fn.short_src}:#{calling_fn.linedefined}")
                    name = colored.bright(colored.blue(name or "???"))
            _from = colored.dim colored.white "|"
            print(("%32s %s %s")\format(name, _from, line))

        os.exit(false, true)

    xpcall(run, err_hand)

return NomsuCompiler
