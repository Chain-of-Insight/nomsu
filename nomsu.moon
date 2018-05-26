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
export lpeg, re
_pairs, _ipairs = pairs, ipairs
if jit
    package.cpath = "./luajit_lpeg/?.so;"..package.cpath
    
    export bit32
    bit32 = require('bit')

    export pairs, ipairs
    pairs = (x)->
        if mt = getmetatable(x)
            if mt.__pairs
                return mt.__pairs(x)
        return _pairs(x)
    ipairs = (x)->
        if mt = getmetatable(x)
            if mt.__ipairs
                return mt.__ipairs(x)
        return _ipairs(x)

re = require 're'
lpeg = require 'lpeg'
lpeg.setmaxstack 10000
{:P,:R,:V,:S,:Cg,:C,:Cp,:B} = lpeg
utils = require 'utils'
new_uuid = require 'uuid'
immutable = require 'immutable'
export Tuple
Tuple = immutable(nil, {name:"Tuple"})
{:repr, :stringify, :min, :max, :equivalent, :set, :is_list, :sum} = utils
colors = setmetatable({}, {__index:->""})
export colored
colored = setmetatable({}, {__index:(_,color)-> ((msg)-> colors[color]..tostring(msg or '')..colors.reset)})
{:insert, :remove, :concat} = table
debug_getinfo = debug.getinfo
{:Nomsu, :Lua, :Source} = require "code_obj"
STDIN, STDOUT, STDERR = "/dev/fd/0", "/dev/fd/1", "/dev/fd/2"

-- TODO:
-- consider non-linear codegen, rather than doing thunks for things like comprehensions
-- type checking?
-- Add compiler options for optimization level (compile-fast vs. run-fast, etc.)
-- Do a pass on all actions to enforce parameters-are-nouns heuristic
-- Maybe do some sort of lazy definitions of actions that defer until they're used in code
-- Add a ((%x foo %y) where {x:"asdf", y:"fdsa"}) compile-time action for substitution
-- Maybe support some kind of regex action definitions like "foo %first (and %next)*"?
-- Re-implement nomsu-to-lua comment translation?

export FILE_CACHE
-- FILE_CACHE is a map from filename (string) -> string of file contents
FILE_CACHE = setmetatable {}, {
    __index: (filename)=>
        file = io.open(filename)
        return nil unless file
        contents = file\read("*a")
        file\close!
        self[filename] = contents
        return contents
}

iterate_single = (item, prev) -> if item == prev then nil else item
all_files = (path)->
    -- Sanitize path
    if path\match("%.nom$") or path\match("%.lua$") or path\match("^/dev/fd/[012]$")
        return iterate_single, path
    -- TODO: improve sanitization
    path = path\gsub("\\","\\\\")\gsub("`","")\gsub('"','\\"')\gsub("$","")
    return coroutine.wrap ->
        f = io.popen('find -L "'..path..'" -not -path "*/\\.*" -type f -name "*.nom"')
        for line in f\lines!
            coroutine.yield(line)
        success = f\close!
        unless success
            error("Invalid file path: "..tostring(path))

line_counter = re.compile([[
    lines <- {| line (%nl line)* |}
    line <- {} (!%nl .)*
]], nl:P("\r")^-1 * P("\n"))
-- Mapping from line number -> character offset
export LINE_STARTS
-- LINE_STARTS is a mapping from strings to a table that maps line number to character positions
LINE_STARTS = setmetatable {}, {
    __mode:"k"
    __index: (k)=>
        -- Implicitly convert Lua and Nomsu objects to strings
        if type(k) != 'string'
            k = tostring(k)
            if v = rawget(self, k)
                return v
        line_starts = line_counter\match(k)
        self[k] = line_starts
        return line_starts
}

-- Map from unique nomsu chunkname to:
--   lua_to_nomsu, nomsu_to_lua, lua_sources, nomsu_sources,
--   nomsu_filename, nomsu_file, lua_filename, lua_file
LUA_METADATA = {}

lua_line_to_nomsu_line = (lua_filename, lua_line_no)->
    metadata = assert LUA_METADATA[lua_filename], "Failed to find nomsu metadata for: #{lua_filename}."
    lua_offset = LINE_STARTS[metadata.lua_file][lua_line_no]
    best = metadata.nomsu_sources[1]
    for lua,nomsu in pairs metadata.lua_to_nomsu
        if lua.start <= lua_offset and lua > best
            best = lua
    return best\get_line_number!

-- Use + operator for string coercive concatenation (note: "asdf" + 3 == "asdf3")
-- Use [] for accessing string characters, or s[{3,4}] for s:sub(3,4)
-- Note: This globally affects all strings in this instance of Lua!
do
    STRING_METATABLE = getmetatable("")
    STRING_METATABLE.__add = (other)=> @ .. stringify(other)
    STRING_METATABLE.__index = (i)=>
        ret = string[i]
        if ret != nil then return ret
        if type(i) == 'number' then return string.sub(@, i, i)
        elseif type(i) == 'table' then return string.sub(@, i[1], i[2])

Types = require "nomsu_tree"

NOMSU_DEFS = with {}
    -- Newline supports either windows-style CR+LF or unix-style LF
    .nl = P("\r")^-1 * P("\n")
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

    -- If the line begins with #indent+4 spaces, the pattern matches *those* spaces
    -- and adds them to the stack (not any more).
    .indent = P (start)=>
        nodent = lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack]
        indented = nodent.."    "
        if @sub(start, start+#indented-1) == indented
            insert(lpeg.userdata.indent_stack, indented)
            return start + #indented
    -- If the number of leading space characters is <= the number of space on the top of the
    -- stack minus 4, this pattern matches and pops off the top of the stack exactly once.
    .dedent = P (start)=>
        nodent = lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack]
        spaces = @match("^[ ]*", start)
        if #spaces <= #nodent-4
            remove(lpeg.userdata.indent_stack)
            return start
    -- If the number of leading space characters is >= the number on the top of the
    -- stack, this pattern matches and does not modify the stack.
    .nodent = P (start)=>
        nodent = lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack]
        if @sub(start, start+#nodent-1) == nodent
            return start + #nodent

    .error = (src,end_pos,start_pos,err_msg)->
        seen_errors = lpeg.userdata.errors
        if seen_errors[start_pos]
            return true
        err_pos = start_pos
        --if src\sub(err_pos,err_pos)\match("[\r\n]")
        --    err_pos += #src\match("[ \t\n\r]*", err_pos)
        text_loc = lpeg.userdata.source\sub(err_pos,err_pos)
        line_no = text_loc\get_line_number!
        src = FILE_CACHE[text_loc.filename]
        prev_line = line_no == 1 and "" or src\sub(LINE_STARTS[src][line_no-1] or 1, LINE_STARTS[src][line_no]-2)
        err_line = src\sub(LINE_STARTS[src][line_no], (LINE_STARTS[src][line_no+1] or 0)-2)
        next_line = src\sub(LINE_STARTS[src][line_no+1] or -1, (LINE_STARTS[src][line_no+2] or 0)-2)
        pointer = ("-")\rep(err_pos-LINE_STARTS[src][line_no]) .. "^"
        err_msg = (err_msg or "Parse error").." at #{lpeg.userdata.source.filename}:#{line_no}:\n"
        if #prev_line > 0 then err_msg ..= "\n"..prev_line
        err_msg ..= "\n#{err_line}\n#{pointer}"
        if #next_line > 0 then err_msg ..= "\n"..next_line
        --error(err_msg)
        seen_errors[start_pos] = err_msg
        return true

setmetatable(NOMSU_DEFS, {__index:(key)=>
    make_node = (start, value, stop)->
        source = lpeg.userdata.source\sub(start, stop)
        tree = if Types[key].is_multi
            Types[key](Tuple(unpack(value)), source)
        else Types[key](value, source)
        return tree
    self[key] = make_node
    return make_node
})

NOMSU_PATTERN = do
    -- Just for cleanliness, I put the language spec in its own file using a slightly modified
    -- version of the lpeg.re syntax.
    peg_tidier = re.compile [[
    file <- {~ %nl* (def/comment) (%nl+ (def/comment))* %nl* ~}
    def <- anon_def / captured_def
    anon_def <- ({ident} (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- %2"
    captured_def <- ({ident} (" "*) "(" {ident} ")" (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- (({} %3 {}) -> %2)"
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    comment <- "--" [^%nl]*
    ]]
    nomsu_peg = peg_tidier\match(FILE_CACHE["nomsu.peg"])
    re.compile(nomsu_peg, NOMSU_DEFS)

class NomsuCompiler
    new: =>
        -- Weak-key mapping from objects to randomly generated unique IDs
        NaN_surrogate = {}
        nil_surrogate = {}
        @ids = setmetatable({}, {
            __mode: "k"
            __index: (key)=>
                if key == nil then return @[nil_surrogate]
                elseif key != key then return @[NaN_surrogate]
                id = new_uuid!
                @[key] = id
                return id
        })
        @file_metadata = setmetatable({}, {__mode:"k"})

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
        @environment.len = if jit
            (x)->
                if mt = getmetatable(x)
                    if mt.__len
                        return mt.__len(x)
                return #x
        else ((x) -> #x)
        @environment.ipairs = (x)->
            if type(x) == 'function'
                return coroutine.wrap(x)
            elseif type(x) == 'thread'
                return coroutine.resume, x, nil
            elseif mt = getmetatable(x)
                if mt.__ipairs
                    return mt.__ipairs(x)
            else return _ipairs(x)
        @environment.pairs = (x)->
            if type(x) == 'function'
                return coroutine.wrap(x)
            elseif type(x) == 'thread'
                return coroutine.resume, x, nil
            elseif mt = getmetatable(x)
                if mt.__pairs
                    return mt.__pairs(x)
            else return _pairs(x)
        for k,v in pairs(Types) do @environment[k] = v
        @environment.Tuple = Tuple
        @environment.Lua = Lua
        @environment.Nomsu = Nomsu
        @environment.Source = Source
        @environment.ACTIONS = setmetatable({}, {__index:(key)=>
            (...)->
                error("Attempt to run undefined action: #{key}", 0)
        })
        @environment.COMPILE_ACTIONS = {}
        @environment.ARG_ORDERS = setmetatable({}, {__mode:"k"})
        @environment.LOADED = {}
        @environment.Types = Types
        @initialize_core!
    
    stub_defs = {
        space:(P(' ') + P('\n..'))^0
        word:(NOMSU_DEFS.ident_char^1 + NOMSU_DEFS.operator)
        varname:(R('az','AZ','09') + P('_') + NOMSU_DEFS.utf8_char + (-P("'") * NOMSU_DEFS.operator))^0
    }
    stub_pattern = re.compile [=[
        {~ (%space->'') (('%' (%varname->'')) / %word)? ((%space->' ') (('%' (%varname->'')) / %word))* (%space->'') ~}
    ]=], stub_defs
    var_pattern = re.compile "{| %space ((('%' {%varname}) / %word) %space)+ |}", stub_defs
    define_action: (signature, fn, is_compile_action=false)=>
        assert(type(fn) == 'function', "Bad fn: #{repr fn}")
        if type(signature) == 'string'
            signature = {signature}
        elseif type(signature) != 'table'
            error("Invalid signature, expected list of strings, but got: #{repr signature}", 0)

        fn_info = debug_getinfo(fn, "u")
        assert(not fn_info.isvararg, "Vararg functions aren't supported. Sorry, use a list instead.")
        fn_arg_positions = {debug.getlocal(fn, i), i for i=1,fn_info.nparams}
        arg_orders = {}
        for alias in *signature
            stub = assert(stub_pattern\match(alias))
            stub_args = assert(var_pattern\match(alias))
            (is_compile_action and @environment.COMPILE_ACTIONS or @environment.ACTIONS)[stub] = fn
            arg_orders[stub] = [fn_arg_positions[Types.Var.as_lua_id {value:a}] for a in *stub_args]
        @environment.ARG_ORDERS[fn] = arg_orders

    define_compile_action: (signature, fn)=>
        return @define_action(signature, fn, true)

    _nomsu_chunk_counter = 0
    parse: (nomsu_code)=>
        if type(nomsu_code) == 'string'
            _nomsu_chunk_counter += 1
            filename = "<nomsu chunk ##{_nomsu_chunk_counter}>.nom"
            FILE_CACHE[filename] = nomsu_code
            nomsu_code = Nomsu(filename, nomsu_code)
        userdata = {
            source_code:nomsu_code, indent_stack: {""}, errors: {},
            source: nomsu_code.source,
        }

        old_userdata, lpeg.userdata = lpeg.userdata, userdata
        tree = NOMSU_PATTERN\match(tostring(nomsu_code))
        lpeg.userdata = old_userdata
        
        assert tree, "In file #{colored.blue filename} failed to parse:\n#{colored.onyellow colored.black nomsu_code}"

        if next(userdata.errors)
            keys = utils.keys(userdata.errors)
            table.sort(keys)
            errors = [userdata.errors[k] for k in *keys]
            error(concat(errors, "\n\n"), 0)
        
        return tree

    run: (nomsu_code, compile_fn=nil)=>
        if #tostring(nomsu_code) == 0 then return nil
        tree = @parse(nomsu_code)
        assert tree, "Failed to parse: #{nomsu_code}"
        lua = @tree_to_lua(tree)\as_statements!
        lua\declare_locals!
        lua\prepend "-- File: #{nomsu_code.source or ""}\n"
        if compile_fn
            compile_fn(lua)
        return @run_lua(lua)

    _running_files = {}
    run_file: (filename, compile_fn=nil)=>
        loaded = @environment.LOADED
        if loaded[filename]
            return loaded[filename]
        ret = nil
        for filename in all_files(filename)
            if loaded[filename]
                ret = loaded[filename]
                continue

            for i,running in ipairs _running_files
                if running == filename
                    loop = [_running_files[j] for j=i,#_running_files]
                    insert loop, filename
                    error("Circular import, this loops forever: #{concat loop, " -> "}")

            insert _running_files, filename
            if filename\match("%.lua$")
                file = assert(FILE_CACHE[filename], "Could not find file: #{filename}")
                ret = @run_lua(Lua(Source(filename), file))
            elseif filename\match("%.nom$") or filename\match("^/dev/fd/[012]$")
                if not @skip_precompiled -- Look for precompiled version
                    lua_filename = filename\gsub("%.nom$", ".lua")
                    file = FILE_CACHE[lua_filename]
                    if file
                        ret = @run_lua(Lua(Source(filename), file))
                        remove _running_files
                        continue
                file = file or FILE_CACHE[filename]
                if not file
                    error("File does not exist: #{filename}", 0)
                ret = @run(Nomsu(Source(filename), file), compile_fn)
            else
                error("Invalid filetype for #{filename}", 0)
            loaded[filename] = ret or true
            remove _running_files

        loaded[filename] = ret or true
        return ret

    run_lua: (lua)=>
        assert(type(lua) != 'string', "Attempt to run lua string instead of Lua (object)")
        lua_string = tostring(lua)
        run_lua_fn, err = load(lua_string, filename, "t", @environment)
        if not run_lua_fn
            n = 1
            fn = ->
                n = n + 1
                ("\n%-3d|")\format(n)
            line_numbered_lua = "1  |"..lua_string\gsub("\n", fn)
            error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack line_numbered_lua}\n\n#{err}", 0)
        return run_lua_fn!

    MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value
    math_expression = re.compile [[ ([+-] " ")* "%" (" " [*/^+-] (" " [+-])* " %")+ !. ]]
    tree_to_lua: (tree)=>
        switch tree.type
            when "Action"
                stub = tree\get_stub!
                compile_action = @environment.COMPILE_ACTIONS[stub]
                if compile_action
                    args = [arg for arg in *tree.value when type(arg) != "string"]
                    -- Force all compile-time actions to take a tree location
                    args = [args[p-1] for p in *@environment.ARG_ORDERS[compile_action][stub]]
                    -- Force Lua to avoid tail call optimization for debugging purposes
                    -- TODO: use tail call
                    ret = compile_action(tree, unpack(args))
                    if not ret then error("Failed to produce any Lua")
                    return ret
                action = rawget(@environment.ACTIONS, stub)
                lua = Lua.Value(tree.source)
                if not action and math_expression\match(stub)
                    -- This is a bit of a hack, but this code handles arbitrarily complex
                    -- math expressions like 2*x + 3^2 without having to define a single
                    -- action for every possibility.
                    for i,tok in ipairs tree.value
                        if type(tok) == 'string'
                            lua\append tok
                        else
                            tok_lua = @tree_to_lua(tok)
                            unless tok_lua.is_value
                                error("non-expression value inside math expression: #{colored.yellow repr(tok)}")
                            if tok.type == "Action"
                                tok_lua\parenthesize!
                            lua\append tok_lua
                        if i < #tree.value
                            lua\append " "
                    return lua

                args = {}
                for i, tok in ipairs tree.value
                    if type(tok) == "string" then continue
                    arg_lua = @tree_to_lua(tok)
                    unless arg_lua.is_value
                        error "Cannot use:\n#{colored.yellow repr(tok)}\nas an argument to #{stub}, since it's not an expression, it produces: #{repr arg_lua}", 0
                    insert args, arg_lua

                if action
                    args = [args[p] for p in *@environment.ARG_ORDERS[action][stub]]

                -- Not really worth bothering with ACTIONS.foo(...) style since almost every action
                -- has arguments, so it won't work
                lua\append "ACTIONS[",repr(stub),"]("
                for i, arg in ipairs args
                    lua\append arg
                    if i < #args then lua\append ", "
                lua\append ")"
                return lua

            when "EscapedNomsu"
                make_tree = (t)->
                    if type(t) != 'userdata'
                        return repr(t)
                    if t.is_multi
                        bits = [make_tree(bit) for bit in *t.value]
                        return t.type.."(Tuple("..table.concat(bits, ", ").."), "..repr(t.source)..")"
                    else
                        return t.type.."("..repr(t.value)..", "..repr(t.source)..")"
                Lua.Value tree.source, make_tree(tree.value[1])
            
            when "Block"
                lua = Lua(tree.source)
                for i,line in ipairs tree.value
                    line_lua = @tree_to_lua(line)
                    if i > 1
                        lua\append "\n"
                    lua\append line_lua\as_statements!
                return lua

            when "Text"
                lua = Lua.Value(tree.source)
                string_buffer = ""
                for i, bit in ipairs tree.value
                    if type(bit) == "string"
                        string_buffer ..= bit
                        continue
                    if string_buffer ~= ""
                        if #lua.bits > 0 then lua\append ".."
                        lua\append repr(string_buffer)
                        string_buffer = ""
                    bit_lua = @tree_to_lua(bit)
                    unless bit_lua.is_value
                        error "Cannot use #{colored.yellow repr(bit)} as a string interpolation value, since it's not an expression.", 0
                    if #lua.bits > 0 then lua\append ".."
                    if bit.type != "Text"
                        bit_lua = Lua.Value(bit.source, "stringify(",bit_lua,")")
                    lua\append bit_lua

                if string_buffer ~= "" or #lua.bits == 0
                    if #lua.bits > 0 then lua\append ".."
                    lua\append repr(string_buffer)

                if #lua.bits > 1
                    lua\parenthesize!
                return lua

            when "List"
                lua = Lua.Value tree.source, "{"
                line_length = 0
                for i, item in ipairs tree.value
                    item_lua = @tree_to_lua(item)
                    unless item_lua.is_value
                        error "Cannot use #{colored.yellow repr(item)} as a list item, since it's not an expression.", 0
                    lua\append item_lua
                    item_string = tostring(item_lua)
                    last_line = item_string\match("[^\n]*$")
                    if item_string\match("\n")
                        line_length = #last_line
                    else
                        line_length += #last_line
                    if i < #tree.value
                        if line_length >= MAX_LINE
                            lua\append ",\n  "
                            line_length = 0
                        else
                            lua\append ", "
                            line_length += 2
                lua\append "}"
                return lua

            when "Dict"
                lua = Lua.Value tree.source, "{"
                line_length = 0
                for i, entry in ipairs tree.value
                    entry_lua = @tree_to_lua(entry)
                    lua\append entry_lua
                    entry_lua_str = tostring(entry_lua)
                    -- TODO: maybe make this more accurate? It's only a heuristic, so eh...
                    last_line = entry_lua_str\match("\n([^\n]*)$")
                    if last_line
                        line_length = #last_line
                    else
                        line_length += #entry_lua_str
                    if i < #tree.value
                        if line_length >= MAX_LINE
                            lua\append ",\n  "
                            line_length = 0
                        else
                            lua\append ", "
                            line_length += 2
                lua\append "}"
                return lua

            when "DictEntry"
                key, value = tree.value[1], tree.value[2]
                key_lua = @tree_to_lua(key)
                unless key_lua.is_value
                    error "Cannot use #{colored.yellow repr(key)} as a dict key, since it's not an expression.", 0
                value_lua = value and @tree_to_lua(value) or Lua.Value(key.source, "true")
                unless value_lua.is_value
                    error "Cannot use #{colored.yellow repr(value)} as a dict value, since it's not an expression.", 0
                key_str = tostring(key_lua)\match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
                return if key_str
                    Lua tree.source, key_str,"=",value_lua
                elseif tostring(key_lua)\sub(1,1) == "["
                    -- NOTE: this *must* use a space after the [ to avoid freaking out
                    -- Lua's parser if the inner expression is a long string. Lua
                    -- parses x[[[y]]] as x("[y]"), not as x["y"]
                    Lua tree.source, "[ ",key_lua,"]=",value_lua
                else
                    Lua tree.source, "[",key_lua,"]=",value_lua
            
            when "IndexChain"
                lua = @tree_to_lua(tree.value[1])
                unless lua.is_value
                    error "Cannot index #{colored.yellow repr(tree.value[1])}, since it's not an expression.", 0
                first_char = tostring(lua)\sub(1,1)
                if first_char == "{" or first_char == '"' or first_char == "["
                    lua\parenthesize!

                for i=2,#tree.value
                    key = tree.value[i]
                    key_lua = @tree_to_lua(key)
                    unless key_lua.is_value
                        error "Cannot use #{colored.yellow repr(key)} as an index, since it's not an expression.", 0
                    key_lua_str = tostring(key_lua)
                    if lua_id = key_lua_str\match("^['\"]([a-zA-Z_][a-zA-Z0-9_]*)['\"]$")
                        lua\append ".#{lua_id}"
                    elseif key_lua_str\sub(1,1) == '['
                        -- NOTE: this *must* use a space after the [ to avoid freaking out
                        -- Lua's parser if the inner expression is a long string. Lua
                        -- parses x[[[y]]] as x("[y]"), not as x["y"]
                        lua\append "[ ",key_lua," ]"
                    else
                        lua\append "[",key_lua,"]"
                return lua

            when "Number"
                Lua.Value(tree.source, tostring(tree.value))

            when "Var"
                Lua.Value(tree.source, tree\as_lua_id!)
            
            else
                error("Unknown type: #{tree.type}")


    tree_to_nomsu: (tree, inline=false, can_use_colon=false)=>
        switch tree.type
            when "Action"
                if inline
                    nomsu = Nomsu(tree.source)
                    for i,bit in ipairs tree.value
                        if type(bit) == "string"
                            if i > 1
                                nomsu\append " "
                            nomsu\append bit
                        else
                            arg_nomsu = @tree_to_nomsu(bit,true)
                            return nil unless arg_nomsu
                            unless i == 1
                                nomsu\append " "
                            if bit.type == "Action" or bit.type == "Block"
                                arg_nomsu\parenthesize!
                            nomsu\append arg_nomsu
                    return nomsu
                else
                    nomsu = Nomsu(tree.source)
                    next_space = ""
                    -- TODO: track line length as we go and use 80-that instead of 80 for wrapping
                    last_colon = nil
                    for i,bit in ipairs tree.value
                        if type(bit) == "string"
                            nomsu\append next_space, bit
                            next_space = " "
                        else
                            arg_nomsu = if last_colon == i-1 and bit.type == "Action" then nil
                            elseif bit.type == "Block" then nil
                            else @tree_to_nomsu(bit,true)

                            if arg_nomsu and #arg_nomsu < MAX_LINE
                                if bit.type == "Action"
                                    if can_use_colon and i > 1
                                        nomsu\append next_space\match("[^ ]*"), ": ", arg_nomsu
                                        next_space = "\n.."
                                        last_colon = i
                                    else
                                        nomsu\append next_space, "(", arg_nomsu, ")"
                                        next_space = " "
                                else
                                    nomsu\append next_space, arg_nomsu
                                    next_space = " "
                            else
                                arg_nomsu = @tree_to_nomsu(bit, nil, true)
                                return nil unless nomsu
                                -- These types carry their own indentation
                                if bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
                                    if i == 1
                                        arg_nomsu = Nomsu(bit.source, "(..)\n    ", arg_nomsu)
                                    else
                                        arg_nomsu = Nomsu(bit.source, "\n    ", arg_nomsu)
                                
                                if last_colon == i-1 and (bit.type == "Action" or bit.type == "Block")
                                    next_space = ""
                                nomsu\append next_space, arg_nomsu
                                next_space = "\n.."

                            if next_space == " " and #(tostring(nomsu)\match("[^\n]*$")) > MAX_LINE
                                next_space = "\n.."
                    return nomsu

            when "EscapedNomsu"
                nomsu = @tree_to_nomsu(tree.value, true)
                if nomsu == nil and not inline
                    nomsu = @tree_to_nomsu(tree.value[1])
                    return nomsu and Nomsu tree.source, "\\:\n    ", nomsu
                return nomsu and Nomsu tree.source, "\\(", nomsu, ")"

            when "Block"
                if inline
                    nomsu = Nomsu(tree.source)
                    for i,line in ipairs tree.value
                        if i > 1
                            nomsu\append "; "
                        line_nomsu = @tree_to_nomsu(line,true)
                        return nil unless line_nomsu
                        nomsu\append line_nomsu
                    return nomsu
                nomsu = Nomsu(tree.source)
                for i, line in ipairs @
                    line = assert(@tree_to_nomsu(line, nil, true), "Could not convert line to nomsu")
                    nomsu\append line
                    if i < #@
                        nomsu\append "\n"
                        if tostring(line)\match("\n")
                            nomsu\append "\n"
                return nomsu

            when "Text"
                if inline
                    nomsu = Nomsu(tree.source, '"')
                    for bit in *tree.value
                        if type(bit) == 'string'
                            -- TODO: unescape better?
                            nomsu\append (bit\gsub("\\","\\\\")\gsub("\n","\\n"))
                        else
                            interp_nomsu = @tree_to_nomsu(bit, true)
                            if interp_nomsu
                                if bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
                                    interp_nomsu\parenthesize!
                                nomsu\append "\\", interp_nomsu
                            else return nil
                    nomsu\append '"'
                    return nomsu
                else
                    inline_version = @tree_to_nomsu(tree, true)
                    if inline_version and #inline_version <= MAX_LINE
                        return inline_version
                    nomsu = Nomsu(tree.source, '".."\n    ')
                    for i, bit in ipairs @
                        if type(bit) == 'string'
                            nomsu\append (bit\gsub("\\","\\\\")\gsub("\n","\n    "))
                        else
                            interp_nomsu = @tree_to_nomsu(bit, true)
                            if interp_nomsu
                                if bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
                                    interp_nomsu\parenthesize!
                                nomsu\append "\\", interp_nomsu
                            else
                                interp_nomsu = @tree_to_nomsu(bit)
                                return nil unless interp_nomsu
                                nomsu\append "\\\n        ", interp_nomsu
                                if i < #@
                                    nomsu\append "\n    .."
                    return nomsu

            when "List"
                if inline
                    nomsu = Nomsu(tree.source, "[")
                    for i, item in ipairs tree.value
                        item_nomsu = @tree_to_nomsu(item, true)
                        return nil unless item_nomsu
                        if i > 1
                            nomsu\append ", "
                        nomsu\append item_nomsu
                    nomsu\append "]"
                    return nomsu
                else
                    inline_version = @tree_to_nomsu(tree, true)
                    if inline_version and #inline_version <= MAX_LINE
                        return inline_version
                    nomsu = Nomsu(tree.source, "[..]")
                    line = Nomsu(tree.source, "\n    ")
                    for item in *tree.value
                        item_nomsu = @tree_to_nomsu(item, true)
                        if item_nomsu and #line + #", " + #item_nomsu <= MAX_LINE
                            if #line.bits > 1
                                line\append ", "
                            line\append item_nomsu
                        else
                            unless item_nomsu
                                item_nomsu = @tree_to_nomsu(item)
                                return nil unless item_nomsu
                            if #line.bits > 1
                                nomsu\append line
                                line = Nomsu(line.source, "\n    ")
                            line\append item_nomsu
                    if #line.bits > 1
                        nomsu\append line
                    return nomsu
            
            when "Dict"
                if inline
                    nomsu = Nomsu(tree.source, "{")
                    for i, entry in ipairs tree.value
                        entry_nomsu = @tree_to_nomsu(entry, true)
                        return nil unless entry_nomsu
                        if i > 1
                            nomsu\append ", "
                        nomsu\append entry_nomsu
                    nomsu\append "}"
                    return nomsu
                else
                    inline_version = @tree_to_nomsu(tree, true)
                    if inline_version then return inline_version
                    nomsu = Nomsu(tree.source, "{..}")
                    line = Nomsu(tree.source, "\n    ")
                    for entry in *tree.value
                        entry_nomsu = @tree_to_nomsu(entry)
                        return nil unless entry_nomsu
                        if #line + #tostring(entry_nomsu) <= MAX_LINE
                            if #line.bits > 1
                                line\append ", "
                            line\append entry_nomsu
                        else
                            if #line.bits > 1
                                nomsu\append line
                                line = Nomsu(line.source, "\n    ")
                            line\append entry_nomsu
                    if #line.bits > 1
                        nomsu\append line
                    return nomsu
            
            when "DictEntry"
                key, value = tree.value[1], tree.value[2]
                key_nomsu = @tree_to_nomsu(key, true)
                return nil unless key_nomsu
                if key.type == "Action" or key.type == "Block"
                    key_nomsu\parenthesize!
                value_nomsu = if value
                    @tree_to_nomsu(value, true)
                else Nomsu(tree.source, "")
                if inline and not value_nomsu then return nil
                if not value_nomsu
                    return nil if inline
                    value_nomsu = @tree_to_nomsu(value)
                    return nil unless value_nomsu
                return Nomsu tree.source, key_nomsu, ":", value_nomsu
            
            when "IndexChain"
                nomsu = Nomsu(tree.source)
                for i, bit in ipairs tree.value
                    if i > 1
                        nomsu\append "."
                    bit_nomsu = @tree_to_nomsu(bit, true)
                    return nil unless bit_nomsu
                    if bit.type == "Action" or bit.type == "Block"
                        bit_nomsu\parenthesize!
                    nomsu\append bit_nomsu
                return nomsu
            
            when "Number"
                return Nomsu(tree.source, tostring(tree.value))

            when "Var"
                return Nomsu(tree.source, "%", tree.value)

            when "Comment"
                return nil if inline
                return Nomsu(tree.source, "#", tree.value\gsub("\n", "\n    "))
            
            else
                error("Unknown type: #{tree.type}")
    
    tree_to_value: (tree)=>
        -- Special case for text literals
        if tree.type == 'Text' and #tree == 1 and type(tree[1]) == 'string'
            return tree[1]
        lua = Lua(tree.source, "return ",@tree_to_lua(tree),";")
        return @run_lua(lua)

    walk_tree: (tree, depth=0)=>
        coroutine.yield(tree, depth)
        if tree.is_multi
            for v in *tree.value
                if Types.is_node(v)
                    @walk_tree(v, depth+1)

    initialize_core: =>
        -- Sets up some core functionality
        nomsu = self
        @define_compile_action "immediately %block", (_block)=>
            lua = nomsu\tree_to_lua(_block)\as_statements!
            lua\declare_locals!
            nomsu\run_lua(lua)
            return Lua(_block.source, "if IMMEDIATE then\n    ", lua, "\nend")

        add_lua_string_bits = (lua, code)->
            if code.type != "Text"
                lua\append ", ", nomsu\tree_to_lua(code)
                return
            for bit in *code.value
                lua\append ", "
                if type(bit) == "string"
                    lua\append repr(bit)
                else
                    bit_lua = nomsu\tree_to_lua(bit)
                    unless bit_lua.is_value
                        error "Cannot use #{colored.yellow repr(bit)} as a string interpolation value, since it's not an expression."
                    lua\append bit_lua

        @define_compile_action "Lua %code", (_code)=>
            lua = Lua.Value(_code.source, "Lua(", repr(_code.source))
            add_lua_string_bits(lua, _code)
            lua\append ")"
            return lua

        @define_compile_action "Lua value %code", (_code)=>
            lua = Lua.Value(_code.source, "Lua.Value(", repr(_code.source))
            add_lua_string_bits(lua, _code)
            lua\append ")"
            return lua

        add_lua_bits = (lua, code)->
            for bit in *code.value
                if type(bit) == "string"
                    lua\append bit
                else
                    bit_lua = nomsu\tree_to_lua(bit)
                    unless bit_lua.is_value
                        error "Cannot use #{colored.yellow repr(bit)} as a string interpolation value, since it's not an expression.", 0
                    lua\append bit_lua
            return lua

        @define_compile_action "lua> %code", (_code)=>
            if _code.type != "Text"
                return Lua @source, "nomsu:run_lua(", nomsu\tree_to_lua(_code), ");"
            return add_lua_bits(Lua(@source), _code)

        @define_compile_action "=lua %code", (_code)=>
            if _code.type != "Text"
                return Lua.Value @source, "nomsu:run_lua(", nomsu\tree_to_lua(_code), ":as_statements('return '))"
            return add_lua_bits(Lua.Value(@source), _code)

        @define_compile_action "use %path", (_path)=>
            path = nomsu\tree_to_value(_path)
            nomsu\run_file(path)
            return Lua(_path.source, "nomsu:run_file(#{repr path});")

-- Only run this code if this file was run directly with command line arguments, and not require()'d:
if arg and debug_getinfo(2).func != require
    export colors
    colors = require 'consolecolors'
    parser = re.compile([[
        args <- {| (flag ";")* {:inputs: {| ({file} ";")* |} :} {:nomsu_args: {| ("--;" ({[^;]*} ";")*)? |} :} ";"? |} !.
        flag <-
            {:interactive: ("-i" -> true) :}
          / {:optimized: ("-O" -> true) :}
          / {:format: ("-f" -> true) :}
          / {:syntax: ("-s" -> true) :}
          / {:print_file: "-p" ";" {file} :}
          / {:output_file: "-o" ";" {file} :}
          / {:help: (("-h" / "--help") -> true) :}
        file <- "-" / [^;]+
    ]], {true: -> true})
    args = concat(arg, ";")..";"
    args = parser\match(args)
    if not args or args.help
        print [=[
Nomsu Compiler

Usage: (lua nomsu.lua | moon nomsu.moon) [-i] [-O] [-f] [-s] [--help] [-o output] [-p print_file] file1 file2... [-- nomsu args...]

OPTIONS
    -i Run the compiler in interactive mode (REPL)
    -O Run the compiler in optimized mode (use precompiled .lua versions of Nomsu files, when available)
    -f Auto-format the given Nomsu file and print the result.
    -s Check the program for syntax errors.
    -h/--help Print this message.
    -o <file> Output the compiled Lua file to the given file (use "-" to output to stdout; if outputting to stdout and -p is not specified, -p will default to /dev/null)
    -p <file> Print to the specified file instead of stdout.
    <input> Input file can be "-" to use stdin.
]=]
        os.exit!

    nomsu = NomsuCompiler!
    nomsu.environment.arg = args.nomsu_args

    ok, to_lua = pcall -> require('moonscript.base').to_lua
    if not ok then to_lua = nil
    moonscript_line_tables = setmetatable {}, {
        __index: (filename)=>
            return nil unless to_lua
            _, line_table = to_lua(FILE_CACHE[filename])
            self[filename] = line_table
            return line_table
    }

    debug.getinfo = (thread,f,what)->
        if what == nil
            f,what,thread = thread,f,nil
        if type(f) == 'number' then f += 1 -- Account for this wrapper function
        info = if thread == nil
            debug_getinfo(f,what)
        else debug_getinfo(thread,f,what)
        if not info or not info.func then return info
        if info.short_src or info.source or info.linedefine or info.currentline
            for k,v in pairs(nomsu.environment.ACTIONS)
                if v == info.func
                    info.name = k
                    break
            [=[
            if metadata = nomsu.action_metadata[info.func]
                info.name = metadata.aliases[1]
                filename = if type(metadata.source) == 'string'
                    metadata.source\match("^[^[:]*")
                else metadata.source.filename
                info.short_src = filename
                info.source = FILE_CACHE[filename]
                ok, linedefined = pcall(lua_line_to_nomsu_line, info.short_src, info.linedefined)
                if ok then info.linedefined = linedefined
                ok, currentline = pcall(lua_line_to_nomsu_line, info.short_src, info.currentline)
                --if ok then info.currentline = currentline
                ]=]
        return info

    print_err_msg = (error_message, stack_offset=2)->
        io.stderr\write("#{colored.red "ERROR:"} #{colored.bright colored.red (error_message or "")}\n")
        io.stderr\write("stack traceback:\n")

        -- TODO: properly print out the calling site of nomsu code, not just the *called* code
        ok, to_lua = pcall -> require('moonscript.base').to_lua
        if not ok then to_lua = -> nil
        nomsu_source = FILE_CACHE["nomsu.moon"]
        _, line_table = to_lua(nomsu_source)

        level = stack_offset
        while true
            -- TODO: reduce duplicate code
            calling_fn = debug_getinfo(level)
            if not calling_fn then break
            if calling_fn.func == run then break
            level += 1
            name = calling_fn.name
            if name == "run_lua_fn" then continue
            line = nil
            [=[
            if metadata = nomsu.action_metadata[calling_fn.func]
                filename, start, stop = metadata.source\match("([^:]*):([0-9]*),([0-9]*)")
                if filename
                    file = FILE_CACHE[filename]
                    line_no = 1
                    for _ in file\sub(1,tonumber(start))\gmatch("\n") do line_no += 1
                    offending_statement = file\sub(tonumber(start),tonumber(stop))
                    if #offending_statement > 50
                        offending_statement = offending_statement\sub(1,50).."..."
                    offending_statement = colored.red(offending_statement)
                    line = colored.yellow(filename..":"..tostring(line_no).."\n    "..offending_statement)
                else
                    line = colored.yellow(metadata.source)
                name = colored.bright(colored.yellow(metadata.aliases[1]))
            else
                if calling_fn.istailcall and not name
                    name = "<tail call>"
                if calling_fn.short_src == "./nomsu.moon" and line_table
                    char = line_table[calling_fn.currentline]
                    line_num = 1
                    for _ in nomsu_source\sub(1,char)\gmatch("\n") do line_num += 1
                    line = colored.cyan("#{calling_fn.short_src}:#{line_num}")
                    name = colored.bright(colored.cyan(name or "???"))
                else
                    line = colored.blue("#{calling_fn.short_src}:#{calling_fn.currentline}")
                    name = colored.bright(colored.blue(name or "???"))
            _from = colored.dim colored.white "|"
            io.stderr\write(("%32s %s %s\n")\format(name, _from, line))
            ]=]
        io.stderr\flush!
    
    run = ->
    
        for i,input in ipairs args.inputs
            if input == "-" then args.inputs[i] = STDIN

        if #args.inputs == 0 and not args.interactive
            args.inputs = {"core"}
            args.interactive = true

        output_file = if args.output_file == "-" then io.stdout
        elseif args.output_file then io.open(args.output_file, 'w')

        print_file = if args.print_file == "-" then io.stdout
        elseif args.print_file then io.open(args.print_file, 'w')
        elseif output_file == io.stdout then nil
        else io.stdout

        nomsu.skip_precompiled = not args.optimized
        if print_file == nil
            nomsu.environment.print = ->
        elseif print_file != io.stdout
            nomsu.environment.print = (...)->
                N = select("#",...)
                if N > 0
                    print_file\write(tostring(select(1,...)))
                    for i=2,N
                        print_file\write('\t',tostring(select(1,...)))
                print_file\write('\n')
                print_file\flush!

        compile_fn = if output_file
            (code)->
                output_file\write("local IMMEDIATE = true;\n"..tostring(code))
                output_file\flush!
        else nil

        parse_errs = {}
        for input in *args.inputs
            if args.syntax
                -- Check syntax:
                for input_file in all_files(input)
                    ok,err = pcall nomsu.parse, nomsu, Nomsu(input_file, io.open(input_file)\read("*a"))
                    if not ok
                        insert parse_errs, err
                    elseif print_file
                        print_file\write("Parse succeeded: #{input_file}\n")
                        print_file\flush!
            elseif args.format
                -- Auto-format
                for input_file in all_files(input)
                    tree = nomsu\parse(io.open(input_file)\read("*a"))
                    formatted = tostring(@tree_to_nomsu(tree))
                    if output_file
                        output_file\write(formatted, "\n")
                        output_file\flush!
                    if print_file
                        print_file\write(formatted, "\n")
                        print_file\flush!
            elseif input == STDIN
                nomsu\run(io.input!\read("*a"), compile_fn)
            else
                nomsu\run_file(input, compile_fn)

        if #parse_errs > 0
            io.stderr\write concat(parse_errs, "\n\n")
            io.stderr\flush!
            os.exit(false, true)
        elseif args.syntax
            os.exit(true, true)

        if args.interactive
            -- REPL
            while true
                io.write(colored.bright colored.yellow ">> ")
                buff = ""
                while true
                    line = io.read("*L")
                    if line == "\n" or not line
                        if #buff > 0
                            io.write("\027[1A\027[2K")
                        break -- Run buffer
                    line = line\gsub("\t", "    ")
                    buff ..= line
                    io.write(colored.dim colored.yellow ".. ")
                if #buff == 0
                    break -- Exit
                ok, ret = pcall(nomsu.run, nomsu, buff)
                if ok and ret != nil
                    print "= "..repr(ret)
                elseif not ok
                    print_err_msg ret
    
    err_hand = (error_message)->
        print_err_msg error_message
        os.exit(false, true)

    -- Note: xpcall has a slightly different API in Lua <=5.1 vs. >=5.2, but this works
    -- for both APIs
    -- TODO: revert back to old error handler

    --require('ProFi')\profile "scratch/profile.txt", (profi)->
    do
        ok, ldt = pcall(require,'ldt')
        if ok
            ldt.guard run
        else xpcall(run, err_hand)

return NomsuCompiler
