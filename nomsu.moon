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
    package.path = "./luajit_lpeg/?.lua;"..package.path
    
    export bit32
    bit32 = require('bit')

lpeg = require 'lpeg'
re = require 're'
lpeg.setmaxstack 10000
{:P,:R,:V,:S,:Cg,:C,:Cp,:B,:Cmt,:Carg} = lpeg
utils = require 'utils'
new_uuid = require 'uuid'
{:repr, :stringify, :min, :max, :equivalent, :set, :is_list, :sum} = utils
colors = setmetatable({}, {__index:->""})
export colored
colored = setmetatable({}, {__index:(_,color)-> ((msg)-> colors[color]..tostring(msg or '')..colors.reset)})
{:insert, :remove, :concat} = table
{:match, :sub, :rep, :gsub, :format, :byte, :match, :find} = string
debug_getinfo = debug.getinfo
{:Nomsu, :Lua, :Source} = require "code_obj"
STDIN, STDOUT, STDERR = "/dev/fd/0", "/dev/fd/1", "/dev/fd/2"

string.as_lua_id = (str)->
    argnum = 0
    -- Cut up escape-sequence-like chunks
    str = gsub str, "x([0-9A-F][0-9A-F])", "x\0%1"
    -- Alphanumeric unchanged, spaces to underscores, and everything else to hex escape sequences
    str = gsub str, "%W", (c)->
        if c == ' ' then '_'
        elseif c == '%' then
            argnum += 1
            tostring(argnum)
        else format("x%02X", byte(c))
    return '_'..str

table.map = (fn)=> [fn(v) for _,v in ipairs(@)]

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
    if match(path, "%.nom$") or match(path, "%.lua$") or match(path, "^/dev/fd/[012]$")
        return iterate_single, path
    -- TODO: improve sanitization
    path = gsub(path,"\\","\\\\")
    path = gsub(path,"`","")
    path = gsub(path,'"','\\"')
    path = gsub(path,"$","")
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
get_lines = re.compile([[
    lines <- {| line (%nl line)* |}
    line <- {[^%nl]*}
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
pos_to_line = (str, pos)->
    line_starts = LINE_STARTS[str]
    -- Binary search for line number of position
    lo, hi = 1, #line_starts
    while lo <= hi
        mid = math.floor((lo+hi)/2)
        if line_starts[mid] > pos
            hi = mid-1
        else lo = mid+1
    return hi

-- Use + operator for string coercive concatenation (note: "asdf" + 3 == "asdf3")
-- Use [] for accessing string characters, or s[{3,4}] for s:sub(3,4)
-- Note: This globally affects all strings in this instance of Lua!
do
    STRING_METATABLE = getmetatable("")
    STRING_METATABLE.__add = (other)=> @ .. stringify(other)
    STRING_METATABLE.__index = (i)=>
        ret = string[i]
        if ret != nil then return ret
        if type(i) == 'number' then return sub(@, i, i)
        elseif type(i) == 'table' then return sub(@, i[1], i[2])

AST = require "nomsu_tree"

NOMSU_DEFS = with {}
    -- Newline supports either windows-style CR+LF or unix-style LF
    .nl = P("\r")^-1 * P("\n")
    .ws = S(" \t")
    .tonumber = tonumber
    string_escapes = n:"\n", t:"\t", b:"\b", a:"\a", v:"\v", f:"\f", r:"\r"
    digit, hex = R('09'), R('09','af','AF')
    .escaped_char = (P("\\")*S("xX")*C(hex*hex)) / => string.char(tonumber(@, 16))
    .escaped_char += (P("\\")*C(digit*(digit^-2))) / => string.char(tonumber @)
    .escaped_char += (P("\\")*C(S("ntbavfr"))) / string_escapes
    .operator_char = S("'~`!@$^&*-+=|<>?/")
    .utf8_char = (
        R("\194\223")*R("\128\191") +
        R("\224\239")*R("\128\191")*R("\128\191") +
        R("\240\244")*R("\128\191")*R("\128\191")*R("\128\191"))
    .ident_char = R("az","AZ","09") + P("_") + .utf8_char

    -- If the line begins with #indent+4 spaces, the pattern matches *those* spaces
    -- and adds them to the current indent (not any more).
    .indent = Cmt Carg(1), (start, userdata)=>
        indented = userdata.indent..'    '
        if sub(@, start, start+#indented-1) == indented
            userdata.indent = indented
            return start + #indented
    -- If the number of leading space characters is <= the number of spaces in the current
    -- indent minus 4, this pattern matches and decrements the current indent exactly once.
    .dedent = Cmt Carg(1), (start, userdata)=>
        dedented = sub(userdata.indent, 1, -5)
        if #match(@, "^[ ]*", start) <= #dedented
            userdata.indent = dedented
            return start
    -- If the number of leading space characters is >= the number of spaces in the current
    -- indent, this pattern matches and does not modify the indent.
    .nodent = Cmt Carg(1), (start, userdata)=>
        if sub(@, start, start+#userdata.indent-1) == userdata.indent
            return start + #userdata.indent

    .userdata = Carg(1)

    .error = (src,end_pos,start_pos,err_msg,userdata)->
        seen_errors = userdata.errors
        if seen_errors[start_pos]
            return true
        if utils.size(seen_errors) >= 10
            seen_errors[start_pos+1] = colored.bright colored.yellow colored.onred "Too many errors, canceling parsing..."
            return #src+1
        err_pos = start_pos
        line_no = pos_to_line(src, err_pos)
        src = FILE_CACHE[userdata.source.filename]
        line_starts = LINE_STARTS[src]
        prev_line = line_no == 1 and "" or src\sub(line_starts[line_no-1] or 1, line_starts[line_no]-2)
        err_line = src\sub(line_starts[line_no], (line_starts[line_no+1] or 0)-2)
        next_line = src\sub(line_starts[line_no+1] or -1, (line_starts[line_no+2] or 0)-2)
        i = err_pos-line_starts[line_no]
        pointer = ("-")\rep(i) .. "^"
        err_msg = colored.bright colored.yellow colored.onred (err_msg or "Parse error").." at #{userdata.source.filename}:#{line_no}:"
        if #prev_line > 0 then err_msg ..= "\n"..colored.dim(prev_line)
        err_line = colored.white(err_line\sub(1, i))..colored.bright(colored.red(err_line\sub(i+1,i+1)))..colored.dim(err_line\sub(i+2,-1))
        err_msg ..= "\n#{err_line}\n#{colored.red pointer}"
        if #next_line > 0 then err_msg ..= "\n"..colored.dim(next_line)
        --error(err_msg)
        seen_errors[start_pos] = err_msg
        return true

setmetatable(NOMSU_DEFS, {__index:(key)=>
    make_node = (start, value, stop, userdata)->
        local source
        with userdata.source
            source = Source(.filename, .start + start-1, .start + stop-1)
        value.source = source
        setmetatable(value, AST[key])
        if value.__init then value\__init!
        for i=1,#value do assert(value[i])
        return value

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
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- (({} %3 {} %%userdata) -> %2)"
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    comment <- "--" [^%nl]*
    ]]
    nomsu_peg = peg_tidier\match(FILE_CACHE["nomsu.peg"])
    re.compile(nomsu_peg, NOMSU_DEFS)

class NomsuCompiler
    compile_error = (tok, err_format_string, ...)->
        file = FILE_CACHE[tok.source.filename]
        line_no = pos_to_line(file, tok.source.start)
        line_start = LINE_STARTS[file][line_no]
        src = colored.dim(file\sub(line_start, tok.source.start-1))
        src ..= colored.underscore colored.bright colored.red(file\sub(tok.source.start, tok.source.stop-1))
        end_of_line = (LINE_STARTS[file][pos_to_line(file, tok.source.stop) + 1] or 0) - 1
        src ..= colored.dim(file\sub(tok.source.stop, end_of_line-1))
        src = '    '..src\gsub('\n', '\n    ')
        err_msg = err_format_string\format(src, ...)
        error("#{tok.source.filename}:#{line_no}: "..err_msg, 0)
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
        -- Mapping from source string (e.g. "@core/metaprogramming.nom[1:100]") to a mapping
        -- from lua line number to nomsu line number
        @source_map = {}

        _list_mt =
            __eq:utils.equivalent
            -- Could consider adding a __newindex to enforce list-ness, but would hurt performance
            __tostring: =>
                "["..concat([repr(b) for b in *@], ", ").."]"
        list = (t)-> setmetatable(t, _list_mt)
        _dict_mt =
            __eq:utils.equivalent
            __tostring: =>
                "{"..concat(["#{repr(k)}: #{repr(v)}" for k,v in pairs @], ", ").."}"
        dict = (t)-> setmetatable(t, _dict_mt)
        @environment = {
            -- Discretionary/convenience stuff
            nomsu:self, repr:repr, stringify:stringify, utils:utils, lpeg:lpeg, re:re,
            :compile_error
            -- Lua stuff:
            :next, :unpack, :setmetatable, :coroutine, :rawequal, :getmetatable, :pcall,
            :error, :package, :os, :require, :tonumber, :tostring, :string, :xpcall, :module,
            :print, :loadfile, :rawset, :_VERSION, :collectgarbage, :rawget, :bit32, :rawlen,
            :table, :assert, :dofile, :loadstring, :type, :select, :debug, :math, :io, :load,
            :pairs, :ipairs,
            -- Nomsu types:
            :list, :dict,
        }
        for k,v in pairs(AST) do @environment[k] = v
        @environment.Lua = Lua
        @environment.Nomsu = Nomsu
        @environment.Source = Source
        @environment.ARG_ORDERS = setmetatable({}, {__mode:"k"})
        @environment.ALIASES = setmetatable({}, {__mode:"k"})
        @environment.compile_time = (fn)->
            @environment.COMPILE_TIME[fn] = true
            return fn
        @environment.COMPILE_TIME = {}
        @environment.LOADED = {}
        @environment.AST = AST
        @environment._ENV = @environment
        setmetatable @environment,
            __index: (k)=>
                if _self = rawget(@, "self")
                    return _self[k]
        @initialize_core!
    
    parse: (nomsu_code)=>
        assert(type(nomsu_code) != 'string')
        userdata = {
            source_code:nomsu_code, indent: "", errors: {},
            source: nomsu_code.source,
        }
        tree = NOMSU_PATTERN\match(tostring(nomsu_code), nil, userdata)
        unless tree
            error "In file #{colored.blue filename} failed to parse:\n#{colored.onyellow colored.black nomsu_code}"

        if next(userdata.errors)
            keys = utils.keys(userdata.errors)
            table.sort(keys)
            errors = [userdata.errors[k] for k in *keys]
            io.stderr\write(concat(errors, "\n\n").."\n")
            os.exit!
        
        return tree

    run: (nomsu_code, compile_fn=nil)=>
        tree = assert(@parse(nomsu_code))
        if type(tree) == 'number' -- Happens if pattern matches, but there are no captures, e.g. an empty string
            return nil
        lua = @tree_to_lua(tree)\as_statements!
        lua\declare_locals!
        lua\prepend "-- File: #{nomsu_code.source or ""}\n"
        if compile_fn
            compile_fn(lua)
        return @run_lua(lua)

    _running_files = {} -- For detecting circular imports
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
            if match(filename, "%.lua$")
                file = assert(FILE_CACHE[filename], "Could not find file: #{filename}")
                ret = @run_lua(Lua(Source(filename, 1, #file), file))
            elseif match(filename, "%.nom$") or match(filename, "^/dev/fd/[012]$")
                if not @skip_precompiled -- Look for precompiled version
                    lua_filename = gsub(filename, "%.nom$", ".lua")
                    file = FILE_CACHE[lua_filename]
                    if file
                        ret = @run_lua(Lua(Source(filename, 1, #file), file))
                        remove _running_files
                        continue
                file = file or FILE_CACHE[filename]
                if not file
                    error("File does not exist: #{filename}", 0)
                ret = @run(Nomsu(Source(filename,1,#file), file), compile_fn)
            else
                error("Invalid filetype for #{filename}", 0)
            loaded[filename] = ret or true
            remove _running_files

        loaded[filename] = ret or true
        return ret

    run_lua: (lua)=>
        assert(type(lua) != 'string', "Attempt to run lua string instead of Lua (object)")
        lua_string = tostring(lua)
        run_lua_fn, err = load(lua_string, nil and tostring(lua.source), "t", @environment)
        if not run_lua_fn
            n = 1
            fn = ->
                n = n + 1
                ("\n%-3d|")\format(n)
            line_numbered_lua = "1  |"..lua_string\gsub("\n", fn)
            error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack line_numbered_lua}\n\n#{err}", 0)
        source_key = tostring(lua.source)
        unless @source_map[source_key]
            map = {}
            offset = 1
            source = lua.source
            nomsu_str = tostring(FILE_CACHE[source.filename]\sub(source.start, source.stop))
            lua_line = 1
            nomsu_line = pos_to_line(nomsu_str, lua.source.start)
            fn = (s)->
                if type(s) == 'string'
                    for nl in s\gmatch("\n")
                        map[lua_line] or= nomsu_line
                        lua_line += 1
                else
                    old_line = nomsu_line
                    if s.source
                        nomsu_line = pos_to_line(nomsu_str, s.source.start)
                    for b in *s.bits do fn(b)
            fn(lua)
            map[lua_line] or= nomsu_line
            map[0] = 0
            -- Mapping from lua line number to nomsu line numbers
            @source_map[source_key] = map

        return run_lua_fn!

    MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value
    math_expression = re.compile [[ ([+-] " ")* "%" (" " [*/^+-] (" " [+-])* " %")+ !. ]]
    tree_to_lua: (tree)=>
        switch tree.type
            when "Action"
                stub = tree.stub
                action = @environment['A'..string.as_lua_id(stub)]
                if action and @environment.COMPILE_TIME[action]
                    args = [arg for arg in *tree when type(arg) != "string"]
                    -- Force all compile-time actions to take a tree location
                    if arg_orders = @environment.ARG_ORDERS[stub]
                        args = [args[p] for p in *arg_orders]
                    -- Force Lua to avoid tail call optimization for debugging purposes
                    -- TODO: use tail call
                    ret = action(tree, unpack(args))
                    if not ret
                        compile_error tree,
                            "Compile-time action:\n%s\nfailed to produce any Lua"
                    return ret
                lua = Lua.Value(tree.source)
                if not action and math_expression\match(stub)
                    -- This is a bit of a hack, but this code handles arbitrarily complex
                    -- math expressions like 2*x + 3^2 without having to define a single
                    -- action for every possibility.
                    for i,tok in ipairs tree
                        if type(tok) == 'string'
                            lua\append tok
                        else
                            tok_lua = @tree_to_lua(tok)
                            unless tok_lua.is_value
                                compile_error tok,
                                    "Non-expression value inside math expression:\n%s"
                            if tok.type == "Action"
                                tok_lua\parenthesize!
                            lua\append tok_lua
                        if i < #tree
                            lua\append " "
                    return lua

                args = {}
                for i, tok in ipairs tree
                    if type(tok) == "string" then continue
                    arg_lua = @tree_to_lua(tok)
                    unless arg_lua.is_value
                        compile_error tok,
                            "Cannot use:\n%s\nas an argument to %s, since it's not an expression, it produces: %s",
                            stub, repr arg_lua
                    insert args, arg_lua

                if action
                    if arg_orders = @environment.ARG_ORDERS[stub]
                        args = [args[p] for p in *arg_orders]

                lua\append "A",string.as_lua_id(stub),"("
                for i, arg in ipairs args
                    lua\append arg
                    if i < #args then lua\append ", "
                lua\append ")"
                return lua

            when "EscapedNomsu"
                make_tree = (t)->
                    unless AST.is_syntax_tree(t)
                        return repr(t)
                    bits = [make_tree(bit) for bit in *t]
                    return t.type.."("..repr(tostring t.source)..", "..table.concat(bits, ", ")..")"
                Lua.Value tree.source, make_tree(tree[1])
            
            when "Block"
                lua = Lua(tree.source)
                for i,line in ipairs tree
                    line_lua = @tree_to_lua(line)
                    if i > 1
                        lua\append "\n"
                    lua\append line_lua\as_statements!
                return lua

            when "Text"
                lua = Lua.Value(tree.source)
                string_buffer = ""
                for i, bit in ipairs tree
                    if type(bit) == "string"
                        string_buffer ..= bit
                        continue
                    if string_buffer ~= ""
                        if #lua.bits > 0 then lua\append ".."
                        lua\append repr(string_buffer)
                        string_buffer = ""
                    bit_lua = @tree_to_lua(bit)
                    unless bit_lua.is_value
                        src = '    '..gsub(tostring(@tree_to_nomsu(bit)), '\n','\n    ')
                        line = "#{bit.source.filename}:#{pos_to_line(FILE_CACHE[bit.source.filename], bit.source.start)}"
                        compile_error bit,
                            "Cannot use:\n%s\nas a string interpolation value, since it's not an expression."
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
                lua = Lua.Value tree.source, "list{"
                line_length = 0
                for i, item in ipairs tree
                    item_lua = @tree_to_lua(item)
                    unless item_lua.is_value
                        compile_error item,
                            "Cannot use:\n%s\nas a list item, since it's not an expression."
                    lua\append item_lua
                    item_string = tostring(item_lua)
                    last_line = match(item_string, "[^\n]*$")
                    if match(item_string, "\n")
                        line_length = #last_line
                    else
                        line_length += #last_line
                    if i < #tree
                        if line_length >= MAX_LINE
                            lua\append ",\n  "
                            line_length = 0
                        else
                            lua\append ", "
                            line_length += 2
                lua\append "}"
                return lua

            when "Dict"
                lua = Lua.Value tree.source, "dict{"
                line_length = 0
                for i, entry in ipairs tree
                    entry_lua = @tree_to_lua(entry)
                    lua\append entry_lua
                    entry_lua_str = tostring(entry_lua)
                    -- TODO: maybe make this more accurate? It's only a heuristic, so eh...
                    last_line = match(entry_lua_str, "\n([^\n]*)$")
                    if last_line
                        line_length = #last_line
                    else
                        line_length += #entry_lua_str
                    if i < #tree
                        if line_length >= MAX_LINE
                            lua\append ",\n  "
                            line_length = 0
                        else
                            lua\append ", "
                            line_length += 2
                lua\append "}"
                return lua

            when "DictEntry"
                key, value = tree[1], tree[2]
                key_lua = @tree_to_lua(key)
                unless key_lua.is_value
                    compile_error tree[1],
                        "Cannot use:\n%s\nas a dict key, since it's not an expression."
                value_lua = value and @tree_to_lua(value) or Lua.Value(key.source, "true")
                unless value_lua.is_value
                    compile_error tree[2],
                        "Cannot use:\n%s\nas a dict value, since it's not an expression."
                key_str = match(tostring(key_lua), [=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
                return if key_str
                    Lua tree.source, key_str,"=",value_lua
                elseif sub(tostring(key_lua),1,1) == "["
                    -- NOTE: this *must* use a space after the [ to avoid freaking out
                    -- Lua's parser if the inner expression is a long string. Lua
                    -- parses x[[[y]]] as x("[y]"), not as x["y"]
                    Lua tree.source, "[ ",key_lua,"]=",value_lua
                else
                    Lua tree.source, "[",key_lua,"]=",value_lua
            
            when "IndexChain"
                lua = @tree_to_lua(tree[1])
                unless lua.is_value
                    compile_error tree[1],
                        "Cannot index:\n%s\nsince it's not an expression."
                first_char = sub(tostring(lua),1,1)
                if first_char == "{" or first_char == '"' or first_char == "["
                    lua\parenthesize!

                for i=2,#tree
                    key = tree[i]
                    key_lua = @tree_to_lua(key)
                    unless key_lua.is_value
                        compile_error key,
                            "Cannot use:\n%s\nas an index, since it's not an expression."
                    key_lua_str = tostring(key_lua)
                    if lua_id = match(key_lua_str, "^['\"]([a-zA-Z_][a-zA-Z0-9_]*)['\"]$")
                        lua\append ".#{lua_id}"
                    elseif sub(key_lua_str,1,1) == '['
                        -- NOTE: this *must* use a space after the [ to avoid freaking out
                        -- Lua's parser if the inner expression is a long string. Lua
                        -- parses x[[[y]]] as x("[y]"), not as x["y"]
                        lua\append "[ ",key_lua," ]"
                    else
                        lua\append "[",key_lua,"]"
                return lua

            when "Number"
                Lua.Value(tree.source, tostring(tree[1]))

            when "Var"
                Lua.Value(tree.source, string.as_lua_id(tree[1]))
            
            else
                error("Unknown type: #{tree.type}")

    tree_to_nomsu: (tree, inline=false, can_use_colon=false)=>
        switch tree.type
            when "Action"
                if inline
                    nomsu = Nomsu(tree.source)
                    for i,bit in ipairs tree
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
                    line_len, last_colon = 0, nil
                    for i,bit in ipairs tree
                        if type(bit) == "string"
                            line_len += #next_space + #bit
                            nomsu\append next_space, bit
                            next_space = " "
                        else
                            arg_nomsu = if last_colon == i-1 and bit.type == "Action" then nil
                            elseif bit.type == "Block" then nil
                            else @tree_to_nomsu(bit,true)

                            if arg_nomsu and line_len + #tostring(arg_nomsu) < MAX_LINE
                                if bit.type == "Action"
                                    if can_use_colon and i > 1
                                        nomsu\append match(next_space,"[^ ]*"), ": ", arg_nomsu
                                        next_space = "\n.."
                                        line_len = 2
                                        last_colon = i
                                    else
                                        nomsu\append next_space, "(", arg_nomsu, ")"
                                        line_len += #next_space + 2 + #tostring(arg_nomsu)
                                        next_space = " "
                                else
                                    nomsu\append next_space, arg_nomsu
                                    line_len += #next_space + #tostring(arg_nomsu)
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
                                line_len = 2

                            if next_space == " " and #(match(tostring(nomsu),"[^\n]*$")) > MAX_LINE
                                next_space = "\n.."
                    return nomsu

            when "EscapedNomsu"
                nomsu = @tree_to_nomsu(tree[1], true)
                if nomsu == nil and not inline
                    nomsu = @tree_to_nomsu(tree[1])
                    return nomsu and Nomsu tree.source, "\\:\n    ", nomsu
                return nomsu and Nomsu tree.source, "\\(", nomsu, ")"

            when "Block"
                if inline
                    nomsu = Nomsu(tree.source)
                    for i,line in ipairs tree
                        if i > 1
                            nomsu\append "; "
                        line_nomsu = @tree_to_nomsu(line,true)
                        return nil unless line_nomsu
                        nomsu\append line_nomsu
                    return nomsu
                nomsu = Nomsu(tree.source)
                for i, line in ipairs tree
                    line = assert(@tree_to_nomsu(line, nil, true), "Could not convert line to nomsu")
                    nomsu\append line
                    if i < #tree
                        nomsu\append "\n"
                        if match(tostring(line), "\n")
                            nomsu\append "\n"
                return nomsu

            when "Text"
                if inline
                    nomsu = Nomsu(tree.source, '"')
                    for bit in *tree
                        if type(bit) == 'string'
                            -- TODO: unescape better?
                            nomsu\append (gsub(gsub(gsub(bit,"\\","\\\\"),"\n","\\n"),'"','\\"'))
                        else
                            interp_nomsu = @tree_to_nomsu(bit, true)
                            if interp_nomsu
                                if bit.type != "Var" and bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
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
                    for i, bit in ipairs tree
                        if type(bit) == 'string'
                            bit_lines = get_lines\match(bit)
                            for j, line in ipairs bit_lines
                                if j > 1 then nomsu\append "\n    "
                                if #line > 1.25*MAX_LINE
                                    remainder = line
                                    while #remainder > 0
                                        split = find(remainder, " ", MAX_LINE, true)
                                        if split
                                            chunk, remainder = sub(remainder, 1, split), sub(remainder, split+1, -1)
                                            nomsu\append chunk
                                        elseif #remainder > 1.75*MAX_LINE
                                            split = math.floor(1.5*MAX_LINE)
                                            chunk, remainder = sub(remainder, 1, split), sub(remainder, split+1, -1)
                                            nomsu\append chunk
                                        else
                                            nomsu\append remainder
                                            break
                                        if #remainder > 0 then nomsu\append "\\\n    .."
                                else
                                    nomsu\append line
                        else
                            interp_nomsu = @tree_to_nomsu(bit, true)
                            if interp_nomsu
                                if bit.type != "Var" and bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
                                    interp_nomsu\parenthesize!
                                nomsu\append "\\", interp_nomsu
                            else
                                interp_nomsu = assert(@tree_to_nomsu(bit))
                                return nil unless interp_nomsu
                                nomsu\append "\\\n        ", interp_nomsu
                                if i < #tree
                                    nomsu\append "\n    .."
                    return nomsu

            when "List"
                if inline
                    nomsu = Nomsu(tree.source, "[")
                    for i, item in ipairs tree
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
                    for item in *tree
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
                    for i, entry in ipairs tree
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
                    for entry in *tree
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
                key, value = tree[1], tree[2]
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
                for i, bit in ipairs tree
                    if i > 1
                        nomsu\append "."
                    bit_nomsu = @tree_to_nomsu(bit, true)
                    return nil unless bit_nomsu
                    if bit.type == "Action" or bit.type == "Block"
                        bit_nomsu\parenthesize!
                    nomsu\append bit_nomsu
                return nomsu
            
            when "Number"
                return Nomsu(tree.source, tostring(tree[1]))

            when "Var"
                return Nomsu(tree.source, "%", tree[1])
            
            else
                error("Unknown type: #{tree.type}")
    
    initialize_core: =>
        -- Sets up some core functionality
        nomsu = self
        with nomsu.environment
            .A_immediately_1 = .compile_time (_block)=>
                lua = nomsu\tree_to_lua(_block)\as_statements!
                lua\declare_locals!
                nomsu\run_lua(lua)
                return Lua(_block.source, "if IMMEDIATE then\n    ", lua, "\nend")

            add_lua_string_bits = (lua, code)->
                line_len = 0
                if code.type != "Text"
                    lua\append ", ", nomsu\tree_to_lua(code)
                    return
                for bit in *code
                    bit_lua = if type(bit) == "string"
                        repr(bit)
                    else
                        bit_lua = nomsu\tree_to_lua(bit)
                        unless bit_lua.is_value
                            compile_error bit,
                                "Cannot use:\n%s\nas a string interpolation value, since it's not an expression."
                        bit_lua
                    line_len += #tostring(bit_lua)
                    if line_len > MAX_LINE
                        lua\append ",\n    "
                        line_len = 4
                    else
                        lua\append ", "
                    lua\append bit_lua

            .A_Lua_1 = .compile_time (_code)=>
                lua = Lua.Value(_code.source, "Lua(", repr(tostring _code.source))
                add_lua_string_bits(lua, _code)
                lua\append ")"
                return lua
            
            .A_Lua_value_1 = .compile_time (_code)=>
                lua = Lua.Value(_code.source, "Lua.Value(", repr(tostring _code.source))
                add_lua_string_bits(lua, _code)
                lua\append ")"
                return lua

            add_lua_bits = (lua, code)->
                for bit in *code
                    if type(bit) == "string"
                        lua\append bit
                    else
                        bit_lua = nomsu\tree_to_lua(bit)
                        unless bit_lua.is_value
                            compile_error bit,
                                "Cannot use:\n%s\nas a string interpolation value, since it's not an expression."
                        lua\append bit_lua
                return lua

            nomsu.environment["A"..string.as_lua_id("lua > 1")] = .compile_time (_code)=>
                if _code.type != "Text"
                    return Lua @source, "nomsu:run_lua(", nomsu\tree_to_lua(_code), ");"
                return add_lua_bits(Lua(@source), _code)

            nomsu.environment["A"..string.as_lua_id("= lua 1")] = .compile_time (_code)=>
                if _code.type != "Text"
                    return Lua.Value @source, "nomsu:run_lua(", nomsu\tree_to_lua(_code), ":as_statements('return '))"
                return add_lua_bits(Lua.Value(@source), _code)

            .A_use_1 = .compile_time (_path)=>
                unless _path.type == 'Text' and #_path == 1 and type(_path[1]) == 'string'
                    return Lua(_path.source, "nomsu:run_file(#{nomsu\tree_to_lua(_path)});")
                path = _path[1]
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
            if arg_orders = nomsu.environment.ARG_ORDERS[info.func]
                info.name = next(arg_orders)
            if map = nomsu.source_map[info.source]
                if info.currentline
                    info.currentline = assert(map[info.currentline])
                if info.linedefined
                    info.linedefined = assert(map[info.linedefined])
                if info.lastlinedefined
                    info.lastlinedefined = assert(map[info.lastlinedefined])
                --info.short_src = info.source\match('@([^[]*)')
        return info

    print_err_msg = (error_message, stack_offset=3)->
        io.stderr\write("#{colored.red "ERROR:"} #{colored.bright colored.red (error_message or "")}\n")
        io.stderr\write("stack traceback:\n")

        -- TODO: properly print out the calling site of nomsu code, not just the *called* code
        ok, to_lua = pcall -> require('moonscript.base').to_lua
        if not ok then to_lua = -> nil
        nomsu_source = FILE_CACHE["nomsu.moon"]
        LINE_TABLES = setmetatable {},
            __index: (file)=>
                _, line_table = to_lua(file)
                self[file] = line_table or false
                return line_table or false

        get_line = (file, line_no)->
            start = LINE_STARTS[file][line_no] or 1
            stop = (LINE_STARTS[file][line_no+1] or 0) - 1
            return file\sub(start, stop)

        level = stack_offset
        while true
            -- TODO: reduce duplicate code
            calling_fn = debug_getinfo(level)
            if not calling_fn then break
            if calling_fn.func == run then break
            level += 1
            name = calling_fn.name and "function '#{calling_fn.name}'" or nil
            if calling_fn.linedefined == 0 then name = "main chunk"
            if name == "run_lua_fn" then continue
            line = nil
            if map = nomsu.source_map[calling_fn.source]
                if calling_fn.currentline
                    calling_fn.currentline = assert(map[calling_fn.currentline])
                if calling_fn.linedefined
                    calling_fn.linedefined = assert(map[calling_fn.linedefined])
                if calling_fn.lastlinedefined
                    calling_fn.lastlinedefined = assert(map[calling_fn.lastlinedefined])
                --calling_fn.short_src = calling_fn.source\match('"([^[]*)')
                filename,start,stop = calling_fn.source\match('@([^[]*)%[([0-9]+):([0-9]+)]')
                assert(filename)
                file = FILE_CACHE[filename]\sub(tonumber(start),tonumber(stop))
                err_line = get_line(file, calling_fn.currentline)\sub(1,-2)
                offending_statement = colored.bright(colored.red(err_line\match("^[ ]*(.*)")))
                if arg_orders = nomsu.environment.ARG_ORDERS[calling_fn.func]
                    name = "action '#{next(arg_orders)}'"
                else
                    name = "main chunk"
                line = colored.yellow("#{filename}:#{calling_fn.currentline} in #{name}\n        #{offending_statement}")
            else
                ok, file = pcall ->FILE_CACHE[calling_fn.short_src]
                if not ok then file = nil
                local line_num
                if name == nil
                    search_level = level
                    _info = debug.getinfo(search_level)
                    while _info and (_info.func == pcall or _info.func == xpcall)
                        search_level += 1
                        _info = debug.getinfo(search_level)
                    if _info
                        for i=1,999
                            varname, val = debug.getlocal(search_level, i)
                            if not varname then break
                            if val == calling_fn.func
                                name = "local '#{varname}'"
                                if not varname\match("%(")
                                    break
                        unless name
                            for i=1,_info.nups
                                varname, val = debug.getupvalue(_info.func, i)
                                if not varname then break
                                if val == calling_fn.func
                                    name = "upvalue '#{varname}'"
                                    if not varname\match("%(")
                                        break
                if file and calling_fn.short_src\match(".moon$") and LINE_TABLES[file]
                    char = LINE_TABLES[file][calling_fn.currentline]
                    line_num = 1
                    for _ in file\sub(1,char)\gmatch("\n") do line_num += 1
                    line = colored.cyan("#{calling_fn.short_src}:#{line_num} in #{name or '?'}")
                else
                    line_num = calling_fn.currentline
                    if calling_fn.short_src == '[C]'
                        line = colored.green("#{calling_fn.short_src} in #{name or '?'}")
                    else
                        line = colored.blue("#{calling_fn.short_src}:#{calling_fn.currentline} in #{name or '?'}")

                if file
                    err_line = get_line(file, line_num)\sub(1,-2)
                    offending_statement = colored.bright(colored.red(err_line\match("^[ ]*(.*)$")))
                    line ..= "\n        "..offending_statement
            io.stderr\write("    #{line}\n")
            if calling_fn.istailcall
                io.stderr\write("    #{colored.dim colored.white "  (...tail calls...)"}\n")

        io.stderr\flush!
    
    run = ->
    
        for i,input in ipairs args.inputs
            if input == "-" then args.inputs[i] = STDIN

        if #args.inputs == 0 and not args.interactive
            args.inputs = {"core"}
            args.interactive = true

        print_file = if args.print_file == "-" then io.stdout
        elseif args.print_file then io.open(args.print_file, 'w')
        elseif args.output_file == '-' then nil
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

        compile_fn = if args.output_file
            (code)->
                output_file = if args.output_file == "-" then io.stdout
                elseif args.output_file then io.open(args.output_file, 'w')
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
                file = io.input!\read("*a")
                FILE_CACHE.stdin = file
                nomsu\run(Nomsu(Source('stdin',1,#file), file), compile_fn)
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
