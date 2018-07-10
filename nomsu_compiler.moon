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
--        lua nomsu.lua your_file.nom
lpeg = require 'lpeg'
re = require 're'
utils = require 'utils'
files = require 'files'
{:repr, :stringify, :equivalent} = utils
export colors, colored
colors = require 'consolecolors'
colored = setmetatable({}, {__index:(_,color)-> ((msg)-> colors[color]..tostring(msg or '')..colors.reset)})
{:insert, :remove, :concat} = table
unpack or= table.unpack
{:match, :sub, :gsub, :format, :byte, :find} = string
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
AST = require "nomsu_tree"
Parser = require("parser")
-- Mapping from source string (e.g. "@core/metaprogramming.nom[1:100]") to a mapping
-- from lua line number to nomsu line number
export SOURCE_MAP
SOURCE_MAP = {}

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
table.fork = (t, values)-> setmetatable(values or {}, {__index:t})

-- TODO:
-- consider non-linear codegen, rather than doing thunks for things like comprehensions
-- Add a ((%x foo %y) where {x:"asdf", y:"fdsa"}) compile-time action for substitution
-- Re-implement nomsu-to-lua comment translation?

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

-- List and Dict classes to provide basic equality/tostring functionality for the tables
-- used in Nomsu. This way, they retain a notion of whether they were originally lists or dicts.
_list_mt =
    __eq:equivalent
    -- Could consider adding a __newindex to enforce list-ness, but would hurt performance
    __tostring: =>
        "["..concat([repr(b) for b in *@], ", ").."]"
    __lt: (other)=>
        assert type(@) == 'table' and type(other) == 'table', "Incompatible types for comparison"
        for i=1,math.max(#@, #other)
            if @[i] < other[i] then return true
            elseif @[i] > other[i] then return false
        return false
    __le: (other)=>
        assert type(@) == 'table' and type(other) == 'table', "Incompatible types for comparison"
        for i=1,math.max(#@, #other)
            if @[i] < other[i] then return true
            elseif @[i] > other[i] then return false
        return true

list = (t)-> setmetatable(t, _list_mt)

_dict_mt =
    __eq:equivalent
    __tostring: =>
        "{"..concat(["#{repr(k)}: #{repr(v)}" for k,v in pairs @], ", ").."}"
dict = (t)-> setmetatable(t, _dict_mt)

MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value
NomsuCompiler = setmetatable({}, {__index: (k)=> if _self = rawget(@, "self") then _self[k] else nil})
with NomsuCompiler
    .NOMSU_COMPILER_VERSION = 3
    .NOMSU_SYNTAX_VERSION = Parser.version
    ._ENV = NomsuCompiler
    .nomsu = NomsuCompiler
    .parse = (...)=> Parser.parse(...)
    .can_optimize = -> false

    -- Discretionary/convenience stuff
    to_add = {
        repr:repr, stringify:stringify, utils:utils, lpeg:lpeg, re:re, files:files,
        -- Lua stuff:
        :next, :unpack, :setmetatable, :coroutine, :rawequal, :getmetatable, :pcall,
        :error, :package, :os, :require, :tonumber, :tostring, :string, :xpcall, :module,
        :print, :loadfile, :rawset, :_VERSION, :collectgarbage, :rawget, :rawlen,
        :table, :assert, :dofile, :loadstring, :type, :select, :debug, :math, :io, :load,
        :pairs, :ipairs,
        -- Nomsu types:
        :list, :dict,
    }
    for k,v in pairs(to_add) do NomsuCompiler[k] = v
    for k,v in pairs(AST) do NomsuCompiler[k] = v
    .LuaCode = LuaCode
    .NomsuCode = NomsuCode
    .Source = Source
    .ALIASES = setmetatable({}, {__mode:"k"})
    .LOADED = {}
    .AST = AST

    .compile_error = (tok, err_format_string, ...)=>
        file = files.read(tok.source.filename)
        line_starts = files.get_line_starts(file)
        line_no = files.get_line_number(file, tok.source.start)
        line_start = line_starts[line_no]
        src = colored.dim(file\sub(line_start, tok.source.start-1))
        src ..= colored.underscore colored.bright colored.red(file\sub(tok.source.start, tok.source.stop-1))
        end_of_line = (line_starts[files.get_line_number(file, tok.source.stop) + 1] or 0) - 1
        src ..= colored.dim(file\sub(tok.source.stop, end_of_line-1))
        src = '    '..src\gsub('\n', '\n    ')
        err_msg = err_format_string\format(src, ...)
        error("#{tok.source.filename}:#{line_no}: "..err_msg, 0)

    -- This is a bit of a hack, but this code handles arbitrarily complex
    -- math expressions like 2*x + 3^2 without having to define a single
    -- action for every possibility.
    math_expression = re.compile [[ ([+-] " ")* "%" (" " [*/^+-] (" " [+-])* " %")+ !. ]]
    add_lua_bits = (lua, code)=>
        for bit in *code
            if type(bit) == "string"
                lua\append bit
            else
                bit_lua = @compile(bit)
                unless bit_lua.is_value
                    @compile_error bit,
                        "Cannot use:\n%s\nas a string interpolation value, since it's not an expression."
                lua\append bit_lua
        return lua

    add_lua_string_bits = (lua, code)=>
        line_len = 0
        if code.type != "Text"
            lua\append ", ", @compile(code)
            return
        for bit in *code
            bit_lua = if type(bit) == "string"
                repr(bit)
            else
                bit_lua = @compile(bit)
                unless bit_lua.is_value
                    @compile_error bit,
                        "Cannot use:\n%s\nas a string interpolation value, since it's not an expression."
                bit_lua
            line_len += #tostring(bit_lua)
            if line_len > MAX_LINE
                lua\append ",\n    "
                line_len = 4
            else
                lua\append ", "
            lua\append bit_lua

    .COMPILE_ACTIONS = setmetatable {
        ["# compile math expr #"]: (tree, ...)=>
            lua = LuaCode.Value(tree.source)
            for i,tok in ipairs tree
                if type(tok) == 'string'
                    lua\append tok
                else
                    tok_lua = @compile(tok)
                    unless tok_lua.is_value
                        @compile_error tok, "Non-expression value inside math expression:\n%s"
                    if tok.type == "Action"
                        tok_lua\parenthesize!
                    lua\append tok_lua
                if i < #tree
                    lua\append " "
            return lua

        ["Lua %"]: (tree, _code)=>
            lua = LuaCode.Value(_code.source, "LuaCode(", repr(tostring _code.source))
            add_lua_string_bits(@, lua, _code)
            lua\append ")"
            return lua
    
        ["Lua value %"]: (tree, _code)=>
            lua = LuaCode.Value(_code.source, "LuaCode.Value(", repr(tostring _code.source))
            add_lua_string_bits(@, lua, _code)
            lua\append ")"
            return lua

        ["lua > %"]: (tree, _code)=>
            if _code.type != "Text"
                return LuaCode tree.source, "nomsu:run_lua(", @compile(_code), ");"
            return add_lua_bits(@, LuaCode(tree.source), _code)

        ["= lua %"]: (tree, _code)=>
            if _code.type != "Text"
                return LuaCode.Value tree.source, "nomsu:run_lua(", @compile(_code), ":as_statements('return '))"
            return add_lua_bits(@, LuaCode.Value(tree.source), _code)

        ["use %"]: (tree, _path)=>
            if _path.type == 'Text' and #_path == 1 and type(_path[1]) == 'string'
                path = _path[1]
                for f in files.walk(path)
                    @run_file(f)
            return LuaCode(tree.source, "for f in files.walk(", @compile(_path), ") do nomsu:run_file(f) end")
    }, {
        __index: (stub)=>
            if math_expression\match(stub)
                return @["# compile math expr #"]
    }

    .run = (to_run, source=nil)=>
        source or= to_run.source or Source(to_run, 1, #to_run)
        if not files.read(source.filename) then files.spoof(source.filename, to_run)
        tree = if AST.is_syntax_tree(to_run) then to_run else @parse(to_run, source)
        if tree == nil -- Happens if pattern matches, but there are no captures, e.g. an empty string
            return nil
        if tree.type == "FileChunks"
            -- Each chunk's compilation is affected by the code in the previous chunks
            -- (typically), so each chunk needs to compile and run before the next one
            -- compiles.
            ret = nil
            all_lua = {}
            for chunk in *tree
                lua = @compile(chunk)\as_statements("return ")
                lua\declare_locals!
                lua\prepend "-- File: #{source.filename\gsub("\n.*", "...")}\n"
                insert all_lua, tostring(lua)
                ret = @run_lua(lua)
            return ret
        else
            lua = @compile(tree)\as_statements("return ")
            lua\declare_locals!
            lua\prepend "-- File: #{source.filename\gsub("\n.*", "...")}\n"
            return @run_lua(lua)

    _running_files = {} -- For detecting circular imports
    .run_file = (filename)=>
        if @LOADED[filename]
            return @LOADED[filename]
        -- Check for circular import
        -- TODO: optimize?
        for i,running in ipairs _running_files
            if running == filename
                loop = [_running_files[j] for j=i,#_running_files]
                insert loop, filename
                error("Circular import, this loops forever: #{concat loop, " -> "}...")

        insert _running_files, filename
        ret = nil
        if match(filename, "%.lua$")
            file = assert(files.read(filename), "Could not find file: #{filename}")
            ret = @run_lua file, Source(filename, 1, #file)
        elseif match(filename, "%.nom$") or match(filename, "^/dev/fd/[012]$")
            ran_lua = if @.can_optimize(filename) -- Look for precompiled version
                lua_filename = gsub(filename, "%.nom$", ".lua")
                if file = files.read(lua_filename)
                    ret = @run_lua file, Source(lua_filename, 1, #file)
                    true
            unless ran_lua
                file = files.read(filename)
                if not file
                    error("File does not exist: #{filename}", 0)
                ret = @run file, Source(filename,1,#file)
        else
            error("Invalid filetype for #{filename}", 0)
        @LOADED[filename] = ret or true
        remove _running_files

        @LOADED[filename] = ret or true
        return ret

    .run_lua = (lua, source=nil)=>
        lua_string = tostring(lua)
        run_lua_fn, err = load(lua_string, nil and tostring(source or lua.source), "t", self)
        if not run_lua_fn
            line_numbered_lua = concat(
                [format("%3d|%s",i,line) for i, line in ipairs files.get_lines(lua_string)],
                "\n")
            error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack line_numbered_lua}\n\n#{err}", 0)
        source_key = tostring(source or lua.source)
        unless SOURCE_MAP[source_key]
            map = {}
            offset = 1
            source or= lua.source
            file = files.read(source.filename)
            if not file
                error "Failed to find file: #{source.filename}"
            nomsu_str = tostring(file\sub(source.start, source.stop))
            lua_line = 1
            nomsu_line = files.get_line_number(nomsu_str, source.start)
            fn = (s)->
                if type(s) == 'string'
                    for nl in s\gmatch("\n")
                        map[lua_line] or= nomsu_line
                        lua_line += 1
                else
                    old_line = nomsu_line
                    if s.source
                        nomsu_line = files.get_line_number(nomsu_str, s.source.start)
                    for b in *s.bits do fn(b)
            fn(lua)
            map[lua_line] or= nomsu_line
            map[0] = 0
            -- Mapping from lua line number to nomsu line numbers
            SOURCE_MAP[source_key] = map

        return run_lua_fn!

    .compile = (tree)=>
        if tree.version
            if upgrade = @['A'..string.as_lua_id("upgrade 1 from 2")]
                tree = upgrade(tree, tree.version)
        switch tree.type
            when "Action"
                stub = tree.stub
                if compile_action = @COMPILE_ACTIONS[stub]
                    args = [arg for arg in *tree when type(arg) != "string"]
                    -- Force Lua to avoid tail call optimization for debugging purposes
                    -- TODO: use tail call?
                    ret = compile_action(@, tree, unpack(args))
                    if not ret
                        @compile_error tree,
                            "Compile-time action:\n%s\nfailed to produce any Lua"
                    return ret

                lua = LuaCode.Value(tree.source, "A",string.as_lua_id(stub),"(")
                args = {}
                for i, tok in ipairs tree
                    if type(tok) == "string" then continue
                    arg_lua = @compile(tok)
                    unless arg_lua.is_value
                        @compile_error tok,
                            "Cannot use:\n%s\nas an argument to %s, since it's not an expression, it produces: %s",
                            stub, repr arg_lua
                    insert args, arg_lua
                lua\concat_append args, ", "
                lua\append ")"
                return lua

            when "EscapedNomsu"
                make_tree = (t)->
                    unless AST.is_syntax_tree(t)
                        return repr(t)
                    bits = [make_tree(bit) for bit in *t]
                    return t.type.."("..repr(tostring t.source)..", "..table.concat(bits, ", ")..")"
                return LuaCode.Value tree.source, make_tree(tree[1])
            
            when "Block"
                lua = LuaCode(tree.source)
                lua\concat_append([@compile(line)\as_statements! for line in *tree], "\n")
                return lua

            when "Text"
                lua = LuaCode.Value(tree.source)
                string_buffer = ""
                for i, bit in ipairs tree
                    if type(bit) == "string"
                        string_buffer ..= bit
                        continue
                    if string_buffer ~= ""
                        if #lua.bits > 0 then lua\append ".."
                        lua\append repr(string_buffer)
                        string_buffer = ""
                    bit_lua = @compile(bit)
                    unless bit_lua.is_value
                        src = '    '..gsub(tostring(recurse(bit)), '\n','\n    ')
                        line = "#{bit.source.filename}:#{files.get_line_number(files.read(bit.source.filename), bit.source.start)}"
                        @compile_error bit,
                            "Cannot use:\n%s\nas a string interpolation value, since it's not an expression."
                    if #lua.bits > 0 then lua\append ".."
                    if bit.type != "Text"
                        bit_lua = LuaCode.Value(bit.source, "stringify(",bit_lua,")")
                    lua\append bit_lua

                if string_buffer ~= "" or #lua.bits == 0
                    if #lua.bits > 0 then lua\append ".."
                    lua\append repr(string_buffer)

                if #lua.bits > 1
                    lua\parenthesize!
                return lua

            when "List"
                lua = LuaCode.Value tree.source, "list{"
                items = {}
                for i, item in ipairs tree
                    item_lua = @compile(item)
                    unless item_lua.is_value
                        @compile_error item,
                            "Cannot use:\n%s\nas a list item, since it's not an expression."
                    items[i] = item_lua
                lua\concat_append(items, ", ", ",\n  ")
                lua\append "}"
                return lua

            when "Dict"
                lua = LuaCode.Value tree.source, "dict{"
                lua\concat_append([@compile(e) for e in *tree], ", ", ",\n  ")
                lua\append "}"
                return lua

            when "DictEntry"
                key, value = tree[1], tree[2]
                key_lua = @compile(key)
                unless key_lua.is_value
                    @compile_error tree[1],
                        "Cannot use:\n%s\nas a dict key, since it's not an expression."
                value_lua = value and @compile(value) or LuaCode.Value(key.source, "true")
                unless value_lua.is_value
                    @compile_error tree[2],
                        "Cannot use:\n%s\nas a dict value, since it's not an expression."
                -- TODO: support arbitrary words here, like operators and unicode
                key_str = match(tostring(key_lua), [=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
                return if key_str
                    LuaCode tree.source, key_str,"=",value_lua
                elseif sub(tostring(key_lua),1,1) == "["
                    -- NOTE: this *must* use a space after the [ to avoid freaking out
                    -- Lua's parser if the inner expression is a long string. Lua
                    -- parses x[[[y]]] as x("[y]"), not as x["y"]
                    LuaCode tree.source, "[ ",key_lua,"]=",value_lua
                else
                    LuaCode tree.source, "[",key_lua,"]=",value_lua
            
            when "IndexChain"
                lua = @compile(tree[1])
                unless lua.is_value
                    @compile_error tree[1],
                        "Cannot index:\n%s\nsince it's not an expression."
                first_char = sub(tostring(lua),1,1)
                if first_char == "{" or first_char == '"' or first_char == "["
                    lua\parenthesize!

                for i=2,#tree
                    key = tree[i]
                    key_lua = @compile(key)
                    unless key_lua.is_value
                        @compile_error key,
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
                return LuaCode.Value(tree.source, tostring(tree[1]))

            when "Var"
                return LuaCode.Value(tree.source, string.as_lua_id(tree[1]))

            when "FileChunks"
                error("Cannot convert FileChunks to a single block of lua, since each chunk's "..
                    "compilation depends on the earlier chunks")

            else
                error("Unknown type: #{tree.type}")

    .tree_to_nomsu = (tree, options)=>
        options or= {}
        comments = options.comments
        if comments == nil and tree.comments
            comments = [{comment:c, pos:p} for p,c in pairs tree.comments]
            table.sort comments, (a,b)-> a.pos > b.pos
        recurse = (t, opts)->
            opts or= {}
            opts.comments = comments
            return @tree_to_nomsu(t, opts)
        pop_comments = (pos)->
            return '' unless comments
            nomsu = NomsuCode(tree.source)
            while #comments > 0 and comments[#comments].pos <= pos
                comment = table.remove comments
                nomsu\append "#"..(gsub(comment.comment, "\n", "\n    ")).."\n"
            if #nomsu.bits == 0 then return ''
            return nomsu

        inline, can_use_colon = options.inline, options.can_use_colon
        switch tree.type
            when "FileChunks"
                return nil if inline
                nomsu = NomsuCode(tree.source)
                for i, chunk in ipairs tree
                    if i > 1
                        nomsu\append "\n\n#{("~")\rep(80)}\n\n"
                    nomsu\append pop_comments(chunk.source.start)
                    nomsu\append recurse(chunk)
                return nomsu

            when "Action"
                if inline
                    nomsu = NomsuCode(tree.source)
                    for i,bit in ipairs tree
                        if type(bit) == "string"
                            if i > 1
                                nomsu\append " "
                            nomsu\append bit
                        else
                            arg_nomsu = recurse(bit,inline:true)
                            return nil unless arg_nomsu
                            unless i == 1
                                nomsu\append " "
                            if bit.type == "Action" or bit.type == "Block"
                                arg_nomsu\parenthesize!
                            nomsu\append arg_nomsu
                    return nomsu
                else
                    nomsu = NomsuCode(tree.source)
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
                            else recurse(bit,inline:true)

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
                                arg_nomsu = recurse(bit, can_use_colon:true)
                                return nil unless arg_nomsu
                                -- These types carry their own indentation
                                if bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
                                    if i == 1
                                        arg_nomsu = NomsuCode(bit.source, "(..)\n    ", pop_comments(bit.source.start), arg_nomsu)
                                    else
                                        arg_nomsu = NomsuCode(bit.source, "\n    ", pop_comments(bit.source.start), arg_nomsu)
                                
                                if last_colon == i-1 and (bit.type == "Action" or bit.type == "Block")
                                    next_space = ""
                                nomsu\append next_space, arg_nomsu
                                next_space = "\n.."
                                line_len = 2

                            if next_space == " " and #(match(tostring(nomsu),"[^\n]*$")) > MAX_LINE
                                next_space = "\n.."
                    return nomsu

            when "EscapedNomsu"
                nomsu = recurse(tree[1], inline:true)
                if nomsu == nil and not inline
                    nomsu = recurse(tree[1])
                    return nomsu and NomsuCode tree.source, "\\:\n    ", pop_comments(tree.source.start), nomsu
                return nomsu and NomsuCode tree.source, "\\(", nomsu, ")"

            when "Block"
                if inline
                    nomsu = NomsuCode(tree.source)
                    for i,line in ipairs tree
                        if i > 1
                            nomsu\append "; "
                        line_nomsu = recurse(line,inline:true)
                        return nil unless line_nomsu
                        nomsu\append line_nomsu
                    return nomsu
                nomsu = NomsuCode(tree.source)
                for i, line in ipairs tree
                    nomsu\append pop_comments(line.source.start)
                    line = assert(recurse(line, can_use_colon:true), "Could not convert line to nomsu")
                    nomsu\append line
                    if i < #tree
                        nomsu\append "\n"
                        if match(tostring(line), "\n")
                            nomsu\append "\n"
                return nomsu

            when "Text"
                if inline
                    nomsu = NomsuCode(tree.source, '"')
                    for bit in *tree
                        if type(bit) == 'string'
                            -- TODO: unescape better?
                            nomsu\append (gsub(gsub(gsub(bit,"\\","\\\\"),"\n","\\n"),'"','\\"'))
                        else
                            interp_nomsu = recurse(bit, inline:true)
                            if interp_nomsu
                                if bit.type != "Var" and bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
                                    interp_nomsu\parenthesize!
                                nomsu\append "\\", interp_nomsu
                            else return nil
                    nomsu\append '"'
                    return nomsu
                else
                    inline_version = recurse(tree, inline:true)
                    if inline_version and #inline_version <= MAX_LINE
                        return inline_version
                    nomsu = NomsuCode(tree.source, '".."\n    ')
                    for i, bit in ipairs tree
                        if type(bit) == 'string'
                            bit_lines = files.get_lines(bit)
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
                            interp_nomsu = recurse(bit, inline:true)
                            if interp_nomsu
                                if bit.type != "Var" and bit.type != "List" and bit.type != "Dict" and bit.type != "Text"
                                    interp_nomsu\parenthesize!
                                nomsu\append "\\", interp_nomsu
                            else
                                interp_nomsu = assert(recurse(bit))
                                return nil unless interp_nomsu
                                nomsu\append "\\\n        ", interp_nomsu
                                if i < #tree
                                    nomsu\append "\n    .."
                    return nomsu

            when "List"
                if inline
                    nomsu = NomsuCode(tree.source, "[")
                    for i, item in ipairs tree
                        item_nomsu = recurse(item, inline:true)
                        return nil unless item_nomsu
                        if i > 1
                            nomsu\append ", "
                        nomsu\append item_nomsu
                    nomsu\append "]"
                    return nomsu
                else
                    inline_version = recurse(tree, inline:true)
                    if inline_version and #inline_version <= MAX_LINE
                        return inline_version
                    nomsu = NomsuCode(tree.source, "[..]")
                    line = NomsuCode(tree.source, "\n    ")
                    line_comments = if #tree > 0
                        pop_comments(tree[1].source.start)
                    else ''
                    for i, item in ipairs tree
                        item_nomsu = recurse(item, inline:true)
                        if item_nomsu and #tostring(line) + #", " + #item_nomsu <= MAX_LINE
                            if #line.bits > 1
                                line\append ", "
                            line\append item_nomsu
                        else
                            unless item_nomsu
                                item_nomsu = recurse(item)
                                return nil unless item_nomsu
                            if #line.bits > 1
                                if #tostring(line_comments) > 0
                                    nomsu\append '\n    ', line_comments
                                nomsu\append line
                                line = NomsuCode(line.source, "\n    ")
                                line_comments = if i < #tree
                                    pop_comments(tree[i+1].source.start)
                                else ''
                            line\append item_nomsu
                    if #line.bits > 1
                        nomsu\append line
                    return nomsu
            
            when "Dict"
                if inline
                    nomsu = NomsuCode(tree.source, "{")
                    for i, entry in ipairs tree
                        entry_nomsu = recurse(entry, inline:true)
                        return nil unless entry_nomsu
                        if i > 1
                            nomsu\append ", "
                        nomsu\append entry_nomsu
                    nomsu\append "}"
                    return nomsu
                else
                    inline_version = recurse(tree, inline:true)
                    if inline_version then return inline_version
                    nomsu = NomsuCode(tree.source, "{..}")
                    line = NomsuCode(tree.source, "\n    ")
                    line_comments = if #tree > 0
                        pop_comments(tree[1].source.start)
                    else ''
                    for i, entry in ipairs tree
                        entry_nomsu = recurse(entry)
                        return nil unless entry_nomsu
                        if #line + #tostring(entry_nomsu) <= MAX_LINE
                            if #line.bits > 1
                                line\append ", "
                            line\append entry_nomsu
                        else
                            if #line.bits > 1
                                if #tostring(line_comments) > 0
                                    nomsu\append "\n    ", line_comments
                                nomsu\append line
                                line = NomsuCode(line.source, "\n    ")
                                line_comments = if i < #tree
                                    pop_comments(tree[1].source.start)
                                else ''
                            line\append entry_nomsu
                    if #line.bits > 1
                        nomsu\append line
                    return nomsu
            
            when "DictEntry"
                key, value = tree[1], tree[2]
                key_nomsu = recurse(key, inline:true)
                return nil unless key_nomsu
                if key.type == "Action" or key.type == "Block"
                    key_nomsu\parenthesize!
                value_nomsu = if value
                    recurse(value, inline:true)
                else NomsuCode(tree.source, "")
                if inline and not value_nomsu then return nil
                if not value_nomsu
                    return nil if inline
                    value_nomsu = recurse(value)
                    return nil unless value_nomsu
                return NomsuCode tree.source, key_nomsu, ":", value_nomsu
            
            when "IndexChain"
                nomsu = NomsuCode(tree.source)
                for i, bit in ipairs tree
                    if i > 1
                        nomsu\append "."
                    local bit_nomsu
                    if bit.type == "Text" and #bit == 1 and type(bit[1]) == 'string'
                        -- TODO: support arbitrary words here, including operators and unicode
                        if bit[1]\match("[_a-zA-Z][_a-zA-Z0-9]*")
                            bit_nomsu = bit[1]
                    unless bit_nomsu then bit_nomsu = recurse(bit, inline:true)
                    return nil unless bit_nomsu
                    switch bit.type
                        when "Action", "Block", "IndexChain"
                            bit_nomsu\parenthesize!
                        when "Number"
                            if i < #tree
                                bit_nomsu\parenthesize!
                    nomsu\append bit_nomsu
                return nomsu
            
            when "Number"
                return NomsuCode(tree.source, tostring(tree[1]))

            when "Var"
                return NomsuCode(tree.source, "%", tree[1])
            
            else
                error("Unknown type: #{tree.type}")

return NomsuCompiler
