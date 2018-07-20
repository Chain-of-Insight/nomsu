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
            if not @[i] and other[i] then return true
            elseif @[i] and not other[i] then return false
            elseif @[i] < other[i] then return true
            elseif @[i] > other[i] then return false
        return false
    __le: (other)=>
        assert type(@) == 'table' and type(other) == 'table', "Incompatible types for comparison"
        for i=1,math.max(#@, #other)
            if not @[i] and other[i] then return true
            elseif @[i] and not other[i] then return false
            elseif @[i] < other[i] then return true
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
    .NOMSU_COMPILER_VERSION = 4
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

    .compile_error = (source, err_format_string, ...)=>
        err_format_string = err_format_string\gsub("%%[^s]", "%%%1")
        file = files.read(source.filename)
        line_starts = files.get_line_starts(file)
        line_no = files.get_line_number(file, source.start)
        line_start = line_starts[line_no]
        src = colored.dim(file\sub(line_start, source.start-1))
        src ..= colored.underscore colored.bright colored.red(file\sub(source.start, source.stop-1))
        end_of_line = (line_starts[files.get_line_number(file, source.stop) + 1] or 0) - 1
        src ..= colored.dim(file\sub(source.stop, end_of_line-1))
        src = '    '..src\gsub('\n', '\n    ')
        err_msg = err_format_string\format(src, ...)
        error("#{source.filename}:#{line_no}: "..err_msg, 0)

    -- This is a bit of a hack, but this code handles arbitrarily complex
    -- math expressions like 2*x + 3^2 without having to define a single
    -- action for every possibility.
    math_expression = re.compile [[ ([+-] " ")* "%" (" " [*/^+-] (" " [+-])* " %")+ !. ]]

    add_lua_bits = (val_or_stmt, code)=>
        cls = val_or_stmt == "value" and LuaCode.Value or LuaCode
        operate_on_text = (text)->
            lua = cls(text.source)
            for bit in *text
                if type(bit) == "string"
                    lua\append bit
                elseif bit.type == "Text"
                    lua\append(operate_on_text(bit))
                else
                    bit_lua = @compile(bit)
                    unless bit_lua.is_value
                        @compile_error bit.source,
                            "Cannot use:\n%s\nas a string interpolation value, since it's not an expression."
                    lua\append bit_lua
            return lua
        return operate_on_text code

    add_lua_string_bits = (val_or_stmt, code)=>
        cls_str = val_or_stmt == "value" and "LuaCode.Value(" or "LuaCode("
        if code.type != "Text"
            return LuaCode.Value(code.source, cls_str, repr(tostring(code.source)), ", ", @compile(code), ")")
        add_bit_lua = (lua, bit_lua)->
            bit_leading_len = #(tostring(bit_lua)\match("^[^\n]*"))
            lua\append(lua\trailing_line_len! + bit_leading_len > MAX_LINE and ",\n    " or ", ")
            lua\append(bit_lua)
        operate_on_text = (text)->
            lua = LuaCode.Value(text.source, cls_str, repr(tostring(text.source)))
            for bit in *text
                if type(bit) == "string"
                    add_bit_lua(lua, repr(bit))
                elseif bit.type == "Text"
                    add_bit_lua(lua, operate_on_text(bit))
                else
                    bit_lua = @compile(bit)
                    unless bit_lua.is_value
                        @compile_error bit.source,
                            "Cannot use:\n%s\nas a string interpolation value, since it's not an expression."
                    add_bit_lua(lua, bit_lua)
            lua\append ")"
            return lua
        return operate_on_text code

    .COMPILE_ACTIONS = setmetatable {
        ["# compile math expr #"]: (tree, ...)=>
            lua = LuaCode.Value(tree.source)
            for i,tok in ipairs tree
                if type(tok) == 'string'
                    lua\append tok
                else
                    tok_lua = @compile(tok)
                    unless tok_lua.is_value
                        @compile_error tok.source, "Non-expression value inside math expression:\n%s"
                    if tok.type == "Action"
                        tok_lua\parenthesize!
                    lua\append tok_lua
                if i < #tree
                    lua\append " "
            return lua

        ["Lua %"]: (tree, _code)=>
            return add_lua_string_bits(@, 'statements', _code)
    
        ["Lua value %"]: (tree, _code)=>
            return add_lua_string_bits(@, 'value', _code)

        ["lua > %"]: (tree, _code)=>
            if _code.type != "Text"
                return LuaCode tree.source, "nomsu:run_lua(", @compile(_code), ");"
            return add_lua_bits(@, "statements", _code)

        ["= lua %"]: (tree, _code)=>
            if _code.type != "Text"
                return LuaCode.Value tree.source, "nomsu:run_lua(", @compile(_code), ":as_statements('return '))"
            return add_lua_bits(@, "value", _code)

        ["use %"]: (tree, _path)=>
            if _path.type == 'Text' and #_path == 1 and type(_path[1]) == 'string'
                path = _path[1]
                for _,f in files.walk(path)
                    @run_file(f)
            return LuaCode(tree.source, "for i,f in files.walk(", @compile(_path), ") do nomsu:run_file(f) end")

        ["test %"]: (tree, _body)=>
            return LuaCode ""
    }, {
        __index: (stub)=>
            if math_expression\match(stub)
                return @["# compile math expr #"]
    }

    .run = (to_run, source=nil, version=nil)=>
        source or= to_run.source or Source(to_run, 1, #to_run)
        if type(source) == 'string' then source = Source\from_string(source)
        if not files.read(source.filename) then files.spoof(source.filename, to_run)
        tree = if AST.is_syntax_tree(to_run) then to_run else @parse(to_run, source, version)
        if tree == nil -- Happens if pattern matches, but there are no captures, e.g. an empty string
            return nil
        if tree.type != "FileChunks"
            tree = {tree}
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
        source or= lua.source
        source_key = tostring(source)
        unless SOURCE_MAP[source_key]
            map = {}
            file = files.read(source.filename)
            if not file
                error "Failed to find file: #{source.filename}"
            nomsu_str = tostring(file\sub(source.start, source.stop))
            lua_line = 1
            nomsu_line = files.get_line_number(file, source.start)
            map_sources = (s)->
                if type(s) == 'string'
                    for nl in s\gmatch("\n")
                        map[lua_line] or= nomsu_line
                        lua_line += 1
                else
                    if s.source and s.source.filename == source.filename
                        nomsu_line = files.get_line_number(file, s.source.start)
                    for b in *s.bits do map_sources(b)
            map_sources(lua)
            map[lua_line] or= nomsu_line
            map[0] = 0
            -- Mapping from lua line number to nomsu line numbers
            SOURCE_MAP[source_key] = map

        return run_lua_fn!

    .compile = (tree)=>
        if tree.version
            if get_version = @['A'..string.as_lua_id("Nomsu version")]
                if upgrade = @['A'..string.as_lua_id("1 upgraded from 2 to 3")]
                    tree = upgrade(tree, tree.version, get_version!)
        switch tree.type
            when "Action"
                stub = tree.stub
                if compile_action = @COMPILE_ACTIONS[stub]
                    args = [arg for arg in *tree when type(arg) != "string"]
                    -- Force Lua to avoid tail call optimization for debugging purposes
                    -- TODO: use tail call?
                    ret = compile_action(@, tree, unpack(args))
                    if not ret
                        @compile_error tree.source,
                            "Compile-time action:\n%s\nfailed to produce any Lua"
                    return ret

                lua = LuaCode.Value(tree.source, "A",string.as_lua_id(stub),"(")
                args = {}
                for i, tok in ipairs tree
                    if type(tok) == "string" then continue
                    arg_lua = @compile(tok)
                    unless arg_lua.is_value
                        @compile_error tok.source,
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
                    insert bits, 1, repr(tostring t.source)
                    return t.type.."("..concat(bits, ", ")..")"
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
                        @compile_error bit.source,
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
                        @compile_error item.source,
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
                    @compile_error tree[1].source,
                        "Cannot use:\n%s\nas a dict key, since it's not an expression."
                value_lua = value and @compile(value) or LuaCode.Value(key.source, "true")
                unless value_lua.is_value
                    @compile_error tree[2].source,
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
                    @compile_error tree[1].source,
                        "Cannot index:\n%s\nsince it's not an expression."
                first_char = sub(tostring(lua),1,1)
                if first_char == "{" or first_char == '"' or first_char == "["
                    lua\parenthesize!

                for i=2,#tree
                    key = tree[i]
                    key_lua = @compile(key)
                    unless key_lua.is_value
                        @compile_error key.source,
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

    .tree_to_inline_nomsu = (tree, parenthesize_blocks=false)=>
        switch tree.type
            when "FileChunks"
                error("Cannot inline a FileChunks")

            when "Action"
                nomsu = NomsuCode(tree.source)
                for i,bit in ipairs tree
                    if type(bit) == "string"
                        clump_words = (type(tree[i-1]) == 'string' and Parser.is_operator(bit) != Parser.is_operator(tree[i-1]))
                        nomsu\append " " if i > 1 and not clump_words
                        nomsu\append bit
                    else
                        arg_nomsu = @tree_to_inline_nomsu(bit, parenthesize_blocks or (i == 1 or i < #tree))
                        nomsu\append " " unless tostring(arg_nomsu)\match("^:") or i == 1
                        arg_nomsu\parenthesize! if bit.type == "Action"
                        nomsu\append arg_nomsu
                return nomsu

            when "EscapedNomsu"
                inner_nomsu = @tree_to_inline_nomsu(tree[1])
                if tree[1].type == "List" or tree[1].type == "Dict" or tree[1].type == "Var"
                    return NomsuCode(tree.source, "\\", inner_nomsu)
                else
                    return NomsuCode(tree.source, "\\(", inner_nomsu, ")")

            when "Block"
                nomsu = NomsuCode(tree.source, ":")
                for i,line in ipairs tree
                    nomsu\append(i == 1 and " " or "; ")
                    nomsu\append @tree_to_inline_nomsu(line, i == 1 or i < #tree)
                nomsu\parenthesize! if #tree > 1 or parenthesize_blocks
                return nomsu

            when "Text"
                make_text = (tree)->
                    nomsu = NomsuCode(tree.source)
                    for i, bit in ipairs tree
                        if type(bit) == 'string'
                            bit = Parser.inline_escape(bit)
                            nomsu\append bit
                        elseif bit.type == "Text"
                            nomsu\append(make_text(bit))
                        else
                            interp_nomsu = @tree_to_inline_nomsu(bit)
                            if bit.type != "Var" and bit.type != "List" and bit.type != "Dict"
                                interp_nomsu\parenthesize!
                            elseif bit.type == "Var" and type(tree[i+1]) == 'string' and not match(tree[i+1], "^[ \n\t,.:;#(){}[%]]")
                                interp_nomsu\parenthesize!
                            nomsu\append "\\", interp_nomsu
                    return nomsu
                return NomsuCode(tree.source, '"', make_text(tree), '"')

            when "List"
                nomsu = NomsuCode(tree.source, "[")
                for i, item in ipairs tree
                    nomsu\append ", " if i > 1
                    nomsu\append @tree_to_inline_nomsu(item)
                nomsu\append "]"
                return nomsu
            
            when "Dict"
                nomsu = NomsuCode(tree.source, "{")
                for i, entry in ipairs tree
                    nomsu\append ", " if i > 1
                    nomsu\append @tree_to_inline_nomsu(entry)
                nomsu\append "}"
                return nomsu
            
            when "DictEntry"
                key, value = tree[1], tree[2]
                key_nomsu = if key.type == "Text" and #key == 1 and Parser.is_identifier(key[1])
                    NomsuCode(key.source, key[1])
                else @tree_to_inline_nomsu(key)
                key_nomsu\parenthesize! if key.type == "Action" or key.type == "Block"
                value_nomsu = if value
                    @tree_to_inline_nomsu(value)
                else NomsuCode(tree.source, "")
                assert(value.type != "Block", "Didn't expect to find a Block as a value in a dict")
                value_nomsu\parenthesize! if value.type == "Block"
                return NomsuCode tree.source, key_nomsu, ": ", value_nomsu
            
            when "IndexChain"
                nomsu = NomsuCode(tree.source)
                for i, bit in ipairs tree
                    nomsu\append "." if i > 1
                    local bit_nomsu
                    bit_nomsu = if bit.type == "Text" and #bit == 1 and type(bit[1]) == 'string' and Parser.is_identifier(bit[1])
                        bit[1]
                    else @tree_to_inline_nomsu(bit)
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

    .tree_to_nomsu = (tree, options)=>
        options or= {}
        options.consumed_comments or= {}
        pop_comments = (pos, prefix='', suffix='')->
            find_comments = (t)->
                if t.comments and t.source.filename == tree.source.filename
                    for c in *t.comments
                        coroutine.yield(c) unless options.consumed_comments[c]
                for x in *t
                    find_comments(x) if AST.is_syntax_tree x
            nomsu = NomsuCode(tree.source)
            for comment in coroutine.wrap(-> find_comments(tree))
                break if comment.pos > pos
                options.consumed_comments[comment] = true
                nomsu\append("#"..(gsub(comment.comment, "\n", "\n    ")).."\n")
                if comment.comment\match("^\n.") then nomsu\append("\n") -- for aesthetics
            return '' if #nomsu.bits == 0
            nomsu\prepend prefix
            nomsu\append suffix
            return nomsu

        recurse = (t, opts)->
            opts or= {}
            opts.consumed_comments = options.consumed_comments
            return @tree_to_nomsu(t, opts)

        switch tree.type
            when "FileChunks"
                nomsu = NomsuCode(tree.source, pop_comments(tree.source.start))
                for i, chunk in ipairs tree
                    nomsu\append "\n\n#{("~")\rep(80)}\n\n" if i > 1
                    nomsu\append pop_comments(chunk.source.start)
                    nomsu\append recurse(chunk, top:true)
                nomsu\append pop_comments(tree.source.stop, '\n')
                return nomsu

            when "Action"
                pos = tree.source.start
                nomsu = NomsuCode(tree.source, pop_comments(pos))
                next_space = ""
                for i,bit in ipairs tree
                    if match(next_space, '\n')
                        nomsu\append pop_comments(pos, '\n')
                    if type(bit) == "string"
                        if next_space == ' ' and (type(tree[i-1]) == 'string' and Parser.is_operator(bit) != Parser.is_operator(tree[i-1]))
                            next_space = ''
                        nomsu\append next_space, bit
                        next_space = " "
                    else
                        arg_nomsu = if bit.type == "Block" and #bit > 1 then nil
                        else @tree_to_inline_nomsu(bit)
                        if bit.type == "Text" and tostring(arg_nomsu) != '"\\n"' and tostring(arg_nomsu)\match("\\n")
                            arg_nomsu = nil -- Force long text for multi-line text
                        next_space = match(next_space, "[^ ]*") if bit.type == "Block"
                        nomsu\append next_space
                        if arg_nomsu and nomsu\trailing_line_len! + #tostring(arg_nomsu) < MAX_LINE
                            if bit.type == "Block"
                                nomsu\append arg_nomsu
                                next_space = "\n.."
                            else
                                arg_nomsu\parenthesize! if bit.type == "Action"
                                nomsu\append arg_nomsu
                                next_space = " "
                        else
                            arg_nomsu = assert recurse(bit)
                            -- These types carry their own indentation
                            switch bit.type
                                when "List", "Dict", "Text", "Block", "EscapedNomsu"
                                    nomsu\append arg_nomsu
                                else
                                    nomsu\append NomsuCode(bit.source, "(..)\n    ", pop_comments(bit.source.start), arg_nomsu)
                            next_space = "\n.."
                        pos = bit.source.stop

                    if next_space == " " and nomsu\trailing_line_len! > MAX_LINE
                        next_space = "\n.."
                nomsu\append pop_comments(pos, '\n')
                return nomsu

            when "EscapedNomsu"
                inline_nomsu = @tree_to_inline_nomsu tree
                if #tostring(inline_nomsu) <= MAX_LINE
                    return inline_nomsu
                nomsu = recurse(tree[1])
                switch tree[1].type
                    when "List", "Dict", "Text", "Block"
                        return NomsuCode tree.source, "\\", nomsu
                    else
                        return NomsuCode tree.source, "\\(..)\n    ", pop_comments(tree.source.start), nomsu

            when "Block"
                nomsu = NomsuCode(tree.source, pop_comments(tree.source.start))
                for i, line in ipairs tree
                    nomsu\append pop_comments(line.source.start, '\n')
                    line_nomsu = recurse(line)
                    nomsu\append line_nomsu
                    if i < #tree
                        nomsu\append(line_nomsu\is_multiline! and "\n\n" or "\n")
                nomsu\append pop_comments(tree.source.stop, '\n')
                return options.top and nomsu or NomsuCode(tree.source, ":\n    ", nomsu)

            when "Text"
                inline_version = @tree_to_inline_nomsu(tree)
                if inline_version and #tostring(inline_version) <= MAX_LINE
                    return inline_version
                make_text = (tree)->
                    nomsu = NomsuCode(tree.source)
                    for i, bit in ipairs tree
                        if type(bit) == 'string'
                            bit = Parser.escape(bit)
                            bit_lines = files.get_lines(bit)
                            for j, line in ipairs bit_lines
                                if j > 1 then nomsu\append "\n"
                                line = gsub(line, "\\", "\\\\")
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
                                        if #remainder > 0 then nomsu\append "\\\n.."
                                else
                                    nomsu\append line
                        elseif bit.type == "Text"
                            nomsu\append make_text(bit)
                        else
                            interp_nomsu = @tree_to_inline_nomsu(bit)
                            if nomsu\trailing_line_len! + #tostring(interp_nomsu) <= MAX_LINE
                                switch bit.type
                                    when "Var"
                                        if type(tree[i+1]) == 'string' and not match(tree[i+1], "^[ \n\t,.:;#(){}[%]]")
                                            interp_nomsu\parenthesize!
                                    when "List", "Dict"
                                        nomsu\append "\\", interp_nomsu
                                    else
                                        nomsu\append "\\(", interp_nomsu, ")"
                            else
                                interp_nomsu = recurse(bit)
                                if bit.type != "List" and bit.type != "Dict" and bit.type != "Text" and bit.type != "Block"
                                    nomsu\append "\\(..)\n    ", interp_nomsu
                                else
                                    nomsu\append "\\", interp_nomsu
                                if i < #tree
                                    nomsu\append "\n.."
                    return nomsu
                return NomsuCode(tree.source, '".."\n    ', make_text(tree))

            when "List"
                inline_version = @tree_to_inline_nomsu tree
                if inline_version and #tostring(inline_version) <= MAX_LINE
                    return inline_version
                assert #tree > 0
                nomsu = NomsuCode(tree.source, pop_comments(tree[1].source.start))
                for i, item in ipairs tree
                    item_nomsu = @tree_to_inline_nomsu(item)
                    item_nomsu\parenthesize! if item.type == "Block"
                    if nomsu\trailing_line_len! + #tostring(item_nomsu) <= MAX_LINE
                        nomsu\append ", " if nomsu\trailing_line_len! > 0
                        nomsu\append item_nomsu
                    else
                        item_nomsu = recurse(item)
                        switch item.type
                            when "List", "Dict", "Text", "Block", "EscapedNomsu"
                                nomsu\append item_nomsu
                            else
                                nomsu\append "(..)\n    ", item_nomsu
                        nomsu\append "\n" if i < #tree
                nomsu\append pop_comments(tree.source.stop, '\n')
                return NomsuCode(tree.source, "[..]\n    ", nomsu)
            
            when "Dict"
                inline_version = @tree_to_inline_nomsu tree
                if inline_version and #tostring(inline_version) <= MAX_LINE
                    return inline_version
                assert #tree > 0
                nomsu = NomsuCode(tree.source, pop_comments(tree[1].source.start))
                for i, item in ipairs tree
                    item_nomsu = @tree_to_inline_nomsu(item)
                    item_nomsu\parenthesize! if item.type == "Block"
                    if nomsu\trailing_line_len! + #tostring(item_nomsu) <= MAX_LINE
                        nomsu\append ", " if nomsu\trailing_line_len! > 0
                        nomsu\append item_nomsu
                    else
                        item_nomsu = recurse(item)
                        switch item.type
                            when "List", "Dict", "Text", "Block", "EscapedNomsu"
                                nomsu\append item_nomsu
                            else
                                nomsu\append "(..)\n    ", item_nomsu
                        nomsu\append "\n" if i < #tree
                nomsu\append pop_comments(tree.source.stop, '\n')
                return NomsuCode(tree.source, "{..}\n    ", nomsu)
            
            when "DictEntry"
                inline_version = @tree_to_inline_nomsu tree
                if #tostring(inline_version) <= MAX_LINE
                    return inline_version
                key, value = tree[1], tree[2]
                key_nomsu = if key.type == "Text" and #key == 1 and Parser.is_identifier(key[1])
                    NomsuCode(key.source, key[1])
                else @tree_to_inline_nomsu(key)
                key_nomsu\parenthesize! if key.type == "Action" or key.type == "Block"
                value_nomsu = recurse(value)
                if value.type == "List" or value.type == "Dict" or value.type == "Text" or value.type == "Block"
                    return NomsuCode tree.source, key_nomsu, ": ", value_nomsu
                else
                    return NomsuCode tree.source, key_nomsu, ": (..)\n    ", value_nomsu
            
            when "IndexChain", "Number", "Var"
                return @tree_to_inline_nomsu tree
            
            else
                error("Unknown type: #{tree.type}")

return NomsuCompiler
