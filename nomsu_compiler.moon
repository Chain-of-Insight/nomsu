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
Files = require 'files'
{:repr, :stringify, :equivalent} = utils
export colors, colored
colors = require 'consolecolors'
colored = setmetatable({}, {__index:(_,color)-> ((msg)-> colors[color]..tostring(msg or '')..colors.reset)})
{:insert, :remove, :concat} = table
unpack or= table.unpack
{:match, :sub, :gsub, :format, :byte, :find} = string
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
AST = require "syntax_tree"
Parser = require("parser")
-- Mapping from source string (e.g. "@core/metaprogramming.nom[1:100]") to a mapping
-- from lua line number to nomsu line number
export SOURCE_MAP
SOURCE_MAP = {}

string.as_lua_id = (str)->
    -- Cut up escape-sequence-like chunks
    str = gsub str, "x([0-9A-F][0-9A-F])", "x\0%1"
    -- Alphanumeric unchanged, spaces to underscores, and everything else to hex escape sequences
    str = gsub str, "%W", (c)->
        if c == ' ' then '_'
        else format("x%02X", byte(c))
    return str

table.map = (fn)=> [fn(v) for _,v in ipairs(@)]
table.fork = (t, values)-> setmetatable(values or {}, {__index:t})
table.copy = (t)-> setmetatable({k,v for k,v in pairs(t)}, getmetatable(t))

-- TODO:
-- consider non-linear codegen, rather than doing thunks for things like comprehensions
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
NomsuCompiler = setmetatable {name:"Nomsu"},
    __index: (k)=> if _self = rawget(@, "self") then _self[k] else nil
    __tostring: => @name
with NomsuCompiler
    .NOMSU_COMPILER_VERSION = 5
    .NOMSU_SYNTAX_VERSION = Parser.version
    .nomsu = NomsuCompiler
    .parse = (...)=> Parser.parse(...)
    .can_optimize = -> false

    -- Discretionary/convenience stuff
    to_add = {
        repr:repr, stringify:stringify, utils:utils, lpeg:lpeg, re:re, Files:Files,
        -- Lua stuff:
        :next, :unpack, :setmetatable, :coroutine, :rawequal, :getmetatable, :pcall,
        :error, :package, :os, :require, :tonumber, :tostring, :string, :xpcall, :module,
        :print, :loadfile, :rawset, :_VERSION, :collectgarbage, :rawget, :rawlen,
        :table, :assert, :dofile, :loadstring, :type, :select, :math, :io, :load,
        :pairs, :ipairs,
        -- Nomsu types:
        :list, :dict,
    }
    if jit then to_add.bit = require('bit')
    elseif _VERSION == "Lua 5.2" then to_add.bit = bit32
    for k,v in pairs(to_add) do NomsuCompiler[k] = v
    for k,v in pairs(AST) do NomsuCompiler[k] = v
    .LuaCode = LuaCode
    .NomsuCode = NomsuCode
    .Source = Source
    .LOADED = {}
    .TESTS = {}
    .AST = AST

    .compile_error = (source, err_format_string, ...)=>
        err_format_string = err_format_string\gsub("%%[^s]", "%%%1")
        file = Files.read(source.filename)
        line_starts = Files.get_line_starts(file)
        line_no = Files.get_line_number(file, source.start)
        line_start = line_starts[line_no]
        src = colored.dim(file\sub(line_start, source.start-1))
        src ..= colored.underscore colored.bright colored.red(file\sub(source.start, source.stop-1))
        end_of_line = (line_starts[Files.get_line_number(file, source.stop) + 1] or 0) - 1
        src ..= colored.dim(file\sub(source.stop, end_of_line-1))
        src = '    '..src\gsub('\n', '\n    ')
        err_msg = err_format_string\format(src, ...)
        error("#{source.filename}:#{line_no}: "..err_msg, 0)

    -- This is a bit of a hack, but this code handles arbitrarily complex
    -- math expressions like 2*x + 3^2 without having to define a single
    -- action for every possibility.
    math_expression = re.compile [[ ([+-] " ")* [0-9]+ (" " [*/^+-] (" " [+-])* " " [0-9]+)+ !. ]]

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
            bit_leading_len = #(bit_lua\match("^[^\n]*"))
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

        ["Lua 1"]: (tree, _code)=>
            return add_lua_string_bits(@, 'statements', _code)
    
        ["Lua value 1"]: (tree, _code)=>
            return add_lua_string_bits(@, 'value', _code)

        ["lua > 1"]: (tree, _code)=>
            if _code.type != "Text"
                return LuaCode tree.source, "nomsu:run_lua(", @compile(_code), ");"
            return add_lua_bits(@, "statements", _code)

        ["= lua 1"]: (tree, _code)=>
            if _code.type != "Text"
                return LuaCode.Value tree.source, "nomsu:run_lua(", @compile(_code), ":as_statements('return '))"
            return add_lua_bits(@, "value", _code)

        ["use 1"]: (tree, _path)=>
            if _path.type == 'Text' and #_path == 1 and type(_path[1]) == 'string'
                path = _path[1]
                for _,f in Files.walk(path)
                    @run_file(f)
            return LuaCode(tree.source, "for i,f in Files.walk(", @compile(_path), ") do nomsu:run_file(f) end")

        ["tests"]: (tree)=> LuaCode.Value(tree.source, "TESTS")
        ["test 1"]: (tree, _body)=>
            test_str = table.concat [tostring(@tree_to_nomsu(line)) for line in *_body], "\n"
            LuaCode tree.source, "TESTS[#{repr(tostring(tree.source))}] = ", repr(test_str)

        ["is jit"]: (tree, _code)=>
            return LuaCode.Value(tree.source, jit and "true" or "false")

        ["Lua version"]: (tree, _code)=>
            return LuaCode.Value(tree.source, repr(_VERSION))
    }, {
        __index: (stub)=>
            if math_expression\match(stub)
                return @["# compile math expr #"]
    }

    .run = (to_run, source=nil, version=nil)=>
        source or= to_run.source or Source(to_run, 1, #to_run)
        if type(source) == 'string' then source = Source\from_string(source)
        if not Files.read(source.filename) then Files.spoof(source.filename, to_run)
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
        -- Filename should be an absolute path, i.e. package.nomsupath will not be searched for it
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
            file = assert(Files.read(filename), "Could not find file: #{filename}")
            ret = @run_lua file, Source(filename, 1, #file)
        elseif match(filename, "%.nom$") or match(filename, "^/dev/fd/[012]$")
            ran_lua = if @.can_optimize(filename) -- Look for precompiled version
                lua_filename = gsub(filename, "%.nom$", ".lua")
                if file = Files.read(lua_filename)
                    ret = @run_lua file, Source(lua_filename, 1, #file)
                    true
            unless ran_lua
                file = Files.read(filename)
                if not file
                    error("Tried to run file that does not exist: #{filename}")
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
                [format("%3d|%s",i,line) for i, line in ipairs Files.get_lines(lua_string)],
                "\n")
            error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack line_numbered_lua}\n\n#{err}", 0)
        source or= lua.source
        source_key = tostring(source)
        unless SOURCE_MAP[source_key]
            map = {}
            file = Files.read(source.filename)
            if not file
                error "Failed to find file: #{source.filename}"
            nomsu_str = file\sub(source.start, source.stop)
            assert type(nomsu_str) == 'string'
            lua_line = 1
            nomsu_line = Files.get_line_number(file, source.start)
            map_sources = (s)->
                if type(s) == 'string'
                    for nl in s\gmatch("\n")
                        map[lua_line] or= nomsu_line
                        lua_line += 1
                else
                    if s.source and s.source.filename == source.filename
                        nomsu_line = Files.get_line_number(file, s.source.start)
                    for b in *s.bits do map_sources(b)
            map_sources(lua)
            map[lua_line] or= nomsu_line
            map[0] = 0
            -- Mapping from lua line number to nomsu line numbers
            SOURCE_MAP[source_key] = map

        return run_lua_fn!

    .compile = (tree)=>
        if tree.version
            if get_version = @['A_'..string.as_lua_id("Nomsu version")]
                if upgrade = @['A_'..string.as_lua_id("1 upgraded from 2 to 3")]
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

                lua = LuaCode.Value(tree.source)
                if tree.target
                    lua\append @compile(tree.target), ":"
                else
                    lua\append "A_"
                lua\append(string.as_lua_id(stub),"(")
                args = {}
                for i, tok in ipairs tree
                    if type(tok) == "string" then continue
                    arg_lua = @compile(tok)
                    unless arg_lua.is_value
                        if tok.type == "Block"
                            @compile_error tok.source,
                                ("Cannot compile action '#{stub}' with a Block as an argument.\n"..
                                 "Maybe there should be a compile-time action with that name that isn't being found?")

                        else
                            @compile_error tok.source,
                                "Cannot use:\n%s\nas an argument to '%s', since it's not an expression, it produces: %s",
                                stub, repr arg_lua
                    insert args, arg_lua
                lua\concat_append args, ", "
                lua\append ")"
                return lua

            when "EscapedNomsu"
                lua = LuaCode.Value tree.source, tree[1].type, "{"
                needs_comma, i = false, 1
                for k,v in pairs(AST.is_syntax_tree(tree[1], "EscapedNomsu") and tree or tree[1])
                    if needs_comma then lua\append ", "
                    else needs_comma = true
                    if k == i
                        i += 1
                    elseif type(k) == 'string' and match(k,"[_a-zA-Z][_a-zA-Z0-9]*")
                        lua\append(k, "= ")
                    else
                        lua\append("[", (AST.is_syntax_tree(k) and @compile(k) or repr(k)), "]= ")
                    if k == "source"
                        lua\append repr(tostring(v))
                    else
                        lua\append(AST.is_syntax_tree(v) and @compile(v) or repr(v))
                lua\append "}"
                return lua
            
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
                        src = '    '..gsub(tostring(@compile(bit)), '\n','\n    ')
                        line = "#{bit.source.filename}:#{Files.get_line_number(Files.read(bit.source.filename), bit.source.start)}"
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
                lua\concat_append([@compile(e) for e in *tree], ", ", ",\n  ")
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
                key_str = match(tostring(key_lua), [=[^["']([a-zA-Z_][a-zA-Z0-9_]*)['"]$]=])
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
                return LuaCode.Value(tree.source, "_", string.as_lua_id(tree[1]))

            when "FileChunks"
                error("Cannot convert FileChunks to a single block of lua, since each chunk's "..
                    "compilation depends on the earlier chunks")

            else
                error("Unknown type: #{tree.type}")

    .tree_to_inline_nomsu = (tree, parenthesize_blocks=false, check=nil, len=0)=>
        recurse = (tree, nomsu=nil, parenthesize_blocks=false)->
            @tree_to_inline_nomsu(tree, parenthesize_blocks, check, len + (nomsu and #tostring(nomsu) or 0))
        switch tree.type
            when "FileChunks"
                error("Cannot inline a FileChunks")

            when "Action"
                nomsu = NomsuCode(tree.source)
                if tree.target
                    nomsu\append @tree_to_inline_nomsu(tree.target), "::"
                for i,bit in ipairs tree
                    if type(bit) == "string"
                        clump_words = (type(tree[i-1]) == 'string' and Parser.is_operator(bit) != Parser.is_operator(tree[i-1]))
                        nomsu\append " " if i > 1 and not clump_words
                        nomsu\append bit
                    else
                        arg_nomsu = recurse(bit, nomsu, parenthesize_blocks or (i == 1 or i < #tree))
                        nomsu\append " " unless arg_nomsu\match("^:") or i == 1
                        arg_nomsu\parenthesize! if bit.type == "Action"
                        nomsu\append arg_nomsu
                    check(len, nomsu, tree) if check
                return nomsu

            when "EscapedNomsu"
                inner_nomsu = recurse(tree[1])
                unless tree[1].type == "List" or tree[1].type == "Dict" or tree[1].type == "Var"
                    inner_nomsu\parenthesize!
                nomsu = NomsuCode(tree.source, "\\", inner_nomsu)
                check(len, nomsu, tree) if check
                return nomsu

            when "Block"
                nomsu = NomsuCode(tree.source, ":")
                check(len, nomsu, tree) if check
                for i,line in ipairs tree
                    nomsu\append(i == 1 and " " or "; ")
                    nomsu\append recurse(line, nomsu, i == 1 or i < #tree)
                    check(len, nomsu, tree) if check
                nomsu\parenthesize! if #tree > 1 or parenthesize_blocks
                return nomsu

            when "Text"
                add_text = (nomsu, tree)->
                    for i, bit in ipairs tree
                        if type(bit) == 'string'
                            escaped = Parser.inline_escape(bit)
                            nomsu\append Parser.inline_escape(bit)
                        elseif bit.type == "Text"
                            add_text(nomsu, bit)
                        else
                            interp_nomsu = recurse(bit, nomsu)
                            if bit.type != "Var" and bit.type != "List" and bit.type != "Dict"
                                interp_nomsu\parenthesize!
                            elseif bit.type == "Var" and type(tree[i+1]) == 'string' and not match(tree[i+1], "^[ \n\t,.:;#(){}[%]]")
                                interp_nomsu\parenthesize!
                            nomsu\append "\\", interp_nomsu
                        check(len, nomsu, tree) if check
                nomsu = NomsuCode(tree.source)
                add_text(nomsu, tree)
                return NomsuCode(tree.source, '"', nomsu, '"')

            when "List", "Dict"
                nomsu = NomsuCode(tree.source, (tree.type == "List" and "[" or "{"))
                for i, item in ipairs tree
                    nomsu\append ", " if i > 1
                    nomsu\append recurse(item, nomsu)
                    check(len, nomsu, tree) if check
                nomsu\append(tree.type == "List" and "]" or "}")
                return nomsu
            
            when "DictEntry"
                key, value = tree[1], tree[2]
                nomsu = if key.type == "Text" and #key == 1 and Parser.is_identifier(key[1])
                    NomsuCode(key.source, key[1])
                else recurse(key)
                nomsu\parenthesize! if key.type == "Action" or key.type == "Block"
                assert(value.type != "Block", "Didn't expect to find a Block as a value in a dict")
                nomsu\append ":"
                if value
                    value_nomsu = recurse(value, nomsu)
                    value_nomsu\parenthesize! if value.type == "Block"
                    nomsu\append value_nomsu
                check(len, nomsu, tree) if check
                return nomsu
            
            when "IndexChain"
                nomsu = NomsuCode(tree.source)
                for i, bit in ipairs tree
                    nomsu\append "." if i > 1
                    local bit_nomsu
                    bit_nomsu = if i > 1 and bit.type == "Text" and #bit == 1 and type(bit[1]) == 'string' and Parser.is_identifier(bit[1])
                        bit[1]
                    else recurse(bit, nomsu)
                    assert bit.type != "Block"
                    if bit.type == "Action" or bit.type == "IndexChain" or (bit.type == "Number" and i < #tree)
                        bit_nomsu\parenthesize!
                    nomsu\append bit_nomsu
                    check(len, nomsu, tree) if check
                return nomsu
            
            when "Number"
                return NomsuCode(tree.source, tostring(tree[1]))

            when "Var"
                return NomsuCode(tree.source, "%", tree[1])
            
            else
                error("Unknown type: #{tree.type}")

    .tree_to_nomsu = (tree, pop_comments=nil)=>
        unless pop_comments
            comment_set = {}
            find_comments = (t)->
                if t.comments and t.source.filename == tree.source.filename
                    comment_set[c] = true for c in *t.comments
                find_comments(x) for x in *t when AST.is_syntax_tree x
            find_comments(tree)
            -- Sort in reversed order so they can be easily popped
            comments = [c for c in pairs comment_set]
            table.sort(comments, (a,b)->(a.pos > b.pos))

            pop_comments = (pos, prefix='', suffix='')->
                nomsu = NomsuCode(tree.source)
                for i=#comments,1,-1
                    break if comments[i].pos > pos
                    comment, comments[i] = comments[i].comment, nil
                    nomsu\append("#"..(gsub(comment, "\n", "\n    ")).."\n")
                    if comment\match("^\n.") then nomsu\append("\n") -- for aesthetics
                return '' if #nomsu.bits == 0
                nomsu\prepend(prefix) unless prefix == ''
                nomsu\append(suffix) unless suffix == ''
                return nomsu

        -- For concision:
        recurse = (t, pos)->
            if pos == nil then pos = 0
            if type(pos) != 'number' then pos = #tostring(pos)\match("[ ]*([^\n]*)$")
            space = MAX_LINE - pos
            local inline
            for prefix, nomsu, tree in coroutine.wrap(-> inline = @tree_to_inline_nomsu(t, false, coroutine.yield))
                len = #tostring(nomsu)
                break if prefix+len > MAX_LINE
                break if tree.type == "Block" and (#tree > 1 or len > 20)
                if tree.type == "Text"
                    -- Disallow inline text if it's got newlines between text, e.g. "hello\nworld"
                    check_for_nl = (tree)->
                        found_nl = false
                        for i,b in ipairs tree
                            return true if type(b) != 'string' and b.type == "Text" and check_for_nl(b)
                            b = b\match('^[\n]*(.*)') if i == 1 and type(b) == 'string'
                            found_nl or= type(b) == 'string' and b\match('\n')
                            return true if found_nl and (type(b) != 'string' or b\match('[^\n]'))
                    break if check_for_nl(tree)
            return inline if inline and #tostring(inline) <= space
            indented = @tree_to_nomsu(t, pop_comments, space)
            if t.type == "Action" and not (tree.type == "Block" or tree.type == "FileChunks")
                indented = NomsuCode(t.source, "(..)\n    ", pop_comments(t.source.start), indented)
            return indented

        switch tree.type
            when "FileChunks"
                nomsu = NomsuCode(tree.source, pop_comments(tree.source.start))
                should_clump = (prev_line, line)->
                    if prev_line.type == "Action" and line.type == "Action"
                        if prev_line.stub == "use 1" then return line.stub == "use 1"
                        if prev_line.stub == "test 1" then return true
                        if line.stub == "test 1" then return false
                    return not recurse(prev_line)\is_multiline!
                for chunk_no, chunk in ipairs tree
                    nomsu\append "\n\n#{("~")\rep(80)}\n\n" if chunk_no > 1
                    nomsu\append pop_comments(chunk.source.start)
                    if chunk.type == "Block"
                        for line_no, line in ipairs chunk
                            if line_no > 1
                                if should_clump(chunk[line_no-1], line)
                                    nomsu\append "\n", pop_comments(line.source.start, '\n')
                                else
                                    nomsu\append "\n\n", pop_comments(line.source.start)
                            nomsu\append @tree_to_nomsu(line, pop_comments)
                        nomsu\append pop_comments(chunk.source.stop, '\n')
                    else
                        nomsu\append recurse(chunk)
                nomsu\append pop_comments(tree.source.stop, '\n')
                nomsu\append('\n') unless nomsu\match("\n$")
                return nomsu

            when "Action"
                pos, next_space = tree.source.start, ''
                nomsu = NomsuCode(tree.source, pop_comments(pos))
                if tree.target
                    nomsu\append @tree_to_nomsu(tree.target), "::"
                for i,bit in ipairs tree
                    if next_space == "\n.." or (next_space == " " and nomsu\trailing_line_len! > MAX_LINE)
                        nomsu\append "\n", pop_comments(pos), '..'
                        next_space = ""

                    if type(bit) == "string"
                        unless type(tree[i-1]) == 'string' and Parser.is_operator(tree[i-1]) != Parser.is_operator(bit)
                            nomsu\append(next_space)
                        nomsu\append bit
                        next_space = ' '
                    elseif bit.type == "Block"
                        nomsu\append(recurse(bit, #nomsu\match('[^\n]*$')))
                        pos = bit.source.stop
                        next_space = inline and " " or "\n.."
                    else
                        nomsu\append next_space
                        bit_nomsu = recurse(bit, #nomsu\match('[^\n]*$'))
                        if bit.type == "Action" and not bit_nomsu\is_multiline!
                            bit_nomsu\parenthesize!
                        nomsu\append bit_nomsu
                        pos = bit.source.stop
                        next_space = bit_nomsu\is_multiline! and "\n.." or " "

                nomsu\append pop_comments(tree.source.stop, '\n')
                return nomsu

            when "EscapedNomsu"
                return NomsuCode tree.source, "\\", recurse(tree[1], 1)

            when "Block"
                nomsu = NomsuCode(tree.source, pop_comments(tree.source.start))
                for i, line in ipairs tree
                    nomsu\append pop_comments(line.source.start, i > 1 and '\n' or '')
                    line_nomsu = recurse(line)
                    nomsu\append line_nomsu
                    if i < #tree
                        nomsu\append(line_nomsu\match('\n[^\n]*\n') and "\n\n" or "\n")
                nomsu\append pop_comments(tree.source.stop, '\n')
                return NomsuCode(tree.source, ":\n    ", nomsu)

            when "Text"
                -- Multi-line text has more generous wrap margins
                max_line = math.floor(1.5*MAX_LINE)
                add_text = (nomsu, tree)->
                    for i, bit in ipairs tree
                        if type(bit) == 'string'
                            bit = Parser.escape(bit)
                            bit_lines = Files.get_lines(bit)
                            for j, line in ipairs bit_lines
                                if j > 1
                                    nomsu\append "\n"
                                elseif #line > 10 and nomsu\trailing_line_len! > max_line
                                    nomsu\append "\\\n.."

                                while #line > 0
                                    space = max_line - nomsu\trailing_line_len!
                                    split = find(line, "[%p%s]", space)
                                    if not split or split > space + 10
                                        split = space + 10
                                    if #line - split < 10
                                        split = #line
                                    bite, line = sub(line, 1, split), sub(line, split+1, -1)
                                    nomsu\append bite
                                    nomsu\append "\\\n.." if #line > 0
                        elseif bit.type == "Text"
                            add_text(nomsu, bit)
                        else
                            nomsu\append "\\"
                            interp_nomsu = recurse(bit, #nomsu\match('[^\n]*$'))
                            unless interp_nomsu\is_multiline!
                                if bit.type == "Var"
                                    if type(tree[i+1]) == 'string' and not match(tree[i+1], "^[ \n\t,.:;#(){}[%]]")
                                        interp_nomsu\parenthesize!
                                elseif bit.type != "List" and bit.type != "Dict"
                                    interp_nomsu\parenthesize!
                            nomsu\append interp_nomsu
                            if interp_nomsu\is_multiline! and i < #tree
                                nomsu\append "\n.."
                nomsu = NomsuCode(tree.source)
                add_text(nomsu, tree)
                if nomsu\is_multiline! and nomsu\match("\n$")
                    nomsu\append '\\("")' -- Need to specify where the text ends
                return NomsuCode(tree.source, '".."\n    ', nomsu)

            when "List", "Dict"
                assert #tree > 0
                nomsu = NomsuCode(tree.source, pop_comments(tree[1].source.start))
                for i, item in ipairs tree
                    nomsu\append(pop_comments(item.source.start)) if nomsu\trailing_line_len! == 0
                    inline_nomsu = @tree_to_inline_nomsu(item)
                    item_nomsu = #tostring(inline_nomsu) <= MAX_LINE and inline_nomsu or recurse(item, #nomsu\match('[^\n]*$'))
                    nomsu\append item_nomsu
                    if i < #tree
                        nomsu\append((item_nomsu\is_multiline! or nomsu\trailing_line_len! + #tostring(item_nomsu) >= MAX_LINE) and '\n' or ', ')
                nomsu\append pop_comments(tree.source.stop, '\n')
                return if tree.type == "List" then
                    NomsuCode(tree.source, "[..]\n    ", nomsu)
                else
                    NomsuCode(tree.source, "{..}\n    ", nomsu)
            
            when "DictEntry"
                key, value = tree[1], tree[2]
                nomsu = if key.type == "Text" and #key == 1 and Parser.is_identifier(key[1])
                    NomsuCode(key.source, key[1])
                else @tree_to_inline_nomsu(key)
                nomsu\parenthesize! if key.type == "Action" or key.type == "Block"
                nomsu\append ": ", recurse(value, #tostring(nomsu))
                return nomsu
            
            when "IndexChain", "Number", "Var"
                return @tree_to_inline_nomsu tree
            
            else
                error("Unknown type: #{tree.type}")

return NomsuCompiler
