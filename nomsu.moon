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
--        lua nomsu.lua your_file.nom
lpeg = require 'lpeg'
re = require 're'
lpeg.setmaxstack 10000
utils = require 'utils'
{:repr, :stringify, :equivalent} = utils
export colors, colored
colors = require 'consolecolors'
colored = setmetatable({}, {__index:(_,color)-> ((msg)-> colors[color]..tostring(msg or '')..colors.reset)})
{:insert, :remove, :concat} = table
unpack or= table.unpack
{:match, :sub, :rep, :gsub, :format, :byte, :match, :find} = string
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
AST = require "nomsu_tree"
parse = require("parser")
STDIN, STDOUT, STDERR = "/dev/fd/0", "/dev/fd/1", "/dev/fd/2"
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
]], nl:lpeg.P("\r")^-1 * lpeg.P("\n"))
get_lines = re.compile([[
    lines <- {| line (%nl line)* |}
    line <- {[^%nl]*}
]], nl:lpeg.P("\r")^-1 * lpeg.P("\n"))
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
export pos_to_line
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

-- List and Dict classes to provide basic equality/tostring functionality for the tables
-- used in Nomsu. This way, they retain a notion of whether they were originally lists or dicts.
_list_mt =
    __eq:equivalent
    -- Could consider adding a __newindex to enforce list-ness, but would hurt performance
    __tostring: =>
        "["..concat([repr(b) for b in *@], ", ").."]"
list = (t)-> setmetatable(t, _list_mt)

_dict_mt =
    __eq:equivalent
    __tostring: =>
        "{"..concat(["#{repr(k)}: #{repr(v)}" for k,v in pairs @], ", ").."}"
dict = (t)-> setmetatable(t, _dict_mt)

MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value
NomsuCompiler = setmetatable({}, {__index: (k)=> if _self = rawget(@, "self") then _self[k] else nil})
with NomsuCompiler
    ._ENV = NomsuCompiler
    .nomsu = NomsuCompiler
    .parse = (...)=> parse(...)

    -- Discretionary/convenience stuff
    to_add = {
        repr:repr, stringify:stringify, utils:utils, lpeg:lpeg, re:re,
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
            unless _path.type == 'Text' and #_path == 1 and type(_path[1]) == 'string'
                return LuaCode(tree.source, "nomsu:run_file(#{@compile(_path)});")
            path = _path[1]
            @run_file(path)
            return LuaCode(tree.source, "nomsu:run_file(#{repr path});")
    }, {
        __index: (stub)=>
            if math_expression\match(stub)
                return @["# compile math expr #"]
    }

    .run = (to_run, source=nil)=>
        tree = if AST.is_syntax_tree(to_run) then tree else @parse(to_run, source or to_run.source)
        if tree == nil -- Happens if pattern matches, but there are no captures, e.g. an empty string
            return nil
        if tree.type == "FileChunks"
            -- Each chunk's compilation is affected by the code in the previous chunks
            -- (typically), so each chunk needs to compile and run before the next one
            -- compiles.
            ret = nil
            all_lua = {}
            for chunk in *tree
                lua = @compile(chunk)\as_statements!
                lua\declare_locals!
                lua\prepend "-- File: #{chunk.source or ""}\n"
                insert all_lua, tostring(lua)
                ret = @run_lua(lua)
            if @on_compile
                self.on_compile(concat(all_lua, "\n"), (source or to_run.source).filename)
            return ret
        else
            lua = @compile(tree, compile_actions)\as_statements!
            lua\declare_locals!
            lua\prepend "-- File: #{source or to_run.source or ""}\n"
            if @on_compile
                self.on_compile(lua, (source or to_run.source).filename)
            return @run_lua(lua)

    _running_files = {} -- For detecting circular imports
    .run_file = (filename)=>
        if @LOADED[filename]
            return @LOADED[filename]
        ret = nil
        for filename in all_files(filename)
            if ret = @LOADED[filename]
                continue

            -- Check for circular import
            for i,running in ipairs _running_files
                if running == filename
                    loop = [_running_files[j] for j=i,#_running_files]
                    insert loop, filename
                    error("Circular import, this loops forever: #{concat loop, " -> "}...")

            insert _running_files, filename
            if match(filename, "%.lua$")
                file = assert(FILE_CACHE[filename], "Could not find file: #{filename}")
                ret = @run_lua file, Source(filename, 1, #file)
            elseif match(filename, "%.nom$") or match(filename, "^/dev/fd/[012]$")
                ran_lua = if not @skip_precompiled -- Look for precompiled version
                    lua_filename = gsub(filename, "%.nom$", ".lua")
                    if file = FILE_CACHE[lua_filename]
                        ret = @run_lua file, Source(lua_filename, 1, #file)
                        true
                unless ran_lua
                    file = file or FILE_CACHE[filename]
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
                [format("%3d|%s",i,line) for i, line in ipairs get_lines\match(lua_string)],
                "\n")
            error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack line_numbered_lua}\n\n#{err}", 0)
        source_key = tostring(source or lua.source)
        unless SOURCE_MAP[source_key]
            map = {}
            offset = 1
            source or= lua.source
            nomsu_str = tostring(FILE_CACHE[source.filename]\sub(source.start, source.stop))
            lua_line = 1
            nomsu_line = pos_to_line(nomsu_str, source.start)
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
            SOURCE_MAP[source_key] = map

        return run_lua_fn!

    .compile = (tree)=>
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
                LuaCode.Value tree.source, make_tree(tree[1])
            
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
                        src = '    '..gsub(tostring(@tree_to_nomsu(bit)), '\n','\n    ')
                        line = "#{bit.source.filename}:#{pos_to_line(FILE_CACHE[bit.source.filename], bit.source.start)}"
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
                LuaCode.Value(tree.source, tostring(tree[1]))

            when "Var"
                LuaCode.Value(tree.source, string.as_lua_id(tree[1]))

            when "FileChunks"
                error("Cannot convert FileChunks to a single block of lua, since each chunk's "..
                    "compilation depends on the earlier chunks")

            else
                error("Unknown type: #{tree.type}")

    .tree_to_nomsu = (tree, inline=false, can_use_colon=false)=>
        switch tree.type
            when "FileChunks"
                return nil if inline
                nomsu = NomsuCode(tree.source)
                nomsu\concat_append [@tree_to_nomsu(c) for c in *tree], "\n#{("~")\rep(80)}\n"
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
                            arg_nomsu = @tree_to_nomsu(bit,true)
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
                                        arg_nomsu = NomsuCode(bit.source, "(..)\n    ", arg_nomsu)
                                    else
                                        arg_nomsu = NomsuCode(bit.source, "\n    ", arg_nomsu)
                                
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
                    return nomsu and NomsuCode tree.source, "\\:\n    ", nomsu
                return nomsu and NomsuCode tree.source, "\\(", nomsu, ")"

            when "Block"
                if inline
                    nomsu = NomsuCode(tree.source)
                    for i,line in ipairs tree
                        if i > 1
                            nomsu\append "; "
                        line_nomsu = @tree_to_nomsu(line,true)
                        return nil unless line_nomsu
                        nomsu\append line_nomsu
                    return nomsu
                nomsu = NomsuCode(tree.source)
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
                    nomsu = NomsuCode(tree.source, '"')
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
                    nomsu = NomsuCode(tree.source, '".."\n    ')
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
                    nomsu = NomsuCode(tree.source, "[")
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
                    nomsu = NomsuCode(tree.source, "[..]")
                    line = NomsuCode(tree.source, "\n    ")
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
                                line = NomsuCode(line.source, "\n    ")
                            line\append item_nomsu
                    if #line.bits > 1
                        nomsu\append line
                    return nomsu
            
            when "Dict"
                if inline
                    nomsu = NomsuCode(tree.source, "{")
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
                    nomsu = NomsuCode(tree.source, "{..}")
                    line = NomsuCode(tree.source, "\n    ")
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
                                line = NomsuCode(line.source, "\n    ")
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
                else NomsuCode(tree.source, "")
                if inline and not value_nomsu then return nil
                if not value_nomsu
                    return nil if inline
                    value_nomsu = @tree_to_nomsu(value)
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
                    unless bit_nomsu then bit_nomsu = @tree_to_nomsu(bit, true)
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
    

-- Command line interface:
-- Only run this code if this file was run directly with command line arguments, and not require()'d:
if arg and debug.getinfo(2).func != require
    parser = re.compile([[
        args <- {| (flag ";")* {:inputs: {| ({file} ";")* |} :} {:nomsu_args: {| ("--;" ({[^;]*} ";")*)? |} :} ";"? |} !.
        flag <-
            {:interactive: ("-i" -> true) :}
          / {:optimized: ("-O" -> true) :}
          / {:format: ("-f" -> true) :}
          / {:syntax: ("-s" -> true) :}
          / {:print_file: "-p" ";" {file} :}
          / {:compile: ("-c" -> true) :}
          / {:verbose: ("-v" -> true) :}
          / {:help: (("-h" / "--help") -> true) :}
        file <- "-" / [^;]+
    ]], {true: -> true})
    args = concat(arg, ";")..";"
    args = parser\match(args)
    if not args or args.help
        print [=[
Nomsu Compiler

Usage: (lua nomsu.lua | moon nomsu.moon) [-i] [-O] [-v] [-c] [-f] [-s] [--help] [-p print_file] file1 file2... [-- nomsu args...]

OPTIONS
    -i Run the compiler in interactive mode (REPL)
    -O Run the compiler in optimized mode (use precompiled .lua versions of Nomsu files, when available)
    -v Verbose: print compiled lua code
    -c Compile .nom files into .lua files
    -f Auto-format the given Nomsu file and print the result.
    -s Check the program for syntax errors.
    -h/--help Print this message.
    -p <file> Print to the specified file instead of stdout.
    <input> Input file can be "-" to use stdin.
]=]
        os.exit!

    nomsu = NomsuCompiler
    nomsu.arg = args.nomsu_args
    
    run = ->
    
        for i,input in ipairs args.inputs
            if input == "-" then args.inputs[i] = STDIN

        if #args.inputs == 0 and not args.interactive
            args.inputs = {"core"}
            args.interactive = true

        print_file = if args.print_file == "-" then io.stdout
        elseif args.print_file then io.open(args.print_file, 'w')
        else io.stdout

        nomsu.skip_precompiled = not args.optimized
        if print_file == nil
            nomsu.print = ->
        elseif print_file != io.stdout
            nomsu.print = (...)->
                N = select("#",...)
                if N > 0
                    print_file\write(tostring(select(1,...)))
                    for i=2,N
                        print_file\write('\t',tostring(select(1,...)))
                print_file\write('\n')
                print_file\flush!

        input_files = {}
        to_run = {}
        for input in *args.inputs
            for f in all_files(input)
                input_files[#input_files+1] = f
                to_run[f] = true

        nomsu.on_compile = if args.compile or args.verbose
            (code, from_file)->
                if to_run[from_file]
                    if args.verbose
                        io.write(tostring(code), "\n")
                    if args.compile and from_file\match("%.nom$")
                        output_filename = from_file\gsub("%.nom$", ".lua")
                        output_file = io.open(output_filename, 'w')
                        output_file\write(tostring(code))
                        output_file\flush!
                        print ("Compiled %-25s -> %s")\format(from_file, output_filename)
                        output_file\close!
        else nil

        parse_errs = {}
        for filename in *input_files
            if args.syntax
                -- Check syntax:
                file_contents = io.open(filename)\read('*a')
                ok,err = pcall nomsu.parse, nomsu, file_contents, Source(filename, 1, #file_contents)
                if not ok
                    insert parse_errs, err
                elseif print_file
                    print_file\write("Parse succeeded: #{filename}\n")
                    print_file\flush!
            elseif args.format
                -- Auto-format
                file = FILE_CACHE[filename]
                if not file
                    error("File does not exist: #{filename}", 0)
                tree = nomsu\parse(file, Source(filename,1,#file))
                formatted = tostring(nomsu\tree_to_nomsu(tree))
                if print_file
                    print_file\write(formatted, "\n")
                    print_file\flush!
            elseif filename == STDIN
                file = io.input!\read("*a")
                FILE_CACHE.stdin = file
                nomsu\run(file, Source('stdin',1,#file))
            else
                nomsu\run_file(filename)

        if #parse_errs > 0
            io.stderr\write concat(parse_errs, "\n\n")
            io.stderr\flush!
            os.exit(false, true)
        elseif args.syntax
            os.exit(true, true)

        if args.interactive
            -- REPL
            for repl_line=1,math.huge
                io.write(colored.bright colored.yellow ">> ")
                buff = {}
                while true
                    line = io.read("*L")
                    if line == "\n" or not line
                        if #buff > 0
                            io.write("\027[1A\027[2K")
                        break -- Run buffer
                    line = line\gsub("\t", "    ")
                    insert buff, line
                    io.write(colored.dim colored.yellow ".. ")
                if #buff == 0
                    break -- Exit
                
                buff = concat(buff)
                FILE_CACHE["REPL#"..repl_line] = buff
                err_hand = (error_message)->
                    print_err_msg error_message
                ok, ret = xpcall(nomsu.run, err_hand, nomsu, buff, Source("REPL#"..repl_line, 1, #buff))
                if ok and ret != nil
                    print "= "..repr(ret)
                elseif not ok
                    print_err_msg ret
    
    run_safely = require "error_handling"
    run_safely(run)

return NomsuCompiler
