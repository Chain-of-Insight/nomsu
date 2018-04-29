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
lpeg.setmaxstack 10000
{:P,:R,:V,:S,:Cg,:C,:Cp,:B,:Cmt} = lpeg
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
        contents = file\read("a")
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
    return io.popen("find \""..path.."\" -type f -name \"*.nom\"")\lines!

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
        if type(i) == 'number' then return string.sub(@, i, i)
        elseif type(i) == 'table' then return string.sub(@, i[1], i[2])
        else return string[i]
    -- Can't use this because it breaks some LPEG stuff
    --STRING_METATABLE.__mul = (other)=> string.rep(@, other)

Types = require "nomsu_tree"

NOMSU_DEFS = with {}
    -- Newline supports either windows-style CR+LF or unix-style LF
    .Tuple = (values)->
        return Tuple(table.unpack(values))
    .DictEntry = (k,v) -> Types.DictEntry(k,v)
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
        spaces = @match("[ ]*", start)
        if #spaces <= #nodent-4
            remove(lpeg.userdata.indent_stack)
            return start
    -- If the number of leading space characters is >= the number on the top of the
    -- stack, this pattern matches and does not modify the stack.
    .nodent = P (start)=>
        nodent = lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack]
        if @sub(start, start+#nodent-1) == nodent
            return start + #nodent

    .error = (src,pos,err_msg)->
        --src = tostring(FILE_CACHE[lpeg.userdata.source_code.source.filename])
        if src\sub(pos,pos)\match("[\r\n]")
            pos += #src\match("[ \t\n\r]*", pos)
        line_no = 1
        text_loc = lpeg.userdata.source_code.source\sub(pos,pos)
        line_no = text_loc\get_line_number!
        src = FILE_CACHE[text_loc.filename]
        prev_line = src\sub(LINE_STARTS[src][line_no-1] or 1, LINE_STARTS[src][line_no]-2)
        err_line = src\sub(LINE_STARTS[src][line_no], (LINE_STARTS[src][line_no+1] or 0)-2)
        next_line = src\sub(LINE_STARTS[src][line_no+1] or -1, (LINE_STARTS[src][line_no+2] or 0)-2)
        pointer = ("-")\rep(pos-LINE_STARTS[src][line_no]) .. "^"
        err_msg = (err_msg or "Parse error").." in #{lpeg.userdata.source_code.source.filename} on line #{line_no}:\n"
        err_msg ..="\n#{prev_line}\n#{err_line}\n#{pointer}\n#{next_line}\n"
        error(err_msg)

setmetatable(NOMSU_DEFS, {__index:(key)=>
    make_node = (start, value, stop)->
        if type(value) == 'table' then error("Not a tuple: #{repr value}")-- = Tuple(value)
        --source = lpeg.userdata.source_code.source\sub(start,stop-1)
        source = lpeg.userdata.source_code.source
        start += source.start-1
        stop += source.start-1
        source = Source(source.filename, start, stop-1)
        node = Types[key](value, source)
        return node
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
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- ({} %3 {}) -> %2"
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
        @use_stack = {}
        @file_metadata = setmetatable({}, {__mode:"k"})
        @action_metadata = setmetatable({}, {__mode:"k"})

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
        for k,v in pairs(Types) do @environment[k] = v
        @environment.Tuple = Tuple
        @environment.Lua = Lua
        @environment.Nomsu = Nomsu
        @environment.Source = Source
        @environment.ACTIONS = setmetatable({}, {__index:(key)=>
            error("Attempt to run undefined action: #{key}", 0)
        })
        @environment.LOADED = {}
        @environment.Types = Types
        @initialize_core!
    
    define_action: (signature, source, fn)=>
        if type(fn) != 'function'
            error 'function', "Bad fn: #{repr fn}"
        if type(signature) == 'string'
            signature = {signature}
        elseif type(signature) != 'table' or signature.type != nil
            error("Invalid signature, expected list of strings, but got: #{repr signature}", 0)
        stubs = @get_stubs_from_signature signature
        stub_args = @get_args_from_signature signature

        fn_info = debug_getinfo(fn, "u")
        local fn_arg_positions, arg_orders
        unless fn_info.isvararg
            fn_arg_positions = {debug.getlocal(fn, i), i for i=1,fn_info.nparams}
            arg_orders = {} -- Map from stub -> index where each arg in the stub goes in the function call
        for sig_i=1,#stubs
            stub, args = stubs[sig_i], stub_args[sig_i]
            @environment.ACTIONS[stub] = fn
            unless fn_info.isvararg
                arg_positions = [fn_arg_positions[a] for a in *args]
                -- TODO: better error checking?
                --if #arg_positions != #args
                --    error("Mismatch in args between lua function's #{repr fn_arg_positions} and stub's #{repr args} for #{repr stub}", 0)
                arg_orders[stub] = arg_positions
        
        @action_metadata[fn] = {
            :fn, :source, aliases:stubs, :arg_orders,
            arg_positions:fn_arg_positions, def_number:@@def_number,
        }

    define_compile_action: (signature, source, fn, src)=>
        @define_action(signature, source, fn)
        @action_metadata[fn].compile_time = true

    serialize_defs: (scope=nil, after=nil)=>
        -- TODO: repair
        error("Not currently functional.", 0)

    -- TODO: figure out whether indent/dedent should affect first line
    dedent: (code)=>
        unless code\find("\n")
            return code
        spaces, indent_spaces = math.huge, math.huge
        for line in code\gmatch("\n([^\n]*)")
            if line\match("^%s*#.*") or line\match("^%s*$")
                continue -- skip comments and blank lines
            elseif s = line\match("^(%s*)%.%..*")
                spaces = math.min(spaces, #s)
            elseif s = line\match("^(%s*)%S.*")
                indent_spaces = math.min(indent_spaces, #s)
        if spaces != math.huge and spaces < indent_spaces
            return (code\gsub("\n"..(" ")\rep(spaces), "\n"))
        elseif indent_spaces != math.huge
            return (code\gsub("\n"..(" ")\rep(indent_spaces), "\n    "))
        else return code

    indent: (code, levels=1)=>
        return code\gsub("\n","\n"..("    ")\rep(levels))

    parse: (nomsu_code)=>
        if type(nomsu_code) == 'string'
            _nomsu_chunk_counter += 1
            filename = "<nomsu chunk ##{_nomsu_chunk_counter}>.nom"
            FILE_CACHE[filename] = nomsu_code
            nomsu_code = Nomsu(filename, nomsu_code)
        userdata = {
            source_code:nomsu_code, indent_stack: {""}
        }

        old_userdata, lpeg.userdata = lpeg.userdata, userdata
        tree = NOMSU_PATTERN\match(tostring(nomsu_code))
        lpeg.userdata = old_userdata
        
        assert tree, "In file #{colored.blue filename} failed to parse:\n#{colored.onyellow colored.black nomsu_code}"
        return tree

    _nomsu_chunk_counter = 0
    run: (nomsu_code, compile_fn=nil)=>
        if #nomsu_code == 0 then return nil
        tree = @parse(nomsu_code)
        assert tree, "Failed to parse: #{nomsu_code}"
        assert tree.type == "File", "Attempt to run non-file: #{tree.type}"
        lua = tree\as_lua(@)
        lua\convert_to_statements!
        lua\declare_locals!
        lua\prepend "-- File: #{nomsu_code.source or ""}\n"
        if compile_fn
            compile_fn(lua)
        return @run_lua(lua)

    run_file: (filename, compile_fn=nil)=>
        ret = nil
        for filename in all_files(filename)
            if filename\match("%.lua$")
                file = assert(FILE_CACHE[filename], "Could not find file: #{filename}")
                ret = @run_lua(Lua(Source(filename), file))
            elseif filename\match("%.nom$") or filename\match("^/dev/fd/[012]$")
                if not @skip_precompiled -- Look for precompiled version
                    lua_filename = filename\gsub("%.nom$", ".lua")
                    file = FILE_CACHE[lua_filename]
                    if file
                        ret = @run_lua(Lua(Source(filename), file))
                        continue
                file = file or FILE_CACHE[filename]
                if not file
                    error("File does not exist: #{filename}", 0)
                ret = @run(Nomsu(Source(filename), file), compile_fn)
            else
                error("Invalid filetype for #{filename}", 0)
        return ret
    
    use_file: (filename)=>
        loaded = @environment.LOADED
        if not loaded[filename]
            ret = nil
            for filename in all_files(filename)
                if not loaded[filename]
                    for i,f in ipairs @use_stack
                        if f == filename
                            loop = [@use_stack[j] for j=i,#@use_stack]
                            insert loop, filename
                            error("Circular import, this loops forever: #{concat loop, " -> "}")
                    insert @use_stack, filename
                    loaded[filename] = @run_file(filename) or true
                    ret = loaded[filename]
                    remove @use_stack
            loaded[filename] = ret
        return loaded[filename]

    run_lua: (lua)=>
        assert(type(lua) != 'string', "Attempt to run lua string instead of Lua (object)")
        lua_string = tostring(lua)
        --metadata = lua\make_offset_table!
        --LUA_METADATA[metadata.lua_filename] = metadata
        if rawget(FILE_CACHE, lua.source.filename) == nil
            FILE_CACHE[lua.source.filename] = lua_string
        if rawget(FILE_CACHE, lua.source) == nil
            FILE_CACHE[lua.source] = lua_string
            
        run_lua_fn, err = load(lua_string, filename, "t", @environment)
        if not run_lua_fn
            n = 1
            fn = ->
                n = n + 1
                ("\n%-3d|")\format(n)
            line_numbered_lua = "1  |"..lua_string\gsub("\n", fn)
            error("Failed to compile generated code:\n#{colored.bright colored.blue colored.onblack line_numbered_lua}\n\n#{err}", 0)
        return run_lua_fn!
    
    tree_to_value: (tree)=>
        -- Special case for text literals
        if tree.type == 'Text' and #tree.value == 1 and type(tree.value[1]) == 'string'
            return tree.value[1]
        lua = Lua(tree.source, "return ",tree\as_lua(@),";")
        return @run_lua(lua)

    value_to_nomsu: (value)=>
        switch type(value)
            when "nil"
                return "(nil)"
            when "bool"
                return value and "(yes)" or "(no)"
            when "number"
                -- TODO: support NaN, inf, etc.?
                return repr(value)
            when "table"
                if is_list(value)
                    return "[#{concat [@value_to_nomsu(v) for v in *value], ", "}]"
                else
                    return "{#{concat ["#{@value_to_nomsu(k)}:#{@value_to_nomsu(v)}" for k,v in pairs(value)], ", "}}"
            when "string"
                if value == "\n"
                    return "'\\n'"
                elseif not value\find[["]] and not value\find"\n" and not value\find"\\"
                    return "\""..value.."\""
                else
                    -- TODO: This might fail if it's being put inside a list or something
                    return '".."\n    '..(@indent value)
            else
                error("Unsupported value_to_nomsu type: #{type(value)}", 0)

    walk_tree: (tree, depth=0)=>
        coroutine.yield(tree, depth)
        return unless Types.is_node(tree)
        switch tree.type
            when "List", "File", "Block", "Action", "Text", "IndexChain"
                for v in *tree.value
                    @walk_tree(v, depth+1)
            when "Dict"
                for e in *tree.value
                    @walk_tree(e.key, depth+1)
                    @walk_tree(e.value, depth+1)
            else @walk_tree(tree.value, depth+1)
        return nil

    print_tree: (tree)=>
        io.write(colors.bright..colors.green)
        for node,depth in coroutine.wrap(-> @walk_tree tree)
            if Types.is_node(node)
                print("#{("    ")\rep(depth)}#{node.type}:")
            else
                print(("    ")\rep(depth)..repr(node))
        io.write(colors.reset)
    
    tree_to_str: (tree)=>
        bits = {}
        for node,depth in coroutine.wrap(-> @walk_tree tree)
            if Types.is_node(node)
                insert bits, ("#{("    ")\rep(depth)}#{node.type}:")
            else
                insert bits, (("    ")\rep(depth)..repr(node))
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

    tree_map: (tree, fn)=>
        -- Return a new tree with fn mapped to each node. If fn provides a replacement,
        -- use that and stop recursing, otherwise recurse.
        unless Types.is_node(tree) then return tree
        replacement = fn(tree)
        if replacement != nil
            return replacement
        switch tree.type
            when "File", "Nomsu", "Block", "List", "Action", "Text", "IndexChain"
                new_values, is_changed = {}, false
                for i,old_value in ipairs(tree.value)
                    new_value = type(old_value) != "string" and @tree_map(old_value, fn) or nil
                    if new_value != nil and new_value != old_value
                        is_changed = true
                        new_values[i] = new_value
                    else
                        new_values[i] = old_value
                if is_changed
                    return tree\with_value Tuple(table.unpack(new_values))
                
            when "Dict"
                new_values, is_changed = {}, false
                for i,e in ipairs tree.value
                    new_key = @tree_map(e.key, fn)
                    new_value = @tree_map(e.value, fn)
                    if (new_key != nil and new_key != e.key) or (new_value != nil and new_value != e.value)
                        is_changed = true
                        new_values[i] = DictEntry(new_key, new_value)
                    else
                        new_values[i] = e
                if is_changed
                    return tree\with_value Tuple(table.unpack(new_values))
            when nil -- Raw table, probably from one of the .value of a multi-value tree (e.g. List)
                error("Invalid tree: #{repr tree}")

        return tree

    tree_with_replaced_vars: (tree, replacements)=>
        return @tree_map tree, (t)->
            if t.type == "Var"
                id = tostring(t\as_lua(self))
                if replacements[id] != nil
                    return replacements[id]

    tree_to_stub: (tree)=>
        if tree.type != "Action" then error "Tried to get stub from non-functioncall tree: #{tree.type}", 0
        return concat([(t.type == "Word" and t.value or "%") for t in *tree.value], " ")

    tree_to_named_stub: (tree)=>
        if tree.type != "Action" then error "Tried to get stub from non-functioncall tree: #{tree.type}", 0
        return concat([(t.type == "Word" and t.value or "%#{t.value}") for t in *tree.value], " ")

    stub_defs = {
        space:(P(' ') + P('\n..'))^0
        word:(NOMSU_DEFS.ident_char^1 + NOMSU_DEFS.operator^1)
        varname:(R('az','AZ','09') + P('_') + NOMSU_DEFS.utf8_char)^0
    }
    stub_pattern = re.compile("{~ (%space->'') (('%' (%varname->'')) / %word)? ((%space->' ') (('%' (%varname->'')) / %word))* (%space->'') ~}", stub_defs)
    get_stubs_from_signature: (signature)=>
        if type(signature) != 'table' or signature.type
            error("Invalid signature: #{repr signature}", 0)
        stubs = {}
        for i,alias in ipairs(signature)
            if type(alias) != 'string'
                error("Expected entries in signature to be strings, not #{type(alias)}s like: #{repr alias}\nsignature: #{repr signature}", 0)
            stubs[i] = stub_pattern\match(alias)
            unless stubs[i]
                error("Failed to match stub pattern on alias: #{repr alias}")
        return stubs

    var_pattern = re.compile("{| %space ((('%' {%varname}) / %word) %space)+ |}", stub_defs)
    get_args_from_signature: (signature)=>
        if type(signature) != 'table' or signature.type
            error("Invalid signature: #{repr signature}", 0)
        stub_args = {}
        for i,alias in ipairs(signature)
            if type(alias) != 'string'
                error("Invalid type for signature: #{type(alias)} for:\n#{repr alias}", 0)
            args = var_pattern\match(alias)
            unless args
                error("Failed to match arg pattern on alias: #{repr alias}", 0)
            for j=1,#args do args[j] = @var_to_lua_identifier(args[j])
            stub_args[i] = args
        return stub_args

    var_to_lua_identifier: (var)=>
        -- Converts arbitrary nomsu vars to valid lua identifiers by replacing illegal
        -- characters with escape sequences
        if Types.Var\is_instance(var)
            var = var.value
        "_"..(var\gsub "%W", (verboten)->
            if verboten == "_" then "__" else ("_%x")\format(verboten\byte!))
    
    initialize_core: =>
        -- Sets up some core functionality
        get_line_no = -> "nomsu.moon:#{debug_getinfo(2).currentline}"
        nomsu = self
        @define_compile_action "immediately %block", get_line_no!, (_block)=>
            lua = _block\as_lua(nomsu)
            lua\convert_to_statements!
            lua\declare_locals!
            nomsu\run_lua(lua)
            return Lua(@source, "if IMMEDIATE then\n    ", lua, "\nend")

        add_lua_bits = (lua, code)->
            if code.type != "Text"
                lua\append ", ", code\as_lua(nomsu)
                return
            for bit in *code.value
                lua\append ", "
                if type(bit) == "string"
                    lua\append repr(bit)
                else
                    bit_lua = bit\as_lua(nomsu)
                    unless bit_lua.is_value
                        line, src = bit.source\get_line!, bit.source\get_text!
                        error "#{line}: Cannot use #{colored.yellow src} as a string interpolation value, since it's not an expression."
                    lua\append bit_lua

        @define_compile_action "Lua %code", get_line_no!, (_code)=>
            lua = Lua.Value(@source, "Lua(", tostring(_code.source))
            add_lua_bits(lua, _code)
            lua\append ")"
            return lua

        @define_compile_action "Lua %source %code", get_line_no!, (_source, _code)=>
            lua = Lua.Value(@source, "Lua(", _source\as_lua(nomsu))
            add_lua_bits(lua, _code)
            lua\append ")"
            return lua

        @define_compile_action "Lua value %code", get_line_no!, (_code)=>
            lua = Lua.Value(@source, "Lua.Value(", tostring(_code.source))
            add_lua_bits(lua, _code)
            lua\append ")"
            return lua

        @define_compile_action "Lua value %source %code", get_line_no!, (_source, _code)=>
            lua = Lua.Value(@source, "Lua.Value(", _source\as_lua(nomsu))
            add_lua_bits(lua, _code)
            lua\append ")"
            return lua

        @define_compile_action "lua> %code", get_line_no!, (_code)=>
            if _code.type != "Text"
                return Lua.Value @source, "nomsu:run_lua(Lua(",repr(_code.source),
                    ", ",repr(tostring(_code\as_lua(nomsu))),"))"

            lua = Lua(_code.source)
            for bit in *_code.value
                if type(bit) == "string"
                    lua\append bit
                else
                    bit_lua = bit\as_lua(nomsu)
                    unless bit_lua.is_value
                        line, src = bit.source\get_line!, bit.source\get_text!
                        error "#{line}: Cannot use #{colored.yellow src} as a string interpolation value, since it's not an expression.", 0
                    lua\append bit_lua
            return lua

        @define_compile_action "=lua %code", get_line_no!, (_code)=>
            if _code.type != "Text"
                return Lua.Value @source, "nomsu:run_lua(Lua(",repr(_code.source),
                    ", ",repr(tostring(_code\as_lua(nomsu))),"))"

            lua = Lua.Value(@source)
            for bit in *_code.value
                if type(bit) == "string"
                    lua\append bit
                else
                    bit_lua = bit\as_lua(nomsu)
                    unless lua.is_value
                        line, src = bit.source\get_line!, bit.source\get_text!
                        error "#{line}: Cannot use #{colored.yellow src} as a string interpolation value, since it's not an expression.", 0
                    lua\append bit_lua
            return lua

        @define_compile_action "!! code location !!", get_line_no!, =>
            return Lua.Value(@source, repr(tostring(@source)))

        @define_action "run file %filename", get_line_no!, (_filename)->
            return nomsu\run_file(_filename)

        @define_compile_action "use %path", get_line_no!, (_path)=>
            path = nomsu\tree_to_value(_path)
            nomsu\use_file(path)
            return Lua(@source, "nomsu:use_file(#{repr path});")

-- Only run this code if this file was run directly with command line arguments, and not require()'d:
if arg and debug_getinfo(2).func != require
    export colors
    colors = require 'consolecolors'
    parser = re.compile([[
        args <- {| (flag ";")* {:inputs: {| ({file} ";")* |} :} |} ";"? !.
        flag <-
            {:interactive: ("-i" -> true) :}
          / {:verbose: ("-v" -> true) :}
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

Usage: (lua nomsu.lua | moon nomsu.moon) [-i] [-O] [-f] [-s] [--help] [-o output] [-p print_file] file1 file2...

OPTIONS
    -i Run the compiler in interactive mode (REPL)
    -O Run the compiler in optimized mode (use precompiled .lua versions of Nomsu files, when available)
    -f Auto-format the given Nomsu file and print the result.
    -s Check the program for syntax errors.
    -v Verbose mode.
    -h/--help Print this message.
    -o <file> Output the compiled Lua file to the given file (use "-" to output to stdout; if outputting to stdout and -p is not specified, -p will default to /dev/null)
    -p <file> Print to the specified file instead of stdout.
    <input> Input file can be "-" to use stdin.
]=]
        os.exit!

    nomsu = NomsuCompiler!

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
            if metadata = nomsu.action_metadata[info.func]
                info.name = metadata.aliases[1]
                [=[
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
        io.stderr\flush!
    
    run = ->
        if args.verbose
            nomsu.debug = true
    
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
        compile_fn = nil
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

        if output_file
            compile_fn = (code)->
                output_file\write("local IMMEDIATE = true;\n"..tostring(code))
                output_file\flush!

        for input in *args.inputs
            if args.syntax
                -- Check syntax:
                for input_file in all_files(input)
                    nomsu\parse(io.open(input_file)\read("*a"))
                if print_file
                    print_file\write("All files parsed successfully!\n")
                    print_file\flush!
            elseif args.format
                -- Auto-format
                for input_file in all_files(input)
                    tree = nomsu\parse(io.open(input_file)\read("*a"))
                    formatted = tostring(tree\as_nomsu!)
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

    --ProFi = require 'ProFi'
    --ProFi\start()
    if ldt = require('ldt')
        ldt.guard run
    else xpcall(run, err_hand)
    --ProFi\stop()
    --ProFi\writeReport( 'MyProfilingReport.txt' )

return NomsuCompiler
