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
if jit
    package.cpath = "./luajit_lpeg/?.so;"..package.cpath
    
    export bit32
    bit32 = require('bit')

    _pairs, _ipairs = pairs, ipairs
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
    return io.popen("find -L \""..path.."\" -type f -name \"*.nom\"")\lines!

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
    .Tuple = (values)->
        return Tuple(unpack(values))
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

    .error = (src,end_pos,start_pos,err_msg)->
        seen_errors = lpeg.userdata.errors
        if seen_errors[start_pos]
            return true
        err_pos = start_pos
        --if src\sub(err_pos,err_pos)\match("[\r\n]")
        --    err_pos += #src\match("[ \t\n\r]*", err_pos)
        text_loc = lpeg.userdata.source_code.source\sub(err_pos,err_pos)
        line_no = text_loc\get_line_number!
        src = FILE_CACHE[text_loc.filename]
        prev_line = line_no == 1 and "" or src\sub(LINE_STARTS[src][line_no-1] or 1, LINE_STARTS[src][line_no]-2)
        err_line = src\sub(LINE_STARTS[src][line_no], (LINE_STARTS[src][line_no+1] or 0)-2)
        next_line = src\sub(LINE_STARTS[src][line_no+1] or -1, (LINE_STARTS[src][line_no+2] or 0)-2)
        pointer = ("-")\rep(err_pos-LINE_STARTS[src][line_no]) .. "^"
        err_msg = (err_msg or "Parse error").." at #{lpeg.userdata.source_code.source.filename}:#{line_no}:\n"
        if #prev_line > 0 then err_msg ..= "\n"..prev_line
        err_msg ..= "\n#{err_line}\n#{pointer}"
        if #next_line > 0 then err_msg ..= "\n"..next_line
        --error(err_msg)
        seen_errors[start_pos] = err_msg
        return true

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
        word:(NOMSU_DEFS.ident_char^1 + NOMSU_DEFS.operator^1)
        varname:(R('az','AZ','09') + P('_') + NOMSU_DEFS.utf8_char)^0
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
            arg_orders[stub] = [fn_arg_positions[Types.Var.as_lua_id(a)] for a in *stub_args]
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
        lua = tree\as_lua(@)\as_statements!
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

    walk_tree: (tree, depth=0)=>
        coroutine.yield(tree, depth)
        return unless Types.is_node(tree)
        switch tree.type
            when "List", "Block", "Action", "Text", "IndexChain"
                for v in *tree.value
                    @walk_tree(v, depth+1)
            when "Dict"
                for e in *tree.value
                    @walk_tree(e.key, depth+1)
                    @walk_tree(e.value, depth+1)
            else @walk_tree(tree.value, depth+1)
        return nil

    tree_with_replaced_vars: (tree, replacements)=>
        return tree unless next(replacements)
        if next(replacements).type == "Var"
            replacements = {tostring(k\as_lua(@)),v for k,v in pairs(replacements)}
        tree\map (t)->
            if t.type == "Var"
                id = tostring(t\as_lua(self))
                if replacements[id] != nil
                    return replacements[id]

    initialize_core: =>
        -- Sets up some core functionality
        nomsu = self
        @define_compile_action "immediately %block", (_block)=>
            lua = _block\as_lua(nomsu)\as_statements!
            lua\declare_locals!
            nomsu\run_lua(lua)
            return Lua(@source, "if IMMEDIATE then\n    ", lua, "\nend")

        add_lua_string_bits = (lua, code)->
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

        @define_compile_action "Lua %code", (_code)=>
            lua = Lua.Value(@source, "Lua(", tostring(_code.source))
            add_lua_string_bits(lua, _code)
            lua\append ")"
            return lua

        @define_compile_action "Lua value %code", (_code)=>
            lua = Lua.Value(@source, "Lua.Value(", tostring(_code.source))
            add_lua_string_bits(lua, _code)
            lua\append ")"
            return lua

        add_lua_bits = (lua, code)->
            for bit in *code.value
                if type(bit) == "string"
                    lua\append bit
                else
                    bit_lua = bit\as_lua(nomsu)
                    unless bit_lua.is_value
                        line, src = bit.source\get_line!, bit.source\get_text!
                        error "#{line}: Cannot use #{colored.yellow src} as a string interpolation value, since it's not an expression.", 0
                    lua\append bit_lua
            return lua

        @define_compile_action "lua> %code", (_code)=>
            if _code.type != "Text"
                return Lua @source, "nomsu:run_lua(", _code\as_lua(nomsu), ");"
            return add_lua_bits(Lua(_code.source), _code)

        @define_compile_action "=lua %code", (_code)=>
            if _code.type != "Text"
                return Lua.Value @source, "nomsu:run_lua(", _code\as_lua(nomsu), ":as_statements('return '))"
            return add_lua_bits(Lua.Value(_code.source), _code)

        @define_compile_action "use %path", (_path)=>
            path = nomsu\tree_to_value(_path)
            nomsu\run_file(path)
            return Lua(@source, "nomsu:run_file(#{repr path});")

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
