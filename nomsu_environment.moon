-- This file defines the environment in which Nomsu code runs, including some
-- basic bootstrapping functionality.
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
{:List, :Dict} = require 'containers'
Text = require 'text'
SyntaxTree = require "syntax_tree"
Files = require "files"
Errhand = require "error_handling"
C = require "colors"
make_parser = require("parser")
pretty_error = require("pretty_errors")

make_tree = (tree, userdata)->
    tree.source = Source(userdata.filename, tree.start, tree.stop)
    tree.start, tree.stop = nil, nil
    tree = SyntaxTree(tree)
    return tree

Parsers = {}
for version=1,NOMSU_VERSION[1]
    local peg_file
    if package.nomsupath
        pegpath = package.nomsupath\gsub("lib/%?%.nom", "?.peg")\gsub("lib/%?%.lua", "?.peg")
        if path = package.searchpath("nomsu.#{version}", pegpath, "/")
            peg_file = io.open(path)
    else
        peg_file = io.open("nomsu.#{version}.peg")
    assert peg_file, "No PEG file found for Nomsu version #{version}"
    peg_contents = peg_file\read('*a')
    peg_file\close!
    Parsers[version] = make_parser(peg_contents, make_tree)

{:tree_to_nomsu, :tree_to_inline_nomsu} = require "nomsu_decompiler"
{:compile, :fail_at} = require('nomsu_compiler')
_module_imports = {}
_importer_mt = {__index: (k)=> _module_imports[@][k]}
Importer = (t, imports)->
    _module_imports[t] = imports or {}
    t._IMPORTS = _module_imports[t]
    return setmetatable(t, _importer_mt)

_1_as_text = (x)->
    if x == true then return "yes"
    if x == false then return "no"
    return tostring(x)

_1_as_list = (x)->
    mt = getmetatable(x)
    if mt.as_list then return mt.as_list(x)
    return x

local nomsu_environment
nomsu_environment = Importer{
    -- Lua stuff:
    :next, unpack: unpack or table.unpack, :setmetatable, :rawequal, :getmetatable, :pcall,
    yield:coroutine.yield, resume:coroutine.resume, coroutine_status_of:coroutine.status,
    coroutine_wrap:coroutine.wrap, coroutine_from: coroutine.create,
    :error, :package, :os, :require, :tonumber, :tostring, :string, :xpcall,
    :print, :loadfile, :rawset, :_VERSION, :collectgarbage, :rawget, :rawlen,
    :table, :assert, :dofile, :loadstring, lua_type_of:type, :select, :math, :io, :load,
    :pairs, :ipairs, :jit, :_VERSION, LUA_VERSION: (jit and jit.version or _VERSION),
    LUA_API: _VERSION, Bit: (jit or _VERSION == "Lua 5.2") and require('bitops') or nil
    -- Nomsu types:
    a_List:List, a_Dict:Dict, Text:Text,
    -- Utilities and misc.
    lpeg:lpeg, re:re, Files:Files,
    :SyntaxTree, TESTS: Dict({}), globals: Dict({}),
    :LuaCode, :NomsuCode, :Source
    LuaCode_from: ((src, ...)-> LuaCode\from(src, ...)),
    NomsuCode_from: ((src, ...)-> NomsuCode\from(src, ...)),
    enhance_error: Errhand.enhance_error
    SOURCE_MAP: {},
    getfenv:getfenv,

    -- Nomsu functions:
    _1_as_nomsu:tree_to_nomsu, _1_as_inline_nomsu:tree_to_inline_nomsu,
    compile: compile, at_1_fail:fail_at, :_1_as_text, :_1_as_list,
    exit:os.exit, quit:os.exit,

    _1_parsed: (nomsu_code, syntax_version)->
        if type(nomsu_code) == 'string'
            filename = Files.spoof(nomsu_code)
            nomsu_code = NomsuCode\from(Source(filename, 1, #nomsu_code), nomsu_code)
        source = nomsu_code.source
        nomsu_code = tostring(nomsu_code)
        version = nomsu_code\match("^#![^\n]*nomsu[ ]+-V[ ]*([0-9.]+)")
        if syntax_version
            syntax_version = tonumber(syntax_version\match("^[0-9]+"))
        syntax_version or= version and tonumber(version\match("^[0-9]+")) or NOMSU_VERSION[1]
        parse = Parsers[syntax_version]
        assert parse, "No parser found for Nomsu syntax version #{syntax_version}"
        tree = parse(nomsu_code, source.filename)
        if tree.shebang
            tree.version or= tree.shebang\match("nomsu %-V[ ]*([%d.]*)")
        return tree

    Module: (package_name)=>
        -- This *must* be the first local
        local path
        if package_name\match("%.nom$") or package_name\match("%.lua")
            path = package_name
        else
            path, err = package.searchpath(package_name, package.nomsupath, "/")
            if not path then error(err)
        path = path\gsub("^%./", "")

        if ret = package.nomsuloaded[package_name] or package.nomsuloaded[path]
            return ret

        -- Traverse up the callstack to look for import loops
        -- This is more reliable than keeping a list of running files, since
        -- that gets messed up when errors occur.
        currently_running = {}
        for i=2,999
            info = debug.getinfo(i, 'f')
            break unless info.func
            if info.func == @Module
                n, upper_path = debug.getlocal(i, 3) -- 3 means "path"
                table.insert(currently_running, upper_path)
                assert(n == "path")
                if upper_path == path
                    --circle = "\n           "..table.concat(currently_running, "\n..imports  ")
                    circle = table.concat(currently_running, "', which imports '")
                    err_i = 2
                    info = debug.getinfo(err_i)
                    while info and (info.func == @Module or info.func == @use or info.func == @export)
                        err_i += 1
                        info = debug.getinfo(err_i)
                    fail_at (info or debug.getinfo(2)), "Circular import: File '#{path}' imports '"..circle.."'"
        mod = @new_environment!
        mod.MODULE_NAME = package_name
        code = Files.read(path)
        if path\match("%.lua$")
            code = LuaCode\from(Source(path, 1, #code), code)
        else
            code = NomsuCode\from(Source(path, 1, #code), code)
        ret = mod\run(code)
        if ret != nil
            mod = ret
        package.nomsuloaded[package_name] = mod
        package.nomsuloaded[path] = mod
        return mod

    use: (package_name)=>
        mod = @Module(package_name)
        imports = assert _module_imports[@]
        for k,v in pairs(mod)
            imports[k] = v
        cr_imports = assert _module_imports[@COMPILE_RULES]
        if mod.COMPILE_RULES
            for k,v in pairs(mod.COMPILE_RULES)
                cr_imports[k] = v
        return mod

    export: (package_name)=>
        mod = @Module(package_name)
        imports = assert _module_imports[@]
        for k,v in pairs(_module_imports[mod])
            if rawget(imports, k) == nil
                imports[k] = v
        for k,v in pairs(mod)
            if rawget(@, k) == nil
                @[k] = v
        cr_imports = assert _module_imports[@COMPILE_RULES]
        if mod.COMPILE_RULES
            for k,v in pairs(_module_imports[mod.COMPILE_RULES])
                if rawget(cr_imports, k) == nil
                    cr_imports[k] = v
            for k,v in pairs(mod.COMPILE_RULES)
                if rawget(@COMPILE_RULES, k) == nil
                    @COMPILE_RULES[k] = v
        return mod

    run: (to_run)=>
        if not to_run
            error("Need both something to run and an environment")
        if type(to_run) == 'string'
            filename = Files.spoof(to_run)
            to_run = NomsuCode\from(Source(filename, 1, #to_run), to_run)
            return @run(to_run)
        elseif NomsuCode\is_instance(to_run)
            tree = @._1_parsed(to_run)
            return nil if tree == nil
            return @run(tree)
        elseif SyntaxTree\is_instance(to_run)
            filename = to_run.source.filename\gsub("\n.*", "...")
            if to_run.type != "FileChunks"
                to_run = {to_run}
            -- Each chunk's compilation is affected by the code in the previous chunks
            -- (typically), so each chunk needs to compile and run before the next one
            -- compiles.
            ret = nil
            for chunk_no, chunk in ipairs to_run
                lua = @compile(chunk)
                lua\declare_locals!
                lua\prepend "-- File: #{filename} chunk ##{chunk_no}\n"
                ret = @run(lua)
            return ret
        elseif LuaCode\is_instance(to_run)
            source = to_run.source
            lua_string = to_run\text!
            -- For some reason, Lua doesn't strip shebangs from Lua files
            lua_string = lua_string\gsub("^#![^\n]*\n","")
            -- If you replace tostring(source) with "nil", source mapping won't happen
            run_lua_fn, err = load(lua_string, tostring(source), "t", @)
            if not run_lua_fn
                lines =[("%3d|%s")\format(i,line) for i, line in ipairs lua_string\lines!]
                line_numbered_lua = table.concat(lines, "\n")
                error("Failed to compile generated code:\n#{C("bright blue", line_numbered_lua)}\n\n#{err}", 0)
            source_key = tostring(source)
            unless @SOURCE_MAP[source_key] or @OPTIMIZATION >= 2
                map = {}
                file = Files.read(source.filename)
                if not file
                    error "Failed to find file: #{source.filename}"
                nomsu_str = file\sub(source.start, source.stop)
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
                map_sources(to_run)
                map[lua_line] or= nomsu_line
                map[0] = 0
                -- Mapping from lua line number to nomsu line numbers
                @SOURCE_MAP[source_key] = map
            return run_lua_fn!
        else
            error("Attempt to run unknown thing: ".._1_as_lua(to_run))

    new_environment: ->
        env = Importer({}, {k,v for k,v in pairs(nomsu_environment)})
        env._ENV = env
        env._G = env
        env.TESTS = Dict{}
        env.COMPILE_RULES = Importer({}, {k,v for k,v in pairs(nomsu_environment.COMPILE_RULES)})
        return env
}

nomsu_environment._ENV = nomsu_environment
nomsu_environment._G = nomsu_environment
nomsu_environment.COMPILE_RULES = Importer(require('bootstrap'))
nomsu_environment.MODULE_NAME = "nomsu"

-- Hacky use of globals:
export SOURCE_MAP
SOURCE_MAP = nomsu_environment.SOURCE_MAP

return nomsu_environment
