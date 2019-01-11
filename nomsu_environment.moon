-- This file defines the environment in which Nomsu code runs, including some
-- basic bootstrapping functionality.
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
{:List, :Dict, :Text} = require 'containers'
SyntaxTree = require "syntax_tree"
Files = require "files"
make_parser = require("parser")
pretty_error = require("pretty_errors")

make_tree = (tree, userdata)->
    tree.source = Source(userdata.filename, tree.start, tree.stop)
    tree.start, tree.stop = nil, nil
    tree = SyntaxTree(tree)
    return tree

table.map = (t, fn)-> setmetatable([fn(v) for _,v in ipairs(t)], getmetatable(t))

Parsers = {}
max_parser_version = 0
for version=1,999
    local peg_file
    if package.nomsupath
        pegpath = package.nomsupath\gsub("%.nom", ".peg")
        if path = package.searchpath("nomsu.#{version}", pegpath, "/")
            peg_file = io.open(path)
    else
        peg_file = io.open("nomsu.#{version}.peg")
    break unless peg_file
    max_parser_version = version
    peg_contents = peg_file\read('*a')
    peg_file\close!
    Parsers[version] = make_parser(peg_contents, make_tree)

{:tree_to_nomsu, :tree_to_inline_nomsu} = require "nomsu_decompiler"
{:compile, :compile_error} = require('nomsu_compiler')
_currently_running_files = List{} -- Used to check for circular imports in run_file_1_in
local nomsu_environment
_module_imports = {}
nomsu_environment = setmetatable({
    NOMSU_COMPILER_VERSION: 13, NOMSU_SYNTAX_VERSION: max_parser_version
    -- Lua stuff:
    :next, unpack: unpack or table.unpack, :setmetatable, :rawequal, :getmetatable, :pcall,
    yield:coroutine.yield, resume:coroutine.resume, coroutine_status_of:coroutine.status,
    coroutine_wrap:coroutine.wrap, coroutine_from: coroutine.create,
    :error, :package, :os, :require, :tonumber, :tostring, :string, :xpcall, :module,
    say:print, :loadfile, :rawset, :_VERSION, :collectgarbage, :rawget, :rawlen,
    :table, :assert, :dofile, :loadstring, lua_type_of:type, :select, :math, :io, :load,
    :pairs, :ipairs, :jit, :_VERSION
    bit: (jit or _VERSION == "Lua 5.2") and require('bitops') or nil
    -- Nomsu types:
    List:List, Dict:Dict,
    -- Utilities and misc.
    lpeg:lpeg, re:re, Files:Files,
    :SyntaxTree, TESTS: Dict({}), globals: Dict({}),
    :LuaCode, :NomsuCode, :Source
    LuaCode_from: ((src, ...)-> LuaCode\from(src, ...)),
    NomsuCode_from: ((src, ...)-> NomsuCode\from(src, ...)),
    SOURCE_MAP: {},
    getfenv:getfenv,

    -- Nomsu functions:
    _1_as_nomsu:tree_to_nomsu, _1_as_inline_nomsu:tree_to_inline_nomsu,
    compile: compile, compile_error_at:compile_error,
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
        syntax_version or= version and tonumber(version\match("^[0-9]+")) or max_parser_version
        parse = Parsers[syntax_version] or Parsers[max_parser_version]
        tree = parse(nomsu_code, source.filename)
        if tree.shebang
            tree.version or= tree.shebang\match("nomsu %-V[ ]*([%d.]*)")
        errs = {}
        find_errors = (t)->
            if t.type == "Error"
                errs[#errs+1] = t
            else
                for k,v in pairs(t)
                    continue unless SyntaxTree\is_instance(v)
                    find_errors(v)
        find_errors(tree)
        num_errs = #errs
        if num_errs > 0
            err_strings = [pretty_error{
                    title:"Parse error"
                    error:e.error, hint:e.hint, source:e\get_source_file!
                    start:e.source.start, stop:e.source.stop, filename:e.source.filename
                } for i, e in ipairs(errs) when i <= 3]
            if num_errs > #err_strings
                table.insert(err_strings, "\027[31;1m +#{num_errs-#err_strings} additional errors...\027[0m\n")
            error(table.concat(err_strings, '\n\n'), 0)
        
        return tree

    load_module: (package_name)=>
        local path
        if package_name\match("%.nom$") or package_name\match("%.lua")
            path = package_name
        else
            path, err = package.searchpath(package_name, package.nomsupath, "/")
            if not path then error(err)
        path = path\gsub("^%./", "")

        if ret = package.loaded[package_name] or package.loaded[path]
            return ret

        if _currently_running_files\has(path)
            i = _currently_running_files\index_of(path)
            _currently_running_files\add path
            circle = _currently_running_files\from_1_to(i, -1)
            error("Circular import detected:\n           "..circle\joined_with("\n..imports  "))
        mod = @new_environment!
        mod.MODULE_NAME = package_name
        mod.TESTS = Dict{}
        code = Files.read(path)
        if path\match("%.lua$")
            code = LuaCode\from(Source(path, 1, #code), code)
        else
            code = NomsuCode\from(Source(path, 1, #code), code)
        _currently_running_files\add path
        assert(mod[jit and "_G" or "_ENV"] == mod)
        mod\run(code)
        assert(mod[jit and "_G" or "_ENV"] == mod)
        _currently_running_files\pop!
        package.loaded[package_name] = mod
        package.loaded[path] = mod
        return mod

    use: (package_name)=>
        mod = @load_module(package_name)
        assert(mod[jit and "_G" or "_ENV"] == mod)
        imports = assert _module_imports[@]
        for k,v in pairs(mod)
            imports[k] = v
        assert(mod[jit and "_G" or "_ENV"] == mod)
        return mod

    export: (package_name)=>
        mod = @load_module(package_name)
        assert(mod[jit and "_G" or "_ENV"] == mod)
        imports = assert _module_imports[@]
        for k,v in pairs(_module_imports[mod])
            imports[k] = v
        for k,v in pairs(mod)
            if k != "_G" and k != "_ENV"
                @[k] = v
        assert(mod[jit and "_G" or "_ENV"] == mod)
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
            -- If you replace tostring(source) with "nil", source mapping won't happen
            run_lua_fn, err = load(lua_string, tostring(source), "t", @)
            if not run_lua_fn
                lines =[("%3d|%s")\format(i,line) for i, line in ipairs Files.get_lines(lua_string)]
                line_numbered_lua = table.concat(lines, "\n")
                error("Failed to compile generated code:\n\027[1;34m#{line_numbered_lua}\027[0m\n\n#{err}", 0)
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
            error("Attempt to run unknown thing: "..tostring(to_run))

    new_environment: ->
        env = {}
        _module_imports[env] = {k,v for k,v in pairs(nomsu_environment)}
        env[jit and "_G" or "_ENV"] = env
        setmetatable(env, getmetatable(nomsu_environment))
        return env
}, {
    __index: (k)=> _module_imports[@][k]
})
nomsu_environment[jit and "_G" or "_ENV"] = nomsu_environment
nomsu_environment.COMPILE_RULES = require('bootstrap')
_module_imports[nomsu_environment] = {}

-- Hacky use of globals:
export SOURCE_MAP
SOURCE_MAP = nomsu_environment.SOURCE_MAP

return nomsu_environment
