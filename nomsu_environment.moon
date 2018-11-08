-- This file defines the environment in which Nomsu code runs, including some
-- basic bootstrapping functionality.
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
{:Importer, :import_to_1_from, :_1_forked} = require 'importer'
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

Parsers = {}
max_parser_version = 0
for version=1,999
    local peg_file
    if package.nomsupath
        for path in package.nomsupath\gmatch("[^;]+")
            peg_file = io.open(path.."/nomsu.#{version}.peg")
            break if peg_file
    else
        peg_file = io.open("nomsu.#{version}.peg")
    break unless peg_file
    max_parser_version = version
    peg_contents = peg_file\read('*a')
    peg_file\close!
    Parsers[version] = make_parser(peg_contents, make_tree)

{:tree_to_nomsu, :tree_to_inline_nomsu} = require "nomsu_decompiler"
nomsu_environment = Importer{
    NOMSU_COMPILER_VERSION: 12, NOMSU_SYNTAX_VERSION: max_parser_version
    -- Lua stuff:
    :next, unpack: unpack or table.unpack, :setmetatable, :coroutine, :rawequal, :getmetatable, :pcall,
    :error, :package, :os, :require, :tonumber, :tostring, :string, :xpcall, :module,
    :print, :loadfile, :rawset, :_VERSION, :collectgarbage, :rawget, :rawlen,
    :table, :assert, :dofile, :loadstring, lua_type_of:type, :select, :math, :io, :load,
    :pairs, :ipairs, :jit, :_VERSION
    bit: (jit or _VERSION == "Lua 5.2") and require('bitops') or nil
    -- Nomsu types:
    List:List, Dict:Dict,
    -- Utilities and misc.
    lpeg:lpeg, re:re, Files:Files,
    :SyntaxTree, TESTS: Dict({}), globals: Dict({}),
    :LuaCode, :NomsuCode, :Source
    SOURCE_MAP: Importer({})

    -- Nomsu functions:
    _1_as_nomsu:tree_to_nomsu, _1_as_inline_nomsu:tree_to_inline_nomsu
    compile: require('nomsu_compiler')
    :_1_forked, :import_to_1_from

    _1_parsed: (nomsu_code)->
        if type(nomsu_code) == 'string'
            filename = Files.spoof(nomsu_code)
            nomsu_code = NomsuCode(Source(filename, 1, #nomsu_code), nomsu_code)
        source = nomsu_code.source
        nomsu_code = tostring(nomsu_code)
        version = nomsu_code\match("^#![^\n]*nomsu[ ]+-V[ ]*([0-9.]+)")
        syntax_version = version and tonumber(version\match("^[0-9]+")) or max_parser_version
        parse = Parsers[syntax_version] or Parsers[max_parser_version]
        tree = parse(nomsu_code, source.filename)
        find_errors = (t)->
            if t.type == "Error"
                coroutine.yield t
            else
                for k,v in pairs(t)
                    continue unless SyntaxTree\is_instance(v)
                    find_errors(v)
        errs = [err for err in coroutine.wrap(-> find_errors(tree))]
        num_errs = #errs
        if num_errs > 0
            err_strings = [pretty_error{
                    title:"Parse error"
                    error:e.error, hint:e.hint, source:e\get_source_code!
                    start:e.source.start, stop:e.source.stop, filename:e.source.filename
                } for i, e in ipairs(errs) when i <= 3]
            if num_errs > #err_strings
                table.insert(err_strings, "\027[31;1m +#{num_errs-#err_strings} additional errors...\027[0m\n")
            error(table.concat(err_strings, '\n\n'), 0)
        
        return tree

    run_1_in: (to_run, environment)->
        if type(to_run) == 'string'
            filename = Files.spoof(to_run)
            to_run = NomsuCode(Source(filename, 1, #to_run), to_run)
            ret = environment.run_1_in(to_run, environment)
            return ret
        elseif NomsuCode\is_instance(to_run)
            tree = environment._1_parsed(to_run)
            return nil if tree == nil
            ret = environment.run_1_in(tree, environment)
            return ret
        elseif SyntaxTree\is_instance(to_run)
            filename = to_run.source.filename\gsub("\n.*", "...")
            if to_run.type != "FileChunks"
                to_run = {to_run}
            -- Each chunk's compilation is affected by the code in the previous chunks
            -- (typically), so each chunk needs to compile and run before the next one
            -- compiles.
            ret = nil
            for chunk in *to_run
                lua = environment.compile(chunk)
                lua\declare_locals!
                lua\prepend "-- File: #{filename}\n"
                ret = environment.run_1_in(lua, environment)
            return ret
        elseif LuaCode\is_instance(to_run)
            source = to_run.source
            lua_string = to_run\text!
            -- If you replace tostring(source) with "nil", source mapping won't happen
            run_lua_fn, err = load(lua_string, tostring(source), "t", environment)
            if not run_lua_fn
                lines =[("%3d|%s")\format(i,line) for i, line in ipairs Files.get_lines(lua_string)]
                line_numbered_lua = table.concat(lines, "\n")
                error("Failed to compile generated code:\n\027[1;34m#{line_numbered_lua}\027[0m\n\n#{err}", 0)
            source_key = tostring(source)
            unless environment.SOURCE_MAP[source_key]
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
                environment.SOURCE_MAP[source_key] = map
            return run_lua_fn!
        else
            error("Attempt to run unknown thing: "..tostring(to_run))
    
    FILE_CACHE: {}
    run_file_1_in: (path, environment, optimization=1)->
        if environment.FILE_CACHE[path]
            import_to_1_from(environment, environment.FILE_CACHE[path])
            return
        mod = _1_forked(environment)
        assert mod._1_parsed
        mod._ENV = mod
        for _,filename in Files.walk(path)
            continue unless filename == "stdin" or filename\match("%.nom$")
            lua_filename = filename\gsub("%.nom$", ".lua")
            -- TODO: don't automatically use precompiled version?
            code = if optimization != 0 and Files.read(lua_filename)
                file = Files.read(lua_filename)
                LuaCode(Source(filename, 1, #file), file)
            else
                file = Files.read(filename)
                NomsuCode(Source(filename, 1, #file), file)
            environment.run_1_in(code, mod)
        import_to_1_from(environment, mod)
        environment.FILE_CACHE[path] = mod

    compile_error_at: (tree, err_msg, hint=nil)->
        err_str = pretty_error{
            title: "Compile error"
            error:err_msg, hint:hint, source:tree\get_source_code!
            start:tree.source.start, stop:tree.source.stop, filename:tree.source.filename
        }
        error(err_str, 0)
}
nomsu_environment._ENV = nomsu_environment

-- Hacky use of globals:
export SOURCE_MAP
SOURCE_MAP = nomsu_environment.SOURCE_MAP

return nomsu_environment