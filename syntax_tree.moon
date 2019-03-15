-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
{:insert, :remove, :concat} = table
{:Source} = require "code_obj"
{:List, :Dict} = require 'containers'
Files = require 'files'
unpack or= table.unpack

as_lua = =>
    if type(@) == 'number'
        return tostring(@)
    if mt = getmetatable(@)
        if _as_lua = mt.as_lua
            return _as_lua(@)
    return @as_lua! if @as_lua
    error("Not supported: #{@}")

local SyntaxTree
class SyntaxTree
    __tostring: =>
        bits = [type(b) == 'string' and b\as_lua! or tostring(b) for b in *@]
        for k,v in pairs(@)
            unless bits[k] or k == 'type' or k == 'source'
                table.insert(bits, "#{k}=#{type(v) == 'string' and v\as_lua! or v}")
        return "#{@type}{#{table.concat(bits, ", ")}}"

    __eq: (other)=>
        return false if type(@) != type(other) or #@ != #other or getmetatable(@) != getmetatable(other)
        return false if @type != other.type
        for i=1,#@
            return false if @[i] != other[i]
        return true

    as_lua: =>
        bits = [as_lua(b) for b in *@]
        for k,v in pairs(@)
            unless bits[k]
                table.insert(bits, "[ #{as_lua(k)}]=#{as_lua(v)}")
        return "SyntaxTree{#{table.concat(bits, ", ")}}"

    @source_code_for_tree: setmetatable({}, {
        __index:(t)=>
            s = t.source
            f = Files.read(s.filename)
            return f
        __mode: "k"
    })
    get_source_file: => @@source_code_for_tree[@]
    get_source_code: => @@source_code_for_tree[@]\sub(@source.start, @source.stop-1)

    add: (...)=>
        n = #@
        for i=1,select('#', ...)
            @[n+i] = select(i, ...)
        @stub = nil

    with: (fn)=>
        if type(fn) == 'table'
            replacements = fn
            fn = (t)->
                if t.type == "Var"
                    if r = replacements[t\as_var!]
                        return r

        replacement = fn(@)
        if replacement == false then return nil
        if replacement
            -- Clone the replacement, so we can give it a proper source/comments
            if SyntaxTree\is_instance(replacement)
                replacement = {k,v for k,v in pairs replacement}
                replacement.source = @source
                replacement.comments = {unpack(@comments)} if @comments
                replacement = SyntaxTree(replacement)
        else
            replacement = {source:@source, comments:@comments and {unpack(@comments)}}
            changes = false
            for k,v in pairs(@)
                replacement[k] = v
                if SyntaxTree\is_instance(v)
                    r = v\with(fn)
                    continue if r == v or r == nil
                    changes = true
                    replacement[k] = r
            return @ unless changes
            replacement = SyntaxTree(replacement)
        return replacement

    contains: (subtree)=>
        if subtree == @ then return true
        for k,v in pairs(@)
            if SyntaxTree\is_instance(v)
                return true if v\contains(subtree)
        return false

    get_args: =>
        assert(@type == "Action" or @type == "MethodCall", "Only actions and method calls have arguments")
        args = {}
        if @type == "MethodCall"
            args[1] = @[1]
            for i=2,#@
                for tok in *@[i]
                    if type(tok) != 'string' then args[#args+1] = tok
        else
            for tok in *@
                if type(tok) != 'string' then args[#args+1] = tok
        return args

    get_stub: =>
        switch @type
            when "Action"
                stub_bits = {}
                arg_i = 1
                for a in *@
                    if type(a) == 'string'
                        stub_bits[#stub_bits+1] = a
                    else
                        stub_bits[#stub_bits+1] = arg_i
                        arg_i += 1
                while type(stub_bits[#stub_bits]) == 'number'
                    stub_bits[#stub_bits] = nil
                return concat stub_bits, " "
            when "MethodCall"
                return "0, "..table.concat([@[i]\get_stub! for i=2,#@], "; ")
            else
                error("#{@type}s do not have stubs")

    as_var: =>
        assert(@type == "Var")
        if type(@[1]) == 'string'
            return @[1]
        else
            return @[1]\get_stub!

    matching: (patt)=>
        if patt.type == "Var"
            return {[patt\as_var!]:@}
        return nil if patt.type != @type
        return nil if patt.type == "Action" and patt\get_stub! != @get_stub!
        -- TODO: support vararg matches like (\(say 1 2 3), matching \(say *$values))
        return nil if #@ != #patt
        match = {}
        for i=1,#@
            v = @[i]
            pv = patt[i]
            return nil if type(v) != type(pv)
            if type(v) != 'table'
                return nil unless v == pv
            else
                m = v\matching(pv)
                return nil unless m
                for mk,mv in pairs(m)
                    return nil if match[mk] and match[mk] != mv
                    match[mk] = mv
        return Dict(match)

    _breadth_first: =>
        coroutine.yield @
        for child in *@
            if getmetatable(child) == SyntaxTree.__base
                child\_breadth_first!
        return
    breadth_first: => coroutine.create(-> @_breadth_first!)

    _depth_first: =>
        coroutine.yield @
        for child in *@
            if getmetatable(child) == SyntaxTree.__base
                child\_depth_first!
        return
    depth_first: => coroutine.create(-> @_depth_first!)

    @is_instance: (t)=>
        type(t) == 'table' and getmetatable(t) == @__base

SyntaxTree.__base.__type = "a Syntax Tree"

getmetatable(SyntaxTree).__call = (t, ...)=>
    if type(t.source) == 'string'
        t.source = Source\from_string(t.source)
    setmetatable(t, @__base)
    for i=1,select("#", ...)
        t[i] = select(i, ...)
    if t.type == 'Action'
        t.stub = t\get_stub!
    return t

return SyntaxTree
