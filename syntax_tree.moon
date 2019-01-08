-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
{:insert, :remove, :concat} = table
{:Source} = require "code_obj"
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

--types = {"Number", "Var", "Block", "EscapedNomsu", "Text", "List", "Dict", "DictEntry",
--    "IndexChain", "Action", "FileChunks", "Error", "Comment"}
class SyntaxTree
    __tostring: =>
        bits = [type(b) == 'string' and b\as_lua! or tostring(b) for b in *@]
        for k,v in pairs(@)
            unless bits[k] or k == 'type' or k == 'source'
                table.insert(bits, "#{k}=#{type(v) == 'string' and v\as_lua! or v}")
        return "#{@type}{#{table.concat(bits, ", ")}}"

    __eq: (other)=>
        return false if type(@) != type(other) or #@ != #other or getmetatable(@) != getmetatable(other)
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
    get_source_code: => @@source_code_for_tree[@]\sub(@source.start, @source.stop)
    map: (fn)=>
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
                    r = v\map(fn)
                    continue if r == v or r == nil
                    changes = true
                    replacement[k] = r
            return @ unless changes
            replacement = SyntaxTree(replacement)
        return replacement

    get_args: =>
        assert(@type == "Action" or @type == "MethodCall", "Only actions and method calls have arguments")
        args = {}
        if @type == "MethodCall"
            assert(#@ == 2, "Can't get arguments for multiple method calls at once.")
            args[1] = @[1]
            for tok in *@[2]
                if type(tok) != 'string' then args[#args+1] = tok
        else
            for tok in *@
                if type(tok) != 'string' then args[#args+1] = tok
        return args

    get_stub: =>
        if @type == "MethodCall"
            assert(#@ == 2, "Can't get the stubs of multiple method calls at once.")
            return @[2]\get_stub!
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

    as_var: =>
        assert(@type == "Var")
        if type(@[1]) == 'string'
            return @[1]
        else
            return @[1]\get_stub!

    @is_instance: (t)=>
        type(t) == 'table' and getmetatable(t) == @__base

SyntaxTree.__base.__type = "Syntax Tree"

getmetatable(SyntaxTree).__call = (t)=>
    if type(t.source) == 'string'
        t.source = Source\from_string(t.source)
    setmetatable(t, @__base)
    if t.type == 'Action'
        t.stub = t\get_stub!
    return t

return SyntaxTree
