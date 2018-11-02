-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
{:insert, :remove, :concat} = table
{:Source} = require "code_obj"
unpack or= table.unpack

as_lua = =>
    if type(@) == 'number'
        return tostring(@)
    if mt = getmetatable(@)
        if _as_lua = mt.as_lua
            return _as_lua(@)
    error("Not supported: #{@}")

--types = {"Number", "Var", "Block", "EscapedNomsu", "Text", "List", "Dict", "DictEntry",
--    "IndexChain", "Action", "FileChunks", "Error", "Comment"}
class SyntaxTree
    @__type: "Syntax Tree"
    
    __tostring: =>
        bits = [tostring(b) for b in *@]
        for k,v in pairs(@)
            unless bits[k]
                table.insert(bits, "[ #{tostring(k)}]=#{tostring(v)}")
        return "SyntaxTree{#{table.concat(bits, ", ")}}"

    __eq: (other)=>
        return false if type(@) != type(other) or #@ != #other or getmetatable(@) != getmetatable(other)
        for i=1,#@
            return false if @[i] != other[i]
        return false if @target != other.target
        return true

    as_lua: =>
        bits = [as_lua(b) for b in *@]
        for k,v in pairs(@)
            unless bits[k]
                table.insert(bits, "[ #{as_lua(k)}]=#{as_lua(v)}")
        return "SyntaxTree{#{table.concat(bits, ", ")}}"

    @source_code_for_tree: setmetatable({}, {__index:(t)=>
        s = t.source
        Files = require 'files'
        f = Files.read(s.filename)
        return f
    })
    get_source_code: => @@source_code_for_tree[@]
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
        assert(@type == "Action", "Only actions have arguments")
        return [tok for tok in *@ when type(tok) != 'string']

    get_stub: =>
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

    @is_instance: (t)=>
        type(t) == 'table' and getmetatable(t) == @__base


getmetatable(SyntaxTree).__call = (t)=>
    if type(t.source) == 'string'
        t.source = Source\from_string(t.source)
    setmetatable(t, @__base)
    if t.type == 'Action'
        t.stub = t\get_stub!
    return t

return SyntaxTree
