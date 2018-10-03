-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
{:insert, :remove, :concat} = table
{:Source} = require "code_obj"
unpack or= table.unpack

AST = {}
AST.is_syntax_tree = (n, t=nil)->
    type(n) == 'table' and getmetatable(n) and getmetatable(n).__type == "Syntax Tree" and (t == nil or n.type == t)

as_lua = =>
    if type(@) == 'number'
        return tostring(@)
    if mt = getmetatable(@)
        if _as_lua = mt.as_lua
            return _as_lua(@)
    error("Not supported: #{@}")

types = {"Number", "Var", "Block", "EscapedNomsu", "Text", "List", "Dict", "DictEntry",
    "IndexChain", "Action", "FileChunks", "Error", "Comment"}
for name in *types
    cls = {}
    with cls
        .__class = cls
        .__index = cls
        .__name = name
        .type = name
        .__type = "Syntax Tree"
        .is_instance = (x)=> getmetatable(x) == @
        .__tostring = =>
            bits = [tostring(b) for b in *@]
            for k,v in pairs(@)
                unless bits[k]
                    table.insert(bits, "[ #{tostring(k)}]=#{tostring(v)}")
            return "#{@type}{#{table.concat(bits, ", ")}}"
        .as_lua = =>
            bits = [as_lua(b) for b in *@]
            for k,v in pairs(@)
                unless bits[k]
                    table.insert(bits, "[ #{as_lua(k)}]=#{as_lua(v)}")
            return "#{@type}{#{table.concat(bits, ", ")}}"
        .source_code_for_tree = setmetatable({}, {__index:(t)=>
            s = t.source
            Files = require 'files'
            f = Files.read(s.filename)
            return f
        })
        .get_source_code = => @source_code_for_tree[@]
        .map = (fn)=>
            replacement = fn(@)
            if replacement == false then return nil
            if replacement
                -- Clone the replacement, so we can give it a proper source/comments
                if AST.is_syntax_tree(replacement)
                    replacement = setmetatable {k,v for k,v in pairs replacement}, getmetatable(replacement)
                    replacement.source = @source
                    replacement.comments = {unpack(@comments)} if @comments
                    if init = replacement.__init then init(replacement)
            else
                replacement = {source:@source, comments:@comments and {unpack(@comments)}}
                changes = false
                for k,v in pairs(@)
                    replacement[k] = v
                    if AST.is_syntax_tree(v)
                        r = v\map(fn)
                        continue if r == v or r == nil
                        changes = true
                        replacement[k] = r
                return @ unless changes
                replacement = setmetatable replacement, getmetatable(@)
                if init = replacement.__init then init(replacement)
            return replacement
        .__eq = (other)=>
            return false if type(@) != type(other) or #@ != #other or getmetatable(@) != getmetatable(other)
            for i=1,#@
                return false if @[i] != other[i]
            return false if @target != other.target
            return true

    AST[name] = setmetatable cls,
        __tostring: => @__name
        __call: (t)=>
            if type(t.source) == 'string'
                t.source = Source\from_string(t.source)
            --else
            --    assert(Source\is_instance(t.source))
            setmetatable(t, @)
            if init = t.__init then init(t)
            return t

AST.Action.__init = =>
    stub_bits = {}
    arg_i = 1
    for a in *@
        if type(a) == 'string'
            stub_bits[#stub_bits+1] = a
        else
            stub_bits[#stub_bits+1] = tostring(arg_i)
            arg_i += 1
    @stub = concat stub_bits, " "

AST.Action.get_args = =>
    [tok for tok in *@ when type(tok) != 'string']

return AST
