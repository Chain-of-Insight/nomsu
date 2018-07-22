-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
{:repr} = require 'utils'
{:insert, :remove, :concat} = table
{:Source} = require "code_obj"
unpack or= table.unpack

AST = {}
AST.is_syntax_tree = (n, t=nil)->
    type(n) == 'table' and getmetatable(n) and AST[n.type] == getmetatable(n) and (t == nil or n.type == t)

types = {"Number", "Var", "Block", "EscapedNomsu", "Text", "List", "Dict", "DictEntry",
    "IndexChain", "Action", "FileChunks"}
for name in *types
    cls = {}
    with cls
        .__class = cls
        .__index = cls
        .__name = name
        .type = name
        .is_instance = (x)=> getmetatable(x) == @
        .__tostring = => "#{@type}(#{repr tostring(@source)}, #{concat([repr(v) for v in *@], ', ')})"
        .map = (fn)=>
            replacement = fn(@)
            if replacement == false then return nil
            if replacement
                -- Clone the replacement, but give it a proper source
                replacement = (replacement.__class)(@source, unpack(replacement))
            else
                replacements = {}
                changes = false
                for i,v in ipairs(@)
                    replacements[#replacements+1] = v
                    if AST.is_syntax_tree(v)
                        r = v\map(fn)
                        continue if r == v or r == nil
                        changes = true
                        replacements[#replacements] = r
                return @ unless changes
                replacement = (@__class)(@source, unpack(replacements))
            replacement.comments = [c for c in *@comments] if @comments
            return replacement
        .__eq = (other)=>
            return false if type(@) != type(other) or #@ != #other or getmetatable(@) != getmetatable(other)
            for i=1,#@
                return false if @[i] != other[i]
            return true

    AST[name] = setmetatable cls,
        __tostring: => @name
        __call: (source, ...)=>
            if type(source) == 'string'
                source = Source\from_string(source)
            for i=1,select('#', ...) do assert(select(i,...))
            assert(Source\is_instance(source))
            inst = {:source, ...}
            setmetatable(inst, @)
            if inst.__init then inst\__init!
            return inst

AST.Action.__init = =>
    stub_bits = [type(a) == 'string' and a or '%' for a in *@]
    @stub = concat stub_bits, " "

AST.Action.get_args = =>
    [tok for tok in *@ when type(tok) != 'string']

return AST
