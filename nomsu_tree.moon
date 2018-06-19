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
            if replacement = fn(@) then return replacement
            replacements = [AST.is_syntax_tree(v) and v\map(fn) or nil for v in *@]
            return @ unless next(replacements)
            return (@__class)(@source, unpack([replacements[i] or v for i,v in ipairs(@)]))

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
