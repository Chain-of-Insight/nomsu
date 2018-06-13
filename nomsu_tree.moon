-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
{:repr} = require 'utils'
{:insert, :remove, :concat} = table
{:Source} = require "code_obj"

AST = {}
AST.is_syntax_tree = (n)->
    type(n) == 'table' and getmetatable(n) and AST[n.type] == getmetatable(n)

-- Helper method:
Tree = (name, methods)->
    cls = methods or {}
    with cls
        .type = name
        .__class = cls
        .__name = name
        .is_instance = (x)=> getmetatable(x) == @
        .__index = cls
        .__tostring = => "#{@name}(#{table.concat([repr(v) for v in *@]), ', '})"
        .map = (fn)=>
            if replacement = fn(@) then return replacement
            made_changes, new_vals = false, {}
            for i,v in ipairs @
                if AST.is_syntax_tree(v)
                    if replacement = v\map(fn)
                        if replacement ~= v
                            made_changes = true
                            v = replacement
                new_vals[i] = v
            return @ unless made_changes
            replacement = getmetatable(self)(@source, unpack(new_vals))
            return replacement

    AST[name] = setmetatable cls,
        __tostring: => @name
        __call: (source, ...)=>
            if type(source) == 'string'
                source = Source\from_string(source)
            assert(Source\is_instance(source))
            inst = {:source, ...}
            setmetatable(inst, @)
            if inst.__init then inst\__init!
            return inst

Tree "Number"
Tree "Var"
Tree "Block"
Tree "EscapedNomsu"
Tree "Text"
Tree "List"
Tree "Dict"
Tree "DictEntry"
Tree "IndexChain"
Tree "Action",
    __init: =>
        stub_bits = [type(a) == 'string' and a or '%' for a in *@]
        @stub = concat stub_bits, " "
    get_spec: =>
        concat [type(a) == "string" and a or "%#{a[1]}" for a in *@], " "

return AST
