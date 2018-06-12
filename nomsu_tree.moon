-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
{:repr} = require 'utils'
{:insert, :remove, :concat} = table
{:Source} = require "code_obj"

AST = {}
AST.is_syntax_tree = (n)->
    type(n) == 'table' and getmetatable(n) and AST[n.type] == getmetatable(n)

-- Helper method:
Tree = (name, leaf_or_branch, methods)->
    cls = methods or {}
    is_multi = leaf_or_branch == 'branch'
    with cls
        .type = name
        .is_instance = (x)=> getmetatable(x) == @
        .__index = cls
        .__tostring = => "#{@name}(#{@value and repr(@value) or table.concat([repr(v) for v in *@]), ', '})"
        .map = (fn)=>
            if replacement = fn(@) then return replacement
            if @value then return @
            new_vals = [v.map and v\map(fn) or v for v in *@]
            return getmetatable(self)(@source, unpack(new_vals))

    AST[name] = setmetatable cls,
        __tostring: => @name
        __call: (source, ...)=>
            if type(source) == 'string'
                source = Source\from_string(source)
            assert(Source\is_instance(source))
            inst = if is_multi then {:source, ...} else {:source, value:...}
            setmetatable(inst, @)
            if inst.__init then inst\__init!
            return inst

Tree "Number", 'leaf'
Tree "Var", 'leaf'
Tree "Block", 'branch'
Tree "EscapedNomsu", 'branch'
Tree "Text", 'branch'
Tree "List", 'branch'
Tree "Dict", 'branch'
Tree "DictEntry", 'branch'
Tree "IndexChain", 'branch'
Tree "Action", 'branch',
    __init: =>
        stub_bits = [type(a) == 'string' and a or '%' for a in *@]
        @stub = concat stub_bits, " "
    get_spec: =>
        concat [type(a) == "string" and a or "%#{a.value}" for a in *@], " "

return AST
