-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
utils = require 'utils'
{:repr, :stringify, :min, :max, :equivalent, :set, :is_list, :sum} = utils
immutable = require 'immutable'
{:insert, :remove, :concat} = table
{:Lua, :Nomsu, :Source} = require "code_obj"

MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value

Types = {}
Types.is_node = (n)->
    type(n) == 'userdata' and getmetatable(n) and Types[n.type] == getmetatable(n)

-- Helper method:
Tree = (name, fields, methods)->
    methods or= {}
    is_multi = true
    for f in *fields do is_multi and= (f != "value")
    with methods
        .type = name
        .name = name
        .__new or= (source, ...)=>
            assert source
            if type(source) == 'string'
                source = Source\from_string(source)
            --assert Source\is_instance(source)
            return source, ...
        .is_multi = is_multi
        .map = (fn)=>
            if type(fn) == 'table'
                return @ unless next(fn)
                _replacements = fn
                fn = (k)-> _replacements[k]
            return @_map(fn)
        if is_multi
            .__tostring = => "#{@name}(#{table.concat [repr(v) for v in *@], ', '})"
            ._map = (fn)=>
                if ret = fn(@)
                    return ret
                new_vals = [v._map and v\_map(fn) or v for v in *@]
                ret = getmetatable(self)(@source, unpack(new_vals))
                return ret
        else
            .__tostring = => "#{@name}(#{repr(@value)})"
            ._map = (fn)=>
                fn(@) or @

    Types[name] = immutable fields, methods

Tree "Block", {"source"}
Tree "EscapedNomsu", {"source"}
Tree "Text", {"source"}
Tree "List", {"source"}
Tree "Dict", {"source"}
Tree "DictEntry", {"source"}
Tree "IndexChain", {"source"}
Tree "Number", {"source", "value"}
Tree "Var", {"source", "value"}

Tree "Action", {"source", "stub"},
    __new: (source, ...)=>
        assert source
        if type(source) == 'string'
            source = Source\from_string(source)
        --assert Source\is_instance(source)
        stub_bits = {}
        for i=1,select("#",...)
            a = select(i, ...)
            stub_bits[i] = type(a) == 'string' and a or "%"
        stub = concat stub_bits, " "
        return source, stub, ...
    get_spec: =>
        concat [type(a) == "string" and a or "%#{a.value}" for a in *@], " "

return Types
