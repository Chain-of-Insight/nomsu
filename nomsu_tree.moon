-- This file contains the datastructures used to represent parsed Nomsu syntax trees,
-- as well as the logic for converting them to Lua code.
utils = require 'utils'
{:repr, :stringify, :min, :max, :equivalent, :set, :is_list, :sum} = utils
immutable = require 'immutable'
{:insert, :remove, :concat} = table
{:Lua, :Nomsu, :Location} = require "code_obj"

MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value

Types = {}
Types.is_node = (n)->
    type(n) == 'userdata' and getmetatable(n) and Types[n.type] == getmetatable(n)

-- Helper method:
Tree = (name, kind, methods)->
    methods or= {}
    assert((kind == 'single') or (kind == 'multi'))
    is_multi = (kind == 'multi')
    with methods
        .type = name
        .name = name
        .__new = (value, source)=>
            assert source
            return value, source
        .is_multi = is_multi
        .map = (fn)=>
            if type(fn) == 'table'
                return @ unless next(fn)
                if type(next(fn)) == 'string'
                    error("SHIT")
                _replacements = fn
                fn = (k)-> _replacements[k]
            return @_map(fn)
        if is_multi
            .__tostring = => "#{@name}(#{table.concat [repr(v) for v in *@value], ', '})"
            ._map = (fn)=>
                if ret = fn(@)
                    return ret
                new_vals = [v._map and v\_map(fn) or v for v in *@value]
                ret = getmetatable(self)(Tuple(unpack(new_vals)), @source)
                return ret
            .__ipairs = => error!
        else
            .__tostring = => "#{@name}(#{repr(@value)})"
            ._map = (fn)=>
                fn(@) or @

    Types[name] = immutable {"value", "source"}, methods

Tree "Block", 'multi'
Tree "EscapedNomsu", 'multi'
Tree "Text", 'multi'
Tree "List", 'multi'
Tree "Dict", 'multi'
Tree "DictEntry", 'multi'
Tree "IndexChain", 'multi'
Tree "Number", 'single'
Tree "Comment", 'single'

Tree "Var", 'single',
    as_lua_id: =>
        "_"..(@value\gsub("%W", (c)-> if c == "_" then "__" else ("_%x")\format(c\byte!)))

Tree "Action", 'multi',
    get_stub: (include_names=false)=>
        if include_names
            concat [type(a) == "string" and a or "%#{a.value}" for a in *@value], " "
        else
            concat [type(a) == "string" and a or "%" for a in *@value], " "

return Types
