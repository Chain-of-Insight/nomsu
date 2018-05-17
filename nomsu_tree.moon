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
        .with_value = (value)=> getmetatable(self)(value)
        .type = name
        .name = name
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
            .__tostring = => "#{@name}(#{table.concat [repr(v) for v in *@], ', '})"
            .map = (fn)=>
                if ret = fn(@)
                    return ret
                new_vals = [v.map and v\map(fn) or v for v in *@]
                ret = getmetatable(self)(unpack(new_vals))
                return ret
        else
            .__tostring = => "#{@name}(#{repr(@value)})"
            .map = (fn)=>
                fn(@) or @

    if is_multi
        Types[name] = immutable nil, methods
    else
        Types[name] = immutable {"value"}, methods

Tree "Block", 'multi'
Tree "Text", 'multi'
Tree "List", 'multi'
Tree "Dict", 'multi'
Tree "DictEntry", 'multi'
Tree "IndexChain", 'multi'
Tree "Number", 'single'
Tree "Word", 'single'
Tree "Comment", 'single'

Tree "EscapedNomsu", 'single',
    map: (fn)=> fn(@) or @\map(fn)

Tree "Var", 'single',
    as_lua_id: =>
        "_"..(@value\gsub("%W", (c)-> if c == "_" then "__" else ("_%x")\format(c\byte!)))

Tree "Action", 'multi',
    get_stub: (include_names=false)=>
        bits = if include_names
            [(t.type == "Word" and t.value or "%#{t.value}") for t in *@]
        else [(t.type == "Word" and t.value or "%") for t in *@]
        return concat(bits, " ")

return Types
