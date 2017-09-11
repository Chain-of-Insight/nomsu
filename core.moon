#!/usr/bin/env moon
Nomic = require 'nomic'
utils = require 'utils'


class PermissionNomic extends Nomic
    new: (...)=>
        super(...)
        @callstack = {}

    call: (fn_name,...)=>
        fn_info = @defs[fn_name]
        if fn_info == nil
            error "Attempt to call undefined function: #{fn_name}"
        unless self\check_permission(fn_name)
            error "You do not have the authority to call: #{fn_name}"
        table.insert @callstack, fn_name
        {:fn, :arg_names} = fn_info
        args = {name, select(i,...) for i,name in ipairs(arg_names)}
        if @debug
            print "Calling #{fn_name} with args: #{utils.repr(args)}"
        ret = fn(self, args)
        table.remove @callstack
        return ret

    check_permission: (fn_name)=>
        fn_info = @defs[fn_name]
        if fn_info == nil
            error "Undefined function: #{fn_name}"
        if fn_info.whitelist == nil then return true
        for caller in *@callstack
            if fn_info.whitelist[caller]
                return true
        return false


g = PermissionNomic()

g\defmacro [[lua %lua_code]], (vars, kind)=>
    lua_code = vars.lua_code.value
    as_lua_code = (str)->
        switch str.type
            when "String"
                escapes = n:"\n", t:"\t", b:"\b", a:"\a", v:"\v", f:"\f", r:"\r"
                unescaped = str.value\gsub("\\(.)", ((c)-> escapes[c] or c))
                return unescaped

            when "Longstring"
                -- TODO: handle comments?
                result = [line for line in str.value\gmatch("[ \t]*|([^\n]*)")]
                return table.concat(result, "\n")
            else
                return @tree_to_lua(str)

    switch lua_code.type
        when "List"
            -- TODO: handle subexpressions
            return table.concat([as_lua_code(i.value) for i in *lua_code.value]), true
        else
            return as_lua_code(lua_code), true

g\def {"restrict %fn to %whitelist"}, (vars)=>
    fns = if type(vars.fn) == 'string' then {vars.fn} else vars.fn
    whitelist = if type(vars.whitelist) == 'string' then {vars.whitelist} else vars.whitelist
    whitelist = {w,true for w in *whitelist}
    for fn in *fns
        fn_info = @defs[fn]
        if fn_info == nil
            print "Undefined function: #{fn}"
            continue
        unless self\check_permission(fn)
            print "You do not have permission to restrict function: #{fn}"
            continue
        @defs[fn] = whitelist

g\def {"allow %whitelist to %fn"}, (vars)=>
    fns = if type(vars.fn) == 'string' then {vars.fn} else vars.fn
    whitelist = if type(vars.whitelist) == 'string' then {vars.whitelist} else vars.whitelist
    for fn in *fns
        fn_info = @defs[fn]
        if fn_info == nil
            print "Undefined function: #{fn}"
            continue
        if fn_info.whitelist == nil
            print "Function is already allowed by everyone: #{fn}"
            continue
        unless self\check_permission(fn)
            print "You do not have permission to grant permissions for function: #{fn}"
            continue
        for w in *whitelist do fn_info.whitelist[w] = true

g\def {"forbid %blacklist to %fn"}, (vars)=>
    fns = if type(vars.fn) == 'string' then {vars.fn} else vars.fn
    blacklist = if type(vars.blacklist) == 'string' then {vars.blacklist} else vars.blacklist
    for fn in *fns
        fn_info = @defs[fn]
        if fn_info == nil
            print "Undefined function: #{fn}"
            continue
        if fn_info.whitelist == nil
            print "Cannot remove items from a whitelist when there is no whitelist on function: #{fn}"
            continue
        unless self\check_permission(fn)
            print "You do not have permission to restrict function: #{fn}"
            continue
        for b in *blacklist do fn_info.whitelist[b] = nil


g\def {"say %x", "print %x"}, (vars)=>
    print(utils.repr(vars.x))

g\def [[printf %str]], (args)=>
    for s in *args.str do io.write(utils.repr(s))
    io.write("\n")

g\def [[concat %strs]], (vars)=>
    return table.concat([utils.repr(s) for s in *vars.strs], "")

g\def [[quote %str]], (vars)=>
    return utils.repr(vars.str, true)

g\defmacro "return %retval", (vars, kind)=>
    if kind == "Expression"
        error("Cannot use a return statement as an expression")
    return "do return "..((@tree_to_lua(vars.retval))\match("%s*(.*)")).." end", true

g\defmacro "let %varname = %value", (vars, kind)=>
    if kind == "Expression"
        error("Cannot set a variable in an expression.")
    return "vars[#{@tree_to_lua(vars.varname)}] = #{@tree_to_lua(vars.value)}"

singleton = (aliases, value)->
    g\defmacro aliases, ((vars)=> value)

infix = (ops)->
    for op in *ops
        alias = op
        if type(op) == 'table'
            {alias,op} = op
        g\defmacro "%x #{alias} %y", (vars)=>
            return "(#{@tree_to_lua(vars.x)} #{op} #{@tree_to_lua(vars.y)})"

unary = (ops)->
    for op in *ops
        g\defmacro "#{op} %x", (vars)=>
            return "#{op}(#{@tree_to_lua(vars.x)})"

singleton {"true","yes"}, "true"
singleton {"false","no"}, "false"
singleton {"nil","null","nop","pass"}, "nil"
infix{"+","-","*","/","==",{"!=","~="},"<","<=",">",">=","^","and","or",{"mod","%"}}
unary{"-","#","not"}
g\def [[%x == %y]], (args)=> utils.equivalent(args.x, args.y)

g\def "rule %spec %body", (vars)=>
    self\def vars.spec, vars.body

-- TODO: write help


g\def [[random]], -> math.random()

g\def [[sum %items]], (args)=> utils.sum(args.items)
g\def [[all %items]], (args)=> utils.all(args.items)
g\def [[any %items]], (args)=> utils.any(args.items)
g\def {[[average %items]], [[avg %items]]}, (args)=> utils.sum(items)/#items
g\def {[[min %items]], [[smallest %items]], [[lowest %items]], [[fewest %items]]}, (args)=>
    utils.min(args.items)

g\def {[[max %items]], [[largest %items]], [[highest %items]], [[most %items]]}, (args)=>
    utils.max(args.items)

g\def {[[argmin %items]]}, (args)=>
    utils.min(args.items, ((i)->i[2]))
g\def {[[argmax %items]]}, (args)=>
    utils.max(args.items, ((i)->i[2]))

g\def {[[min %items with respect to %keys]]}, (args)=>
    utils.min(args.items, args.keys)
g\def {[[max %items with respect to %keys]]}, (args)=>
    utils.max(args.items, args.keys)

g\def {[[%index st in %list]], [[%index nd in %list]], [[%index rd in %list]], [[%index th in %list]]}, (args)=>
    with args
        if type(.list) != 'table'
            print "Not a list: #{.list}"
            return
        .list[.index]

g\def {[[index of %item in %list]]}, (args)=>
    with args
        if type(.list) != 'table'
            print "Not a list: #{.list}"
            return
        utils.key_for(args.list, args.item)

g\run [=[
rule ["%item is in %list", "%list contains %item"]: (index of %item in %list) != (nil)
]=]

g\def {[[# %list]], [[length of %list]], [[size of %list]]}, (args)=>
    with args
        if type(.list) != 'table'
            print "Not a list: #{.list}"
            return
        return #(.list)

g\defmacro "if %condition %if_body else %else_body", (vars, kind)=>
    if kind == "Expression"
        return ([[(function(game, vars)
            if (%s) then
                %s
            else
                %s
            end
        end)(game, vars)]])\format(@tree_to_lua(vars.condition),
            @tree_to_lua(vars.if_body.value.value),
            @tree_to_lua(vars.else_body.value.value))
    else
        return ([[
            if (%s) then
                %s
            else
                %s
            end
        ]])\format(@tree_to_lua(vars.condition),
            @tree_to_lua(vars.if_body.value.value),
            @tree_to_lua(vars.else_body.value.value)), true

g\defmacro "for %varname in %iterable %body", (vars, kind)=>
    if kind == "Expression"
        return "
(function(game, vars)
    local comprehension, vars = {}, setmetatable({}, {__index=vars})
    for i, value in ipairs(#{@tree_to_lua(vars.iterable)}) do
        local ret
        vars[#{@tree_to_lua(vars.varname)}] = value
        #{@tree_to_lua(vars.body.value)}
        table.insert(comprehension, ret)
    end
    return comprehension
end)(game, vars)"
    else
        return "
do
    local comprehension, vars = {}, setmetatable({}, {__index=vars})
    for i, value in ipairs(#{@tree_to_lua(vars.iterable)}) do
        vars[#{@tree_to_lua(vars.varname)}] = value
        #{@tree_to_lua(vars.body.value)}
    end
end", true

g\simplemacro "for %varname = %start to %stop %body", [[for %varname in (lua ["utils.range(",%start,",",%stop,")"]) %body]]

g\simplemacro "if %condition %body", [[
if %condition %body
..else: nil
]]

g\simplemacro "unless %condition %body", [[
if (not %condition) %body
..else: nil
]]

g\def [[do %action]], (vars)=> return vars.action(self,vars)


g\defmacro [[macro %spec %body]], (vars, kind)=>
    if kind == "Expression" then error("Cannot use a macro definition in an expression.")
    self\simplemacro vars.spec.value.value, vars.body.src
    return "", true

g\defmacro [[test %code yields %tree]], (vars, kind)=>
    if kind == "Expression" then error("Tests must be statements.")
    got = self\stringify_tree(vars.code.value)
    got = got\match("Thunk:\n  (.*)")\gsub("\n  ","\n")
    got = utils.repr(got,true)
    expected = @tree_to_lua(vars.tree)
    return "
do
    local got = #{got}
    local expected = #{expected}
    if got ~= expected then
        error('Test failed. Expected:\n'..expected..'\n\nButGot:\n'..got)
    end
end", true

return g
