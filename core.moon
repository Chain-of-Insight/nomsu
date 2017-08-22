#!/usr/bin/env moon
nomic = require 'nomic'
utils = require 'utils'
g = nomic()


g\def {"say %x", "print %x"}, (vars)=>
    print(utils.repr(vars.x))

g\def [[printf %str]], (args)=>
    for s in *args.str do io.write(utils.repr(s))
    io.write("\n")

g\defmacro "return %retval", (vars,helpers,ftype)=>
    with helpers
        switch ftype
            when "Expression"
                error("Cannot use a return statement as an expression")
            when "Statement"
                .lua "do return "..(.ded(.transform(vars.retval))).." end"
            else
                error"Unknown: #{ftype}"
    return nil

g\defmacro "let %varname = %value", (vars, helpers, ftype)=>
    with helpers
        if ftype == "Expression" then error("Cannot set a variable in an expression.")
        .lua "vars[#{.ded(.transform(vars.varname))} = #{.ded(.transform(vars.value))}"
    return nil

singleton = (aliases, value)->
    g\defmacro aliases, (vars,helpers,ftype)=>
        if ftype == "Expression"
            helpers.lua(value)
        else
            helpers.lua("ret = #{value}")

infix = (ops)->
    for op in *ops
        alias = op
        if type(op) == 'table'
            {alias,op} = op
        g\defmacro "%x #{op} %y", (vars,helpers,ftype)=>
            if ftype == "Statement"
                helpers.lua("ret = (#{helpers.var('x')} #{op} #{helpers.var('y')})")
            elseif ftype == "Expression"
                helpers.lua("(#{helpers.var('x')} #{op} #{helpers.var('y')})")
            else error("Unknown: #{ftype}")
unary = (ops)->
    for op in *ops
        g\defmacro "#{op} %x", (vars,helpers,ftype)=>
            if ftype == "Statement"
                helpers.lua("ret = #{op}(#{helpers.var('x')})")
            elseif ftype == "Expression"
                helpers.lua("#{op}(#{helpers.var('x')})")
            else error("Unknown: #{ftype}")

singleton {"true","yes"}, "true"
singleton {"false","no"}, "false"
singleton {"nil","null"}, "nil"
infix{"+","-","*","/","==",{"!=","~="},"<","<=",">",">=","^","and","or"}
unary{"-","#","not"}
g\def [[%x == %y]], (args)=> utils.equivalent(args.x, args.y)

g\def "rule %spec %body", (vars)=>
    self\def vars.spec, vars.body
    print "Defined rule: #{utils.repr(vars.spec)}"

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

g\defmacro "if %condition %if_body else %else_body", (vars,helpers,ftype)=>
    with helpers
        switch ftype
            when "Expression"
                .lua "((#{.ded(.transform(vars.condition))}) and"
                .indented ->
                    .lua "("..(.ded(.transform(vars.if_body)))..")"
                    .lua "or ("..(.ded(.transform(vars.if_body))).."))(game, vars)"
            when "Statement"
                .lua("if (#{.ded(.transform(vars.condition))}) then")
                .indented ->
                    if_body = vars.if_body
                    while if_body.type != "Block"
                        if_body = if_body.value
                        if if_body == nil then error("Failed to find body.")
                    for statement in *if_body.value
                        .lua(.ded(.transform(statement)))
                .lua("else")
                .indented ->
                    else_body = vars.else_body
                    while else_body.type != "Block"
                        else_body = else_body.value
                        if else_body == nil then error("Failed to find body.")
                    for statement in *else_body.value
                        .lua(.ded(.transform(statement)))
                .lua("end")
    return nil

g\defmacro "for %varname in %iterable %body", (vars,helpers,ftype)=>
    with helpers
        switch ftype
            when "Expression"
                .lua "(function(game, vars)"
                .indented ->
                    .lua "local comprehension, vars = {}, setmetatable({}, {__index=vars})"
                    .lua "for i, value in ipairs(#{.ded(.transform(vars.iterable))}) do"
                    .indented ->
                        .lua "local comp_value"
                        .lua "vars[#{.ded(.transform(vars.varname))}] = value"
                        body = vars.body
                        while body.type != "Block"
                            body = body.value
                        if body == nil then error("Failed to find body.")
                        for statement in *body.value
                            -- TODO: Clean up this ugly bit
                            .lua("comp_value = "..(.ded(.transform(statement.value, {type:"Expression"}))))
                        .lua "table.insert(comprehension, comp_value)"
                    .lua "end"
                    .lua "return comprehension"
                .lua "end)(game,vars)"
            when "Statement"
                .lua "do"
                .indented ->
                    .lua "local vars = setmetatable({}, {__index=vars})"
                    .lua "for i, value in ipairs(#{.ded(.transform(vars.iterable))}) do"
                    .indented ->
                        .lua "vars[#{.ded(.transform(vars.varname))}] = value"
                        body = vars.body
                        while body.type != "Block"
                            body = body.value
                        if body == nil then error("Failed to find body.")
                        for statement in *body.value
                            .lua(.ded(.transform(statement)))
                    .lua "end"
                .lua "end"
    return nil

g\simplemacro "if %condition %body", [[
if %condition %body
..else: nil
]]

g\simplemacro "unless %condition %body", [[
if (not %condition) %body
..else: nil]]

g\def [[do %action]], (vars)=> return vars.action(self,vars)


return g
