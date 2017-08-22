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

g\defmacro {"true", "yes"}, (vars,helpers,ftype)=> helpers.lua("true")
g\defmacro {"false", "no"}, (vars,helpers,ftype)=> helpers.lua("false")
g\defmacro "nil", (vars,helpers,ftype)=> helpers.lua("nil")
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


return game
