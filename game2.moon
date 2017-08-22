#!/usr/bin/env moon
utils = require 'utils'
Game = require 'nomic_whitespace'
g = Game()

print("===========================================================================================")

g\def "rule %spec %body", (vars)=>
    self\def vars.spec, vars.body
    print "Defined rule: #{vars.spec}"

g\def "say %x", (vars)=>
    print(utils.repr(vars.x))

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

g\defmacro "true", (vars,helpers,ftype)=> helpers.lua("true")
g\defmacro "false", (vars,helpers,ftype)=> helpers.lua("false")
g\defmacro "nil", (vars,helpers,ftype)=> helpers.lua("nil")
infix = (ops)->
    for op in *ops
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
infix{"+","-","*","/","==","!=","<","<=",">",">=","^"}
unary{"-","#","not"}

g\test[[
say "foo"
===
Call [say %]:
  "foo"
]]

g\test[[
say (4)
===
Call [say %]:
  4
]]

g\test[[
rule "fart": say "poot"
===
Call [rule % %]:
  "fart"
  Thunk:
    Call [say %]:
      "poot"
]]

g\test[[
rule "doublefart":
    say "poot"
    say "poot"
===
Call [rule % %]:
  "doublefart"
  Thunk:
    Call [say %]:
      "poot"
    Call [say %]:
      "poot"
]]

g\test[[
say (subexpressions work)
===
Call [say %]:
  Call [subexpressions work]!
]]

g\test[[
say ["lists", "work"]
===
Call [say %]:
  List:
    "lists"
    "work"
]]

g\test[[
say []
===
Call [say %]:
  <Empty List>
]]

g\test[[
say [..]
    1, 2
    3
===
Call [say %]:
  List:
    1
    2
    3
]]

g\test[[
say both [..]
    1,2
..and [..]
    3,4
===
Call [say both % and %]:
  List:
    1
    2
  List:
    3
    4
]]

g\test[[
say both..
    "hello"
    and "world"
===
Call [say both % and %]:
  "hello"
  "world"
]]

g\test[[
say both ..
    "a list:"
    and [..]
        1,2,(three),(4)
===
Call [say both % and %]:
  "a list:"
  List:
    1
    2
    Call [three]!
    4
]]

g\test[[
if 1: yes
..else: no
===
Call [if % % else %]:
  1
  Thunk:
    Call [yes]!
  Thunk:
    Call [no]!
]]
g\test[[
if 1: yes ..else: no
===
Call [if % % else %]:
  1
  Thunk:
    Call [yes]!
  Thunk:
    Call [no]!
]]
g\test[[
say (do: return 5)
===
Call [say %]:
  Call [do %]:
    Thunk:
      Call [return %]:
        5
]]
g\test[[
say (..)
  fn call
===
Call [say %]:
  Call [fn call]!
]]

g\run[[

say [..]
    "this is a stupidly long list", "the items go way past the 80 character", "limit that older consoles"
    "had.", "It just keeps going and going"

rule "dumbfunc %a %b %c %d %e":
    say "doop"

dumbfunc..
    "this is a stupidly long set of arguments" "the items go way past the 80 character" "limit that older consoles"
    "had." "It just keeps going and going"

]]
g\run[[
rule "four": return 4
rule "say both %one and %two":
    say %one
    say %two

say both ..
    "a list:"
    and [..]
        1,2,3,(four),(5)

say "done"
]]

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

--g\defmacro "if %condition %if_body", "if %condition %if_body else: return nil"

g\def [[do %action]], (vars)=> return vars.action(self,vars)
g\run[[
rule "do %thing also %also-thing":
    do %thing
    do %also-thing
    return 99

do: say "one liner"
..also: say "another one liner"

say (..)
    do:
        say "hi"
        return 5
        say "bye"

say (do: return "wow")
if 1: say "hi1" ..else: say "bye1"

if 1: say "hi2"
..else: say "bye2"

]]
g\run[[
rule "foo %x":
    if %x:
        say "YES"
        55
    ..else:
        say "NO"
        -99

say (foo 1)
say (foo (false))

]]

g\run[[
say (1 + (-(2 * 3)))
]]

g\run_debug[[
for "x" in ["A","B","C"]:
    say %x
]]
g\run_debug[[
say (for "x" in [1,2,3]:%x + 100)
say (..)
    for "x" in [1,2,3]:
        %x + 200
]]
