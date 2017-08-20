#!/usr/bin/env moon
utils = require 'utils'
Game = require 'nomic_whitespace'
g = Game()

g\def "rule %spec %body", (vars)=>
    self\def vars.spec, vars.body
    print "Defined rule: #{vars.spec}"

g\defmacro("lua %code", ((args)->
    print("entering macro...: #{utils.repr(args)}")
    return args[1].value
), true)

g\defmacro("macro %spec %body", ((spec,body)->
    print("entering macro...: #{utils.repr(spec,true)} / #{utils.repr(body,true)}")
    -- TODO: parse better
    lua_thunk, err = loadstring("return "..spec)
    if not lua_thunk
        error("Failed to compile")
    spec = lua_thunk!
    print"SPEC IS NOW #{utils.repr(spec,true)}"
    g\defmacro spec, (args,blargs)->
        print("entering macro...: #{utils.repr(spec,true)} / #{utils.repr(body,true)}")
        return body
    return "nil"
), false)

g\def "say %x", (vars)=>
    print(utils.repr(vars.x))

g\def "return %x", (vars)=>
    return vars.x

g\run_debug[[
say "hello world!"

rule "fart": say "poot"
rule "doublefart":
    say "poot"
    say "poot"

fart
doublefart

rule "say both %x and %y":
    say %x
    say %y

say both "vars" and "work!"

say ( return "subexpressions work" )

say "goodbye"

say [1,2,3]

say [..]
    1, 2, 3
    4, 5

say both [..]
    1,2,3
..and [..]
    4,5,6

say both..
    "hello"
    and "world"


rule "four": return 4
say both..
    "a list:"
    and [..]
        1,2,3,(four),(5)

say "done"
]]
