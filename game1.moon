#!/usr/bin/env moon
game = require 'core'

export __DEBUG__
__DEBUG__ = true
------------------ BASIC TESTS ---------------------
game\run [[

say "=========== INITIALIZING GAME ============"

"fart" := {say "poot"}

fart

"fart twice" := {
    fart
    fart
}

fart twice

["greet", "say hello"] := { say "Hello!" }

greet

say (return "returned value")
do {say "did"}
say 6
say -6
say [1,2,3]

]]
error("done")

game\run "say [1,2,3]"
game\run "sum [1,2,3]"
game\run "say (sum [1,2,3])"


game\def [[you]], (_)=> @you
game\run [[you]]
game\run [[say (you)]]

game\def [[five]], [[return 5]]
game\run [[say (five)]]
game\def [[$x squared]], (locals)=> locals.x^2
game\run [[say ((five) squared)]]


game\def [[remember that $key $relation $value]], (locals)=>
    with locals
        assert .relation, "no relation!!"
        if not @relations[.relation] then @relations[.relation] = {}
        @relations[.relation][.key] = .value
        return nil

game\def [[remember that $key $relation]], [[remember that $key $relation (true)]]

game\def [[forget about $key $relation]], (locals)=>
    with locals
        if not @relations[.relation] then @relations[.relation] = {}
        @relations[.relation][.key] = nil
        return nil

game\def [[the value of $key $relation]], (locals)=>
    with locals
        return (@relations[.relation] or {})[.key]

game\def [[it is true that $key $relation $value]], [[return ((the value of $key $relation) == $value)]]

game\def [[it is true that $key $relation]], [[return ((the value of $key $relation) == (true))]]


game\run [[remember that "socrates" "is mortal"]]
game\run [[say (the value of "socrates" "is mortal")]]


game\def [[all keys where $relation is $value]], (locals)=>
    with locals
        result = {}
        for k,v in pairs(@relations[.relation] or {})
            if v == .value
                table.insert(result, k)
        return result

game\run [[if (1 == 1) then {say "Affirmative"} else {say "Negatory"}]]
game\run [[if (1 == 2) then {say "Affirmative"} else {say "Negatory"}]]
game\run [[if (1 == 2) then {say "Affirmative"}]]

game\run [[say (if (1 == 1) then {return "Ternary yes"} else {return "Ternary no"})]]

game\def [[$who is a member]], [[return (it is true that $who "is a member")]]
game\def [[you are a member]], [[return ((you) is a member)]]
game\run [[say (you are a member)]]
error("done")

game\run [[
    if (you are a member) then {say "youre a member!"} else {say "youre not a member"}
]]




[[
def: $who is a member
    return it is true that $who "is a member"

def: you are a member
    return (you) is a member

def: members
alias: all members
    return all keys where "is a member" is "yes"

def: let $someone join
    if (you are a member) then {
        remember that $someone "is a member"
    }

def: propose $def
    if (you are a member) then {
        remember that $def "is pending"
        vote yes on $def
    }

def: make $voter vote yes on $def
alias: make $voter approve $def
    remember $def "is approved by" $voter
    if ((all members) == (all $def "is approved by" ?)) then {
        forget $def "is approved by" ?
        add def $def
    }

def: vote yes on $def
    make (you) vote yes on $def

]]
