#!/usr/bin/env moon
nomic = require 'nomic'
game = nomic()


game\def [[say $str]], (locals)=>
    with locals
        print(.str)
        return nil

game\def [[return $retval]], (locals)=> locals.retval

game\def [[do $thunk]], (locals)=>
    locals.thunk\run(@, locals)

game\def {[[true]], [[yes]]}, (locals)=> true
game\def {[[false]], [[no]]}, (locals)=> false
game\def {[[nil]], [[None]], [[nop]]}, (locals)=> nil

game\run [[say "Hello world!"]]
game\run [[
    say "Hello!"
    say "World!"
]]

game\def {[[greet]], [[say hello]]}, [[say "Hello!"]]
game\run[[greet]]
game\run[[say hello]]


game\run [[say (return "returned value")]]

game\run [[do {say "did"}]]

game\run [[say 5]]
game\run [[say -5]]


game\def [[fart]], [[say "poot"]]
game\run [[fart]]
game\def [[fart twice]], [[
    say "poot"
    say "poot again"
]]
game\run [[fart twice]]

game\def [[sum $items]], (locals)=>
    tot = 0
    for x in *locals.items do tot += x
    return tot

game\run "say [1,2,3]"
game\run "sum [1,2,3]"
game\run "say (sum [1,2,3])"

game\def [[print $x]], [[say $x]]
game\run [[print "printing variables works"]]


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

game\def [[remember that $key $relation]], [[remember that $key $relation (true)]]

game\def [[forget about $key $relation]], (locals)=>
    with locals
        if not @relations[.relation] then @relations[.relation] = {}
        @relations[.relation][.key] = nil

game\def [[the value of $key $relation]], (locals)=>
    with locals
        return (@relations[.relation] or {})[.key]

game\def [[it is true that $key $relation $value]], [[return ((the value of $key $relation) == $value)]]

game\def [[it is true that $key $relation]], [[return ((the value of $key $relation) == (true))]]


game\run [[remember that "socrates" "is mortal"]]
game\run [[say (the value of "socrates" "is mortal")]]

game\def [[$x == $y]], (locals)=>
    with locals
        print("testing equality of #{.x} and #{.y}")
        if type(.x) != type(.y)
            return false
        if type(.x) == 'table'
            for k,v in pairs(.x)
                if .y[k] != v
                    return false
            for k,v in pairs(.y)
                if .x[k] != v
                    return false
            return true
        else
            return .x == .y

game\def [[not $x]], (locals)=> not locals.x

game\def [[$x != $y]], [[return (not (x == y))]]

game\def [[$x < $y]], (locals)=> locals.x < locals.y

game\def [[$x <= $y]], (locals)=> locals.x <= locals.y

game\def [[$x > $y]], (locals)=> locals.x > locals.y

game\def [[$x >= $y]], (locals)=> locals.x >= locals.y

-- TODO: the rest of the comparisons

game\def [[if $condition then $body else $else_body]], (locals)=>
    with locals
        if .condition
            return .body\run(@, locals)
        else return .else_body\run(@, locals)

game\def [[if $condition then $body]], [[if $condition then $body else {}]]
game\def [[when $condition do $body]], [[if $condition then $body else {}]]

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
