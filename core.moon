#!/usr/bin/env moon
nomic = require 'nomic'
game = nomic()

is_list = (t)->
    i = 0
    for _ in pairs(t)
        i += 1
        if t[i] == nil then return false
    return true

repr = (x)->
    if type(x) == 'table'
        if is_list x
            "[#{table.concat([repr(i) for i in *x], ", ")}]"
        else
            "{#{table.concat(["#{k}: #{v}" for k,v in pairs x], ", ")}}"
    else
        tostring(x)

game\def {[[print $str]], [[say $str]]}, (locals)=>
    with locals
        print(repr(.str))
        return nil

game\def [[help $invocation]], (locals)=>
    with locals
        if @rules[.invocation]
            print(@rules[.invocation])
            return nil
        for i,r in pairs(@rules)
            if i\match .invocation
                print("Rule: #{@rules[.invocation]}")
        return nil


game\def [[return $retval]], (locals)=> locals.retval

game\def [[do $thunk]], (locals)=>
    locals.thunk\run(@, locals)

game\def {[[true]], [[yes]]}, (locals)=> true
game\def {[[false]], [[no]]}, (locals)=> false
game\def {[[nil]], [[None]], [[nop]], [[done]]}, (locals)=> nil

game\def {[[$x == $y]], [[equal $x $y]]}, (locals)=>
    with locals
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

game\def {[[$x + $y]], [[$x plus $y]]}, (locals)=> locals.x + locals.y
game\def {[[$x - $y]], [[$x minus $y]]}, (locals)=> locals.x - locals.y
game\def {[[$x * $y]], [[$x times $y]]}, (locals)=> locals.x * locals.y
game\def {[[$x / $y]], [[$x divided by $y]]}, (locals)=> locals.x / locals.y
game\def {[[$x ^ $y]], [[$x to the power of $y]]}, (locals)=> locals.x ^ locals.y


game\def [[if $condition then $body else $else_body]], (locals)=>
    with locals
        if .condition
            return .body\run(@, locals)
        else return .else_body\run(@, locals)

game\def [[if $condition then $body]], [[if $condition then $body else {}]]
game\def [[when $condition do $body]], [[if $condition then $body else {}]]


game\def [[sum $items]], (locals)=>
    tot = 0
    for x in *locals.items do tot += x
    return tot

game\def {[[average $items]], [[avg $items]]}, (locals)=>
    tot = 0
    for x in *locals.items do tot += x
    return tot / #locals.items

game\def {[[min $items]], [[smallest $items]], [[lowest $items]], [[fewest $items]]}, (locals)=>
    with locals
        min = .items[1]
        for i=2,#.items
            if .items[i] < min
                min = .items[i]
        return min

game\def {[[max $items]], [[largest $items]], [[highest $items]], [[most $items]]}, (locals)=>
    with locals
        max = .items[1]
        for i=2,#.items
            if .items[i] > max
                max = .items[i]
        return max

return game
