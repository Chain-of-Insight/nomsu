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

game\def [[$invocations := $body]], (locals)=>
    game\def locals.invocations, locals.body
    return nil

game\def [[help $invocation]], (locals)=>
    with locals
        if @rules[.invocation\gsub(" ",";")]
            print(@rules[.invocation\gsub(" ",";")])
            return nil
        words = [w for w in .invocation\gmatch("%S+")]
        match_count = (i)->
            iws = [w for w in i\gmatch("[^;$]+")]
            count = 0
            for w in *words
                for iw in *iws
                    if w == iw then count += 1
            count += 1/#iws
            return count
        rules = {}
        for i,r in pairs(@rules)
            rules[r] = math.max((rules[r] or 0), match_count(i))
        best = [r for r in pairs rules]
        table.sort best, ((a,b)-> rules[a] > rules[b])
        if rules[best[1]] > 0
            for r in *best
                if rules[r] < rules[best[1]]
                    break
                print("Closest match for \"#{.invocation}\" is:\n#{r}")
        return nil


game\def [[return $retval]], ((locals)=> locals.retval), "... returns the specified value ..."

game\def {[[true]], [[yes]]}, ((locals)=> true), "... returns true ..."
game\def {[[false]], [[no]]}, ((locals)=> false), "... returns false ..."
game\def {[[nil]], [[None]], [[nop]], [[done]]}, ((locals)=> nil), "... does nothing, returns nil ..."

game\def {[[$x == $y]], [[equal $x $y]]}, (locals)=>
    with locals
        if .x == .y then return true
        if type(.x) != type(.y) then return false
        if type(.x) != 'table' then return false
        for k,v in pairs(.x)
            if .y[k] != v
                return false
        for k,v in pairs(.y)
            if .x[k] != v
                return false
        return true

game\def [[not $x]], (locals)=> not locals.x
game\run [["$x != $y" := {return (not (x == y))}]]
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

game\run [[
["if $condition then $body", "when $condition do $body"] := {if $condition then $body else {}}
]]
game\run [[
["unless $condition do $body"] := {if (not $condition) then $body else {}}
]]
game\run [[
["unless $condition do $body else $else_body"] := {if (not $condition) then $body else $else_body}
]]


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

game\def {[[argmin $items]]}, (locals)=>
    with locals
        min = .items[1]
        for i=2,#.items
            if .items[i][2] < min[2]
                min = .items[i]
        return min

game\def {[[argmax $items]]}, (locals)=>
    with locals
        max = .items[1]
        for i=2,#.items
            if .items[i][2] > max[2]
                max = .items[i]
        return max

game\def {[[$index st in $list]], [[$index nd in $list]], [[$index rd in $list]], [[$index th in $list]]}, (locals)=>
    locals.list[locals.index]

return game
