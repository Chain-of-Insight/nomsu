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

game\def {[[print $str]], [[say $str]]}, (args)=> print(repr(args.str))
game\def {[[printf $str]]}, (args)=>
    for s in *args.str do io.write(repr(s))
    io.write("\n")

game\def [[return $value]], (args)=> args.value

game\macro [[let $var = $value]], (var, value)->
    "locals[#{var}] = #{value}"

game\def [[$signature := $body]], (args)=>
    invocations = if type(args.signature) == 'table' then args.signature else {args.signature}
    for i in *invocations
        unless @check_authorization(i)
            print "You are not permitted to redefine this function"
            return

    @\def args.signature, args.body
    print "Defined new rule: \"#{game.repr(args.signature)}\""
    --print debug.getinfo(args.body, "S").source
    return nil

game\def [[compile $body]], (args)=>
    print debug.getinfo(args.body, "S").source
    return nil

game\def [[help $invocation]], (args)=>
    print_rule = (invocation, rule)->
        print "\"#{invocation}\" :="
        info = debug.getinfo(rule, "S")
        i = 0
        for line in info.source\gmatch("[^\n]+")
            i+=1
            if i >= info.linedefined+1
                print(line)
            if i >= info.lastlinedefined-1
                return
        print "    #{info.short_src}:#{info.linedefined}-#{info.lastlinedefined}"

    with args
        if rule = @rules[.invocation]
            print_rule .invocation, rule
            return nil
        words = [w for w in .invocation\gmatch("%S+")]
        match_count = (i)->
            iws = [w for w in i\gmatch("[^ $]+")]
            count = 0
            for w in *words
                for iw in *iws
                    if w == iw then count += 1
            count += 1/#iws
            return count
        rules = {}
        invocations = {}
        for i,r in pairs(@rules)
            c = match_count(i)
            if c > (rules[r] or 0)
                rules[r] = c
                invocations[r] = i
        best = [r for r in pairs rules]
        table.sort best, ((a,b)-> rules[a] > rules[b])
        if rules[best[1]] > 0
            for r in *best
                if rules[r] < rules[best[1]]
                    break
                print_rule invocations[r], r
        return nil

game\macro "true", -> "true"
game\macro "yes", -> "true"
game\macro "false", -> "false"
game\macro "no", -> "false"
game\macro "nil", -> "nil"
game\macro "None", -> "nil"
game\macro "null", -> "nil"
game\def [[nop]], ((args)=> nil), "... does nothing, returns nil ..."

game\def [[$x == $y]], (args)=>
    with args
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
game\run [=[
    ["$x != $y", "$x <> $y", "$x ~= $y"] := {return (not (x == y))}
]=]
game\def [[$x < $y]], (args)=> args.x < args.y
game\def [[$x <= $y]], (args)=> args.x <= args.y
game\def [[$x > $y]], (args)=> args.x > args.y
game\def [[$x >= $y]], (args)=> args.x >= args.y

game\macro [[$x + $y]], (x,y)-> "(#{x} + #{y})"
game\macro [[$x - $y]], (x,y)-> "(#{x} - #{y})"
game\macro [[$x * $y]], (x,y)-> "(#{x} * #{y})"
game\macro [[$x / $y]], (x,y)-> "(#{x} / #{y})"
game\macro [[$x ^ $y]], (x,y)-> "(#{x} ^ #{y})"

game\macro [[$x < $y]], (x,y)-> "(#{x} < #{y})"
game\macro [[$x <= $y]], (x,y)-> "(#{x} <= #{y})"
game\macro [[$x > $y]], (x,y)-> "(#{x} > #{y})"
game\macro [[$x >= $y]], (x,y)-> "(#{x} >= #{y})"

game\macro [[not $x]], (x)-> "(not #{x})"
game\macro [[$x and $y]], (x,y)-> "(#{x} and #{y})"
game\macro [[$x or $y]], (x,y)-> "(#{x} or #{y})"


[==[
game\def [[if $condition $body else $else_body]], (args)=>
    with args
        if .condition
            return .body(@, args)
        else return .else_body(@, args)

game\run [=[
    ["if $condition $body", "when $condition $body"] := {if $condition $body else {}}
    ["unless $condition $body"] := {if (not $condition) $body else {}}
    ["unless $condition $body else $else_body"] := {if (not $condition) $body else $else_body}
]=]
]==]

game\def [[random]], -> math.random()

game\def [[sum $items]], (args)=>
    tot = 0
    for x in *args.items do tot += x
    return tot

game\def [[all $items]], (args)=>
    for x in *args.items
        if not x then return false
    return true

game\def [[any $items]], (args)=>
    for x in *args.items
        if x then return true
    return false

game\def {[[average $items]], [[avg $items]]}, (args)=>
    tot = 0
    for x in *args.items do tot += x
    return tot / #args.items

game\def {[[min $items]], [[smallest $items]], [[lowest $items]], [[fewest $items]]}, (args)=>
    with args
        min = .items[1]
        for i=2,#.items
            if .items[i] < min
                min = .items[i]
        return min

game\def {[[max $items]], [[largest $items]], [[highest $items]], [[most $items]]}, (args)=>
    with args
        max = .items[1]
        for i=2,#.items
            if .items[i] > max
                max = .items[i]
        return max

game\def {[[argmin $items]]}, (args)=>
    with args
        min = .items[1]
        for i=2,#.items
            if .items[i][2] < min[2]
                min = .items[i]
        return min

game\def {[[argmax $items]]}, (args)=>
    with args
        max = .items[1]
        for i=2,#.items
            if .items[i][2] > max[2]
                max = .items[i]
        return max

game\def {[[$index st in $list]], [[$index nd in $list]], [[$index rd in $list]], [[$index th in $list]]}, (args)=>
    with args
        if type(.list) != 'table'
            print "Not a list: #{.list}"
            return
        .list[.index]

game\def {[[index of $item in $list]]}, (args)=>
    with args
        if type(.list) != 'table'
            print "Not a list: #{.list}"
            return
        for i,x in ipairs .list
            if x == .item
                return i
        return nil
game\run [=[
    ["$item is in $list", "$list contains $item"] := {(index of $item in $list) != (nil)}
]=]

game\def {[[# $list]], [[length of $list]], [[size of $list]]}, (args)=>
    with args
        if type(.list) != 'table'
            print "Not a list: #{.list}"
            return
        #.list


return game
