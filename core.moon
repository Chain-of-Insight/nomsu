#!/usr/bin/env moon
nomic = require 'nomic'
utils = require 'utils'
game = nomic()

game\def {[[print $str]], [[say $str]]}, (args)=> print(utils.repr(args.str))
game\def {[[printf $str]]}, (args)=>
    for s in *args.str do io.write(utils.repr(s))
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
    print "Defined new rule: #{utils.repr(args.signature)}"
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
        utils.sort best, rules
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

game\def [[$x == $y]], (args)=> utils.equivalent(args.x, args.y)
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

game\def [[sum $items]], (args)=> utils.sum(args.items)
game\def [[all $items]], (args)=> utils.all(args.items)
game\def [[any $items]], (args)=> utils.any(args.items)
game\def {[[average $items]], [[avg $items]]}, (args)=> utils.sum(items)/#items
game\def {[[min $items]], [[smallest $items]], [[lowest $items]], [[fewest $items]]}, (args)=>
    utils.min(args.items)

game\def {[[max $items]], [[largest $items]], [[highest $items]], [[most $items]]}, (args)=>
    utils.max(args.items)

game\def {[[argmin $items]]}, (args)=>
    utils.min(args.items, ((i)->i[2]))
game\def {[[argmax $items]]}, (args)=>
    utils.max(args.items, ((i)->i[2]))

game\def {[[min $items with respect to $keys]]}, (args)=>
    utils.min(args.items, args.keys)
game\def {[[max $items with respect to $keys]]}, (args)=>
    utils.max(args.items, args.keys)

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
        utils.key_for(args.list, args.item)

game\run [=[
    ["$item is in $list", "$list contains $item"] := {(index of $item in $list) != (nil)}
]=]

game\def {[[# $list]], [[length of $list]], [[size of $list]]}, (args)=>
    with args
        if type(.list) != 'table'
            print "Not a list: #{.list}"
            return
        return #(.list)


return game
