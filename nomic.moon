re = require 're'
lpeg = require 'lpeg'
moon = require 'moon'
type = moon.type

export __DEBUG__

invocation_def = [[
    name <- {| {chunk} (" " {chunk})* |} -> semicolonify
    chunk <- ({"$"} %S+) / ({%S+})
]]
invocation_def = re.compile(invocation_def, {semicolonify: (bits) -> table.concat(bits, ";")})


as_value = (x, globals, locals)->
    assert (globals and locals), "Shit's fucked"
    if type(x) == 'number' or type(x) == 'string' or type(x) == 'table'
        return x
    ret = x\as_value globals, locals
    return ret

class Var
    new:(@name)=>
    as_value:(globals, locals)=>
        if __DEBUG__
            print("Looking up variable #{@name} = #{locals[@name]}")
        if locals[@name] == nil
            print("LOCALS:")
            for k,v in pairs locals
                print("    #{k} = #{v}")
            print("GLOBALS:")
            for k,v in pairs globals
                print("    #{k} = #{v}")
            error("Could not find #{@name}")
        locals[@name]
    __tostring:=> "Var(#{@text})"

class Word
    new:(@text)=>
    __tostring:=> "Word(#{@text})"

class Action
    new:(tokens)=>
        words = [(if type(t) == Word then t.text else "$") for t in *tokens]
        @name = table.concat(words, ";")
        if __DEBUG__
            print("ACTION: #{@name}")
            for t in *tokens
                print("    TOKEN: #{t}")
        @args = [t for t in *tokens when type(t) != Word]

    __tostring:=> "Action(#{@name})"

    as_value:(globals, locals)=>
        assert((globals and locals), "f'd up")
        ret = @\run globals, locals
        return ret

    run:(globals, locals)=>
        assert((globals and locals), "f'd up")
        rule = globals.rules[@name]
        unless rule
            error("Tried to run rule, but couldn't find: #{@name}")
        arg_names = rule.arg_names
        new_locals = {}
        for i, arg in ipairs(@args)
            new_locals[arg_names[i]] = as_value(arg, globals, locals)

        ret = rule.fn(globals, new_locals)
        return ret

class Thunk
    new:(@actions)=>
        if __DEBUG__
            print("CONSTRUCTING THUNK WITH ACTIONS:")
            for k,v in pairs @actions
                print("    #{k} = #{v}")
    as_value:=>@
    run:(globals, locals)=>
        assert((globals and locals), "f'd up")
        ret = nil
        for a in *@actions
            ret = a\run globals,locals
        return ret
    __tostring:=>
        "Thunk(#{table.concat([tostring(a) for a in *@actions], ", ") .. tostring(@actions.returnValue or "") })"

lingo = [[
    actions <- {| (%nl " "*)* ((" "*) action ((%nl " "*)+ action)*)? (%nl " "*)* |} -> Thunk
    action <- {| token (" "+ token)* " "* |} -> Action
    token <- expression / ({(!"[" [^ {}()$])+} -> Word)
    expression <- number / string / list / variable / thunk / subexpression
    number <- ('-'? [0-9]+ ("." [0-9]+)?) -> tonumber
    string <- ('"' {(("\\" .) / [^"])*} '"') -> tostring
    list <- {| '[' (expression (',' (' '*) expression)*)? ']' |}
    variable <- ("$" {%S+}) -> Var
    subexpression <- "(" action ")"
    thunk <- ("{" {| actions |} "}") -> Thunk
]]
defs = {
    Var: (...)->Var(...)
    Word: (...)->Word(...)
    Action: (...)->Action(...)
    Thunk: (...)->Thunk(...)
    tostring: tostring
    tonumber: tonumber
}
lingo = re.compile lingo, defs

class Rule
    new:(invocations, action)=>
        if type(action) == 'string'
            @body_str = action
            thunk = lingo\match action
            unless thunk
                error("failed to parse!")
            @fn = (globals,locals)-> thunk\run(globals, locals)
        else
            @body_str = "<lua function>"
            @fn = action

        eq = (x,y)->
            if #x != #y then return false
            for i=1,#x
                if x[i] != y[i] then return false
            return true

        @raw_invocations = invocations
        @invocations = {}
        for raw_invocation in *invocations
            name_bits = {}
            arg_names = {}
            for chunk in raw_invocation\gmatch("%S+")
                if chunk\sub(1,1) == "$"
                    table.insert name_bits, "$"
                    table.insert arg_names, chunk\sub(2,-1)
                else
                    table.insert name_bits, chunk
            if @arg_names
                assert(eq(arg_names, @arg_names), "Attempt to use an alias with different variables")
            else
                @arg_names = arg_names
            invocation = table.concat name_bits, ";"
            table.insert @invocations, invocation

    __tostring:=>
        "Rule: #{table.concat(@raw_invocations, " | ")} :=\n  #{@body_str\gsub("\n","\n  ")}"


def = (game, invocation, action)->
    invocations = if type(invocation) == 'table' then invocation else {invocation}
    rule = Rule(invocations, action)
    for invocation in *rule.invocations
        game.rules[invocation] = rule
    print rule

run = (game, str)->
    print(">> #{str\gsub("\n", "\n.. ")}")
    thunk = lingo\match str
    unless thunk
        error("failed to parse nomic:\n#{str}")
    game.you = "@spill"
    ret = thunk\run game, {}
    if ret != nil
        print("= #{ret}")
    return ret

return ()->
    {rules:{}, relations:{}, :run, :def}
