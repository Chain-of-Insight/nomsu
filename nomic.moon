re = require 're'
lpeg = require 'lpeg'
moon = require 'moon'
type = moon.type

export __DEBUG__

currently_parsing = nil


as_value = (x, game, locals)->
    assert (game and locals), "Shit's fucked"
    if type(x) == 'number' or type(x) == 'string' or type(x) == 'table'
        return x
    ret = x\as_value game, locals
    return ret

class Var
    new:(@name)=>
    as_value:(game, locals)=>
        if __DEBUG__
            print("Looking up variable #{@name} = #{locals[@name]}")
        if locals[@name] == nil
            print("LOCALS:")
            for k,v in pairs locals
                print("    #{k} = #{v}")
            print("game:")
            for k,v in pairs game
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

    as_value:(game, locals)=>
        assert((game and locals), "f'd up")
        ret = @\run game, locals
        return ret

    run:(game, locals)=>
        assert((game and locals), "f'd up")
        rule = game.rules[@name]
        unless rule
            error("Tried to run rule, but couldn't find: #{@name}")
        arg_names = rule.arg_names
        new_locals = {}
        for i, arg in ipairs(@args)
            new_locals[arg_names[i]] = as_value(arg, game, locals)

        ret = rule.fn(game, new_locals)
        return ret

class Thunk
    new:(@actions)=>
        if @actions.startPos
            assert currently_parsing, "Not currently parsing!"
        @body_str = currently_parsing\sub(@actions.startPos, @actions.endPos-1)
        if __DEBUG__
            print("CONSTRUCTING THUNK WITH ACTIONS:")
            for k,v in pairs @actions
                print("    #{k} = #{v}")
    as_value:=>@
    run:(game, locals)=>
        assert((game and locals), "f'd up")
        ret = nil
        for a in *@actions
            ret = a\run game,locals
        return ret
    __tostring:=>
        --"Thunk(#{table.concat([tostring(a) for a in *@actions], ", ") .. tostring(@actions.returnValue or "") })"
        "{#{@body_str}}"

lingo = [[
    actions <- {| {:startPos: {}:} (%ws*) (%nl %ws*)* (action ((%nl %ws*)+ action)*)? (%nl %ws*)* {:endPos: {}:} |} -> Thunk
    action <- {| token (%ws+ token)* %ws* |} -> Action
    token <- expression / (!"$" {%wordchars+} -> Word)
    expression <- number / string / list / variable / thunk / subexpression
    number <- ('-'? [0-9]+ ("." [0-9]+)?) -> tonumber
    string <- ('"' {(("\\" .) / [^"])*} '"') -> tostring
    list <- {| '[' (expression (',' (%ws*) expression)*)? ']' |}
    variable <- ("$" {%wordchars+}) -> Var
    subexpression <- ("(" %ws* action ")")
    thunk <- ("{" actions "}")
]]
defs = {
    Var: (...)->Var(...)
    Word: (...)->Word(...)
    Action: (...)->Action(...)
    Thunk: (...)->Thunk(...)
    tostring: tostring
    tonumber: tonumber
    ws: lpeg.S(" \t")
    wordchars: lpeg.P(1)-lpeg.S(' \t\n{}[]()"')
}
lingo = re.compile lingo, defs

class Rule
    new:(invocations, action)=>
        if type(action) == 'string'
            @body_str = action
            export currently_parsing
            old_parsing = currently_parsing
            currently_parsing = action
            thunk = lingo\match action
            currently_parsing = old_parsing
            unless thunk
                error("failed to parse!")
            @fn = (game,locals)-> thunk\run(game, locals)
        elseif type(action) == Thunk
            @body_str = tostring(action)
            @fn = (game,locals)-> action\run(game, locals)
        elseif type(action) == 'function'
            @body_str = "<lua function>"
            @fn = action
        else
            error("Invalid action type: #{type(action)}")

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
    if game.src
        rule.body_str = game.src
    for invocation in *rule.invocations
        game.rules[invocation] = rule
    if __DEBUG__
        print rule

run = (game, str)->
    if __DEBUG__
        print(">> #{str\gsub("\n", "\n.. ")}")
    export currently_parsing
    old_parsing = currently_parsing
    currently_parsing = str
    thunk = lingo\match str
    currently_parsing = old_parsing
    unless thunk
        error("failed to parse nomic:\n#{str}")
    -- TODO: remove
    game.you = "@spill"
    ret = thunk\run game, {}
    if ret != nil
        print("= #{ret}")
    return ret

repl = (game)->
    while true
        io.write(">> ")
        buf = ""
        while buf\sub(-2,-1) != "\n\n"
            buf ..= io.read("*line").."\n"
        if buf == "exit\n\n" or buf == "quit\n\n"
            break
        game\run buf

return ()->
    game = {rules:{}, relations:{}, :run, :def, :repl}
    game\def [[$invocations := $body]], (locals)=>
        game\def locals.invocations, locals.body
        return nil
    return game
