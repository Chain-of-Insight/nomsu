re = require 're'
lpeg = require 'lpeg'
moon = require 'moon'
type = moon.type

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

currently_parsing = nil
macros = nil
indentation = 0
indents = ->
    ("    ")\rep(indentation)
indent = ->
    export indentation
    indentation += 1
dedent = ->
    export indentation
    indentation -= 1
indent_block = (block)->
    block = block\gsub("\n", "\n"..indents!)
    return indents!..block
add_line = (lines, new_line)->
    table.insert lines, (indents!..new_line)

compactify_invocation = (raw_invocation)->
    name_bits = {}
    arg_names = {}
    for chunk in raw_invocation\gmatch("%S+")
        if chunk\sub(1,1) == "$"
            table.insert name_bits, "$"
            table.insert arg_names, chunk\sub(2,-1)
        else
            table.insert name_bits, chunk
    invocation = table.concat name_bits, " "
    return invocation, arg_names

Number = (s)-> s

String = (s)-> '"'..s..'"'

List = (t)-> "{" .. table.concat(t, ", ") .. "}"

Var = (s)-> "locals[\"#{s}\"]"

Word = (s)-> setmetatable({type:'word', text:s}, {__tostring:=> error("Cannot convert word \"#{@text}\" to string")})

Conditional = (condition, if_block, else_block)->
    ret = {}
    add_line ret, "(function()"
    indent!
    add_line ret, "local ret"
    table.insert ret, indent_block("local condition = #{condition}")

    add_line ret, "if condition then"
    indent!
    table.insert ret, indent_block("ret = (#{if_block})(game, locals)")
    if else_block
        dedent!
        add_line ret, "else"
        indent!
        table.insert ret, indent_block("ret = (#{else_block})(game, locals)")
    dedent!
    add_line ret, "end"
    add_line ret, "return ret"
    dedent!
    add_line ret, "end)()"
    code = table.concat(ret, "\n")
    return code

FunctionCall = (tokens)->
    words = [(if t.type == 'word' then t.text else "$") for t in *tokens]
    args = [t for t in *tokens when t.type != 'word']
    rule_name = table.concat(words, " ")
    if rule_name == "$"
        error("Empty rule: #{repr(tokens)}")

    if macros[rule_name]
        return macros[rule_name](unpack(args))

    if #args == 0
       return indent_block("game:call(\"#{rule_name}\")")

    ret = {}
    add_line ret, "game:call(\"#{rule_name}\","
    indent!
    arg_strs = [indent_block(arg) for arg in *args]
    dedent!
    table.insert ret, table.concat(arg_strs, ",\n")
    add_line ret, ")"

    code = table.concat(ret, "\n")
    return code

Thunk = (lines)->
    ret = {}
    add_line ret, "function(game, locals)"
    indent!
    for i,line in ipairs lines
        if line\match "locals%[\".*\"%] = .*"
            table.insert ret, indent_block(line)
        elseif i == #lines
            table.insert ret, indent_block("return "..line..";")
        else
            table.insert ret, indent_block(line..";")
    dedent!
    add_line ret, "end"
    return table.concat(ret, "\n")

lingo = [[
    actions <- {| {:startPos: {}:} (%ws*) (%nl %ws*)* (action ((%nl %ws*)+ action)*)? (%nl %ws*)* {:endPos: {}:} |} -> Thunk
    action <- (&conditional conditional) / ({| token (%ws+ token)* %ws* |} -> FunctionCall)
    conditional <- ("if" !%wordchars %ws* expression %ws* thunk (%ws* "else" %ws* thunk %ws*)?) -> Conditional
    token <- expression / (!"$" {%wordchars+} -> Word)
    expression <- number / string / list / variable / thunk / subexpression
    number <- ('-'? [0-9]+ ("." [0-9]+)?) -> Number
    string <- ('"' {(("\\" .) / [^"])*} '"') -> String
    list <- ({| '[' %ws* (%nl %ws*)* (expression (',' %ws* (%nl %ws*)* expression)*)? %ws* (%nl %ws*)* ']' |}) -> List
    variable <- ("$" {%wordchars+}) -> Var
    subexpression <- ("(" %ws* action ")")
    thunk <- ("{" actions "}")
    keywords <- "if" / "else" / "let"
]]
defs = {
    :Var
    :Word
    :String
    :Number
    :List
    :FunctionCall
    :Thunk
    :Conditional
    ws: lpeg.S(" \t")
    wordchars: lpeg.P(1)-lpeg.S(' \t\n,{}[]()"')
}
lingo = re.compile lingo, defs

cross_compile = (nomic_code)->
    export currently_parsing
    old_parsing = currently_parsing
    currently_parsing = nomic_code
    lua_code = lingo\match nomic_code
    currently_parsing = old_parsing
    return lua_code

defaulttable = (table,key)->
    new = {}
    table[key] = new
    return new

class Game
    new:(@parent)=>
        @rules = setmetatable({}, {__index:parent and parent.rules or nil})
        @macros = setmetatable({}, {__index:parent and parent.macros or nil})
        @arg_names = setmetatable({}, {__index:parent and parent.arg_names or nil})
        @relations = setmetatable({}, {__index:parent and parent.relations or defaulttable})
        @invocations = setmetatable({}, {__index:parent and parent.invocations or nil})
        @authorized = setmetatable({}, {__index:parent and parent.authorized or nil})
        @callstack = {}
        @debug = false
        @you = "Anonymous"

    def: (invocations, fn)=>
        if not fn then fn = false
        invocations = if type(invocations) == 'table' then invocations else {invocations}
        if fn then @invocations[fn] = {}
        else @invocations[fn] = false
        for raw_invocation in *invocations
            invocation, arg_names = compactify_invocation raw_invocation
            if invocation == "$"
                error("Anonymous function: #{raw_invocation}")
            @rules[invocation] = fn
            if fn
                table.insert @invocations[fn], invocation
                @arg_names[invocation] = arg_names
            else
                @arg_names[invocation] = false
    
    macro: (invocation, fn)=>
        invocation, _ = compactify_invocation invocation
        @macros[invocation] = fn
    
    undefine: (invocations)=>
        @\def invocations, false
    
    all_aliases: (invocations)=>
        if type(invocations) != 'table' then invocations = {invocations}
        all_aliases = {}
        for i in *invocations
            all_aliases[i] = true
            if not @invocations[@rules[i]]
                error "Could not find aliases of [[#{i}]]"
            for alias in *@invocations[@rules[i]]
                all_aliases[alias] = true
        return [a for a in pairs(all_aliases)]

    canonicalize: (invocations)=>
        if type(invocations) == 'string'
            return @invocations[@rules[invocations]][1]
        canonicals = {}
        for i in *invocations
            if @rules[i] == nil
                error "Attempt to canonicalize invalid invocation: #{i}"
            canonicals[@invocations[@rules[i]][1]] = true
        return [c for c in pairs canonicals]

    set_whitelist: (actions, whitelist)=>
        if is_list whitelist then whitelist = {w,true for w in *whitelist}
        for action in *@all_aliases(actions)
            @authorized[action] = whitelist
    
    check_authorization:(action)=>
        authority = @authorized[action]
        return true if authority == nil
        for call in *@callstack
            if authority[call] then return true
        return false

    run: (str, user)=>
        user or= "anon"

        if @debug
            print("SOURCE NOMIC CODE:\n#{str}")
        export macros
        old_macros = macros
        macros = (self.macros)
        lua_code = cross_compile str
        macros = old_macros
        if @debug
            print("\nGENERATED LUA CODE:\n#{lua_code}")

        lua_thunk, err = loadstring("return "..lua_code)
        if not lua_thunk
            print("Parsing: "..lua_code)
            error(err)
        lua_fn = lua_thunk!
        
        ret = lua_fn @, {}
        return ret

    call: (invocation, ...)=>
        if not @rules[invocation]
            error "Could not find rule: '#{invocation}'"
        if not @\check_authorization invocation
            print "Not authorized to #{invocation} from callstack: #{repr(@callstack)}"
            return
        table.insert @callstack, invocation
        arg_names = @arg_names[invocation]
        args = {...}
        ret = (@rules[invocation])(@, {arg_names[i],arg for i, arg in ipairs(args)})
        table.remove @callstack
        return ret

    run_debug:(...)=>
        @debug = true
        print("Debugging:")
        @run ...
        @debug = false

    repl:=>
        while true
            io.write(">> ")
            buf = ""
            while buf\sub(-2,-1) != "\n\n"
                buf ..= io.read("*line").."\n"
            if buf == "exit\n\n" or buf == "quit\n\n"
                break
            @\run buf

    repr: repr

return Game
