re = require 're'
lpeg = require 'lpeg'
utils = require 'utils'
moon = require 'moon'

lpeg.setmaxstack 10000 -- whoa

linebreak = lpeg.P("\r")^-1 * lpeg.P("\n")

get_line_indentation = (line)->
    indent_amounts = {[" "]:1, ["\t"]:4}
    with sum = 0
        leading_space = line\gsub("([\t ]*).*", "%1")
        for c in leading_space\gmatch "[\t ]"
            sum += indent_amounts[c]

pos_to_line = (str,pos)->
    line_no = 1
    for line in str\gmatch("[^%n]+")
        if #line >= pos then return line, line_no
        pos -= (#line + 1)
        line_no += 1
    error "Failed to find position #{pos} in str"

pos_to_line_no = (str,pos)->
    with line = 1
        for _ in str\sub(1, pos)\gmatch("\n")
            line += 1

add_indent_tokens = (str)->
    indent_stack = {0}
    result = {}
    -- TODO: Store mapping from new line numbers to old ones
    defs =
        linebreak: linebreak
        process_line: (line)->
            -- Remove blank lines
            unless line\match"[^ \t\n]"
                return
            indent = get_line_indentation(line)
            if indent > indent_stack[#indent_stack]
                table.insert result, "{\n "
                table.insert indent_stack, indent
            elseif indent < indent_stack[#indent_stack]
                dedents = 0
                tokens = {}
                while indent < indent_stack[#indent_stack]
                    table.remove indent_stack
                    table.insert tokens, "}"
                table.insert tokens, " "
                table.insert result, table.concat(tokens, "\n")
            else
                table.insert result, " "
            -- Delete leading whitespace
            --line = line\gsub("[ \t]*", "", 1)
            -- Delete trailing whitespace and carriage returns
            line = line\gsub("[ \t\r]*\n", "\n", 1)
            table.insert result, line

    indentflagger = [=[
        file <- line*
        line <- ((string / [^%linebreak])* %linebreak) -> process_line
        string <- '"' (("\\" .) / [^"])* '"'
    ]=]
    indentflagger = re.compile indentflagger, defs
    indentflagger\match(str.."\n")
    while #indent_stack > 1
        table.remove indent_stack
        table.insert result, "}\n"
    return (table.concat result)\sub(1,-2)

lingo = [=[
    file <- ({ {| {:body: (" " block) :} ({:errors: errors :})? |} }) -> File
    errors <- ({ {.+} }) -> Errors
    block <- ({ {| statement (%nodent statement)* |} }) -> Block
    statement <- ({ (functioncall / expression) }) -> Statement
    one_liner <- ({ {|
            (({ 
                (({ {|
                    (expression (%word_boundary fn_bit)+) / (word (%word_boundary fn_bit)*)
                |} }) -> FunctionCall)
             }) -> Statement)
        |} }) -> Block

    functioncall <- ({ {| (expression %word_boundary fn_bits) / (word (%word_boundary fn_bits)?) |} }) -> FunctionCall
    fn_bit <- (expression / word)
    fn_bits <-
        ((".." %ws? (%indent %nodent indented_fn_bits %dedent) (%nodent ".." %ws? fn_bits)?)
         / (%nodent ".." fn_bit fn_bits)
         / (fn_bit (%word_boundary fn_bits)?))
    indented_fn_bits <-
        fn_bit ((%nodent / %word_boundary) indented_fn_bits)?
    
    thunk <-
        ({ ":" %ws?
           ((%indent %nodent block %dedent (%nodent "..")?)
            / (one_liner (%ws? ((%nodent? "..")))?)) }) -> Thunk

    word <- ({ !number {%wordchar+} }) -> Word
    expression <- ({ (string / number / variable / list / thunk / subexpression) }) -> Expression

    string <- ({ '"' {(("\\" .) / [^"])*} '"' }) -> String
    number <- ({ {'-'? [0-9]+ ("." [0-9]+)?} }) -> Number
    variable <- ({ ("%" {%wordchar+}) }) -> Var

    subexpression <-
        ("(" %ws? (functioncall / expression) %ws? ")")
        / ("(..)" %ws? %indent %nodent (expression / (({ {| indented_fn_bits |} }) -> FunctionCall)) %dedent (%nodent "..")?)

    list <- ({ {|
        ("[..]" %ws? %indent %nodent indented_list ","? %dedent (%nodent "..")?)
        / ("[" %ws? (list_items ","?)?  %ws?"]")
      |} }) -> List
    list_items <- (expression (list_sep list_items)?)
    list_sep <- %ws? "," %ws?
    indented_list <-
        expression (((list_sep %nodent?) / %nodent) indented_list)?
]=]

wordchar = lpeg.P(1)-lpeg.S(' \t\n\r%:;,.{}[]()"')
defs =
    eol: #(linebreak) + (lpeg.P("")-lpeg.P(1))
    ws: lpeg.S(" \t")^1
    wordchar: wordchar
    indent: linebreak * lpeg.P("{") * lpeg.S(" \t")^0
    nodent: linebreak * lpeg.P(" ") * lpeg.S(" \t")^0
    dedent: linebreak * lpeg.P("}") * lpeg.S(" \t")^0
    word_boundary: lpeg.S(" \t")^1 + lpeg.B(lpeg.P("..")) + lpeg.B(lpeg.S("\";)]")) + #lpeg.S("\":([") + #lpeg.P("..")

setmetatable(defs, {
    __index: (t,key)->
        --print("WORKING for #{key}")
        fn = (src, value, ...)->
            token = {type: key, src:src, value: value}
            return token
        t[key] = fn
        return fn
})
lingo = re.compile lingo, defs


class Game
    new:(parent)=>
        @defs = setmetatable({}, {__index:parent and parent.defs})
        @macros = setmetatable({}, {__index: parent and parent.macros})
        @debug = false

    call: (fn_name,...)=>
        if @defs[fn_name] == nil
            error "Attempt to call undefined function: #{fn_name}"
        {fn, arg_names} = @defs[fn_name]
        if @debug
            print("Calling #{fn_name}...")
        args = {}
        for i,name in ipairs(arg_names)
            args[name] = select(i,...)
            if @debug
                print("arg #{utils.repr(name,true)} = #{utils.repr(select(i,...), true)}")
        ret = fn(self, args)
        if @debug
            print "returned #{utils.repr(ret,true)}"
        return ret

    def: (spec, fn)=>
        invocations,arg_names = self\get_invocations spec
        for invocation in *invocations
            @defs[invocation] = {fn, arg_names}

    get_invocations:(text)=>
        if type(text) == 'string' then text = {text}
        invocations = {}
        local arg_names
        for _text in *text
            name_bits = {}
            _arg_names = {}
            for chunk in _text\gmatch("%S+")
                if chunk\sub(1,1) == "%"
                    table.insert name_bits, "%"
                    table.insert _arg_names, chunk\sub(2,-1)
                else
                    table.insert name_bits, chunk
            invocation = table.concat name_bits, " "
            table.insert(invocations, invocation)
            if arg_names and not utils.equivalent(utils.set(arg_names), utils.set(_arg_names))
                error("Conflicting argument names #{utils.repr(arg_names)} and #{utils.repr(_arg_names)} for #{utils.repr(text)}")
            arg_names = _arg_names
        return invocations, arg_names

    defmacro: (spec, fn)=>
        invocations,arg_names = self\get_invocations spec
        for invocation in *invocations
            @macros[invocation] = {fn, arg_names}

    simplemacro: (spec, replacement)=>
        replace_grammar = [[
            stuff <- {~ (var / string / .)+ ~}
            var <- ("%" {%wordchar+}) -> replacer
            string <- '"' (("\\" .) / [^"])* '"'
        ]]
        replacement = add_indent_tokens replacement
        fn = (vars, helpers, ftype)=>
            replacer = (varname)->
                ret = vars[varname].src
                return ret
            replacement_grammar = re.compile(replace_grammar, {:wordchar, :replacer})
            replaced = replacement_grammar\match(replacement)
            tree = lingo\match (replaced)
            if not tree
                error "Couldn't match:\n#{replaced}"
            helpers.lua(helpers.transform(tree.value.body))
            return code
        self\defmacro spec, fn
    
    run: (text)=>
        if @debug
            print("RUNNING TEXT:\n")
            print(text)
        code = self\compile(text)
        if @debug
            print("\nGENERATED LUA CODE:")
            print(code)
        lua_thunk, err = loadstring(code)
        if not lua_thunk
            error("Failed to compile generated code:\n#{code}")
        action = lua_thunk!
        if @debug
            print("Running...")
        return action(self, {})
    
    run_debug:(text)=>
        old_debug = @debug
        @debug = true
        ret = self\run(text)
        @debug = old_debug
        return ret

    parse: (str)=>
        if @debug
            print("PARSING:\n#{str}")
        indentified = add_indent_tokens str
        if @debug
            print("\nINDENTIFIED:\n#{indentified}")
        tree = lingo\match indentified
        if @debug
            print("\nPARSE TREE:")
            self\print_tree(tree)
        assert tree, "Failed to parse: #{str}"
        return tree
    
    transform: (tree, indent_level=0, parent=nil)=>
        indented = (fn)->
            export indent_level
            indent_level += 1
            fn!
            indent_level -= 1
        transform = (t,parent)-> self\transform(t, indent_level, parent or tree)
        ind = (line) -> ("  ")\rep(indent_level)..line
        ded = (lines)->
            if not lines.match then error("WTF: #{utils.repr(lines)}")
            lines\match"^%s*(.*)"

        ret_lines = {}
        lua = (line, skip_indent=false)->
            unless skip_indent
                line = ind(ded(line))
            table.insert ret_lines, line
            return line
        
        comma_separated_items = (open, items, close)->
            buffer = open
            so_far = indent_level*2
            indented ->
                export buffer,so_far
                for i,item in ipairs(items)
                    if i < #items then item ..= ", "
                    if so_far + #item >= 80 and #buffer > 0
                        lua buffer
                        so_far -= #buffer
                        buffer = item
                    else
                        so_far += #item
                        buffer ..= item
                buffer ..= close
                lua buffer

        switch tree.type
            when "File"
                if tree.value.errors and #tree.value.errors.value > 0
                    ret = transform(tree.value.errors)
                    return ret

                lua "return (function(game, vars)"
                indented ->
                    lua "local ret"
                    lua transform(tree.value.body)
                    lua "return ret"
                lua "end)"

            when "Errors"
                -- TODO: Better error reporting via tree.src
                error("\nParse error on: #{tree.value}")

            when "Block"
                for chunk in *tree.value
                    lua transform(chunk)
        
            when "Thunk"
                if not tree.value
                    error("Thunk without value: #{utils.repr(tree)}")
                lua "(function(game,vars)"
                indented ->
                    lua "local ret"
                    assert tree.value.type == "Block", "Non-block value in Thunk"
                    lua transform(tree.value)
                    lua "return ret"
                lua "end)"

            when "Statement"
                ret = transform(tree.value)
                return ret

            when "Expression"
                ret = transform(tree.value)
                if parent.type == "Statement"
                    ret = "ret = "..ded(ret)
                return ret

            when "FunctionCall"
                name_bits = {}
                for token in *tree.value
                    table.insert name_bits, if token.type == "Word" then token.value else "%"
                name = table.concat(name_bits, " ")
                if @macros[name]
                    {fn, arg_names} = @macros[name]
                    helpers = {:indented, :transform, :ind, :ded, :lua, :comma_separated_items}
                    args = [a for a in *tree.value when a.type != "Word"]
                    args = {name,args[i] for i,name in ipairs(arg_names)}
                    helpers.var = (varname)->
                        ded(transform(args[varname]))
                    m = fn(self, args, helpers, parent.type)
                    if m != nil then return m
                else
                    if parent.type == "Statement"
                        lua "ret ="
                    args = [ded(transform(a)) for a in *tree.value when a.type != "Word"]
                    table.insert args, 1, utils.repr(name, true)
                    comma_separated_items("game:call(", args, ")")

            when "String"
                lua utils.repr(tree.value, true)

            when "Number"
                lua tree.value

            when "List"
                if #tree.value == 0
                    lua "{}"
                elseif #tree.value == 1
                    lua "{#{transform(tree.value)}}"
                else
                    comma_separated_items("{", [ded(transform(item)) for item in *tree.value], "}")

            when "Var"
                lua "vars[#{utils.repr(tree.value,true)}]"

            else
                error("Unknown/unimplemented thingy: #{tree.type}")
        
        ret = table.concat ret_lines, "\n"
        return ret

    _yield_tree: (tree, indent_level=0)=>
        ind = (s) -> ("  ")\rep(indent_level)..s
        switch tree.type
            when "File"
                coroutine.yield(ind"File:")
                self\_yield_tree(tree.value.body, indent_level+1)

            when "Errors"
                coroutine.yield(ind"Error:\n#{tree.value}")

            when "Block"
                for chunk in *tree.value
                    self\_yield_tree(chunk, indent_level)
        
            when "Thunk"
                coroutine.yield(ind"Thunk:")
                self\_yield_tree(tree.value, indent_level+1)

            when "Statement"
                self\_yield_tree(tree.value, indent_level)

            when "Expression"
                self\_yield_tree(tree.value, indent_level)

            when "FunctionCall"
                name_bits = {}
                for token in *tree.value
                    table.insert name_bits, if token.type == "Word" then token.value else "%"
                name = table.concat(name_bits, " ")
                if #[a for a in *tree.value when a.type != "Word"] == 0
                    coroutine.yield(ind"Call [#{name}]!")
                else
                    coroutine.yield(ind"Call [#{name}]:")
                    for a in *tree.value
                        if a.type != "Word"
                            self\_yield_tree(a, indent_level+1)

            when "String"
                coroutine.yield(ind(utils.repr(tree.value, true)))

            when "Number"
                coroutine.yield(ind(tree.value))

            when "List"
                if #tree.value == 0
                    coroutine.yield(ind("<Empty List>"))
                else
                    coroutine.yield(ind"List:")
                    for item in *tree.value
                        self\_yield_tree(item, indent_level+1)

            when "Var"
                coroutine.yield ind"Var[#{utils.repr(tree.value)}]"

            else
                error("Unknown/unimplemented thingy: #{tree.type}")
        return nil -- to prevent tail calls

    print_tree:(tree)=>
        for line in coroutine.wrap(-> self\_yield_tree(tree))
            print(line)

    stringify_tree:(tree)=>
        result = {}
        for line in coroutine.wrap(-> self\_yield_tree(tree))
            table.insert(result, line)
        return table.concat result, "\n"

    compile: (src)=>
        tree = self\parse(src)
        code = self\transform(tree,0)
        return code

    test: (src, expected)=>
        if expected == nil
            start,stop = src\find"==="
            if not start or not stop then
                error("WHERE'S THE ===? in:\n#{src}")
            src, expected = src\sub(1,start-1), src\sub(stop+1,-1)
        expected = expected\match'[\n]*(.*[^\n])'
        if not expected then error("WTF???")
        tree = self\parse(src)
        got = if tree.value.errors and #tree.value.errors.value > 0
            self\stringify_tree(tree.value.errors)
        else
            self\stringify_tree(tree.value.body)
        if got != expected
            error"TEST FAILED!\nSource:\n#{src}\nExpected:\n#{expected}\n\nGot:\n#{got}"


return Game
