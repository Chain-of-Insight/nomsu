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
                table.insert result, "{\n " --(" ")\rep(indent_stack[#indent_stack]+1).."{\n "
                table.insert indent_stack, indent
            elseif indent < indent_stack[#indent_stack]
                dedents = 0
                tokens = {}
                while indent < indent_stack[#indent_stack]
                    table.remove indent_stack
                    table.insert tokens, "}" --(" ")\rep(indent_stack[#indent_stack]+1).."}"
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
    indentflagger\match(str)
    while #indent_stack > 1
        table.remove indent_stack
        table.insert result, "}\n"
    return table.concat result

lingo = [=[
    file <- ({} {| {:body: (" " block) :} ({:errors: errors :})? |} {}) -> File
    errors <- ({} {.+} {}) -> Errors
    block <- ({} {| statement (%nodent statement)* |} {}) -> Block
    one_liner <- ({} {| statement |} {}) -> Block
    statement <- ({} functioncall {}) -> Statement

    functioncall <- ({} {| fn_bits |} {}) -> FunctionCall
    fn_bit <- (expression / word)
    fn_bits <-
        ((".." (%indent %nodent indented_fn_bits %dedent))
         / fn_bit) (fn_sep fn_bits)?
    indented_fn_bits <-
        fn_bit ((%ws / %nodent) indented_fn_bits)?
    
    fn_sep <- (%nodent ".." %ws?) / %ws / (&":") / (&"..") / (&'"') / (&"[")

    thunk <-
        ({} ":" %ws? ((one_liner (%ws? ";")?) / (%indent %nodent block %dedent)) {}) -> Thunk

    word <- ({} {%wordchar+} {}) -> Word
    expression <- string / number / variable / list / thunk / subexpression

    string <- ({} '"' {(("\\" .) / [^"])*} '"' {}) -> String
    number <- ({} {'-'? [0-9]+ ("." [0-9]+)?} {}) -> Number
    variable <- ({} ("%" {%wordchar+}) {}) -> Var

    subexpression <- "(" %ws? (expression / functioncall) %ws? ")"

    list <- ({} {|
        ("[..]" %indent %nodent indented_list ","? %dedent)
        / ("[" %ws? (list_items ","?)?  %ws?"]")
      |} {}) -> List
    list_items <- (expression (list_sep list_items)?)
    list_sep <- %ws? "," %ws?
    indented_list <-
        expression (((list_sep %nodent?) / %nodent) indented_list)?
]=]

defs =
    eol: #(linebreak) + (lpeg.P("")-lpeg.P(1))
    ws: lpeg.S(" \t")^1
    wordchar: lpeg.P(1)-lpeg.S(' \t\n\r%:;,.{}[]()"')
    indent: linebreak * lpeg.P("{") * lpeg.S(" \t")^0
    nodent: linebreak * lpeg.P(" ") * lpeg.S(" \t")^0
    dedent: linebreak * lpeg.P("}") * lpeg.S(" \t")^0

setmetatable(defs, {
    __index: (t,key)->
        --print("WORKING for #{key}")
        fn = (start, value, stop, ...)->
            token = {type: key, range:{start,stop}, value: value}
            return token
        t[key] = fn
        return fn
})
lingo = re.compile lingo, defs


class Game
    new:=>
        @defs = {}
        @macros = {}
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
                print("arg #{utils.repr(name,true)} = #{select(i,...)}")
        ret = fn(self, args)
        if @debug
            print "returned #{utils.repr(ret,true)}"
        return ret

    def: (spec, fn)=>
        invocation,arg_names = self\get_invocation spec
        @defs[invocation] = {fn, arg_names}

    get_invocation:(text)=>
        name_bits = {}
        arg_names = {}
        for chunk in text\gmatch("%S+")
            if chunk\sub(1,1) == "%"
                table.insert name_bits, "%"
                table.insert arg_names, chunk\sub(2,-1)
            else
                table.insert name_bits, chunk
        invocation = table.concat name_bits, " "
        return invocation, arg_names

    defmacro: (spec, fn, advanced_mode=false)=>
        invocation,arg_names = self\get_invocation spec
        if advanced_mode
            @macros[invocation] = {fn, arg_names}
            return

        text_manipulator = fn
        fn = (args, transform,src,indent_level,macros)->
            text_args = [transform(src,a,indent_level,macros) for a in *args]
            return text_manipulator(unpack(text_args))
        @macros[invocation] = {fn, arg_names}
    
    run: (text)=>
        if @debug
            print("Running text:\n")
            print(text)
            indentified = add_indent_tokens(text)
            print("Indentified:\n[[#{indentified}]]")
            print("\nCompiling...")
        code = compile(text, @macros)
        if @debug
            print(code)
        lua_thunk, err = loadstring(code)
        if not lua_thunk
            error("Failed to compile")
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
            print("\nRESULT:\n#{utils.repr(tree)}")
        assert tree, "Failed to parse: #{str}"
        return tree
    
    transform: (tree, indent_level=0)=>
        indented = (fn)->
            export indent_level
            indent_level += 1
            fn!
            indent_level -= 1
        transform = (t)-> self\transform(t, indent_level)
        ind = (line) -> ("  ")\rep(indent_level)..line
        ded = (lines)-> lines\match"^%s*(.*)"

        ret_lines = {}
        lua = (line, skip_indent=false)->
            unless skip_indent
                line = ind(ded(line))
            table.insert ret_lines, line

        switch tree.type
            when "File"
                if tree.value.errors and #tree.value.errors.value > 1
                    return transform(tree.value.errors)

                lua "return (function(game, vars)"
                indented->
                    lua transform(tree.value.body)
                lua "end)"

            when "Errors"
                -- TODO: Better error reporting via tree.range[1]
                error("\nParse error on: #{tree.value}")

            when "Block"
                for chunk in *tree.value
                    lua transform(chunk)
        
            when "Thunk"
                if not tree.value
                    error("Thunk without value: #{utils.repr(tree)}")
                lua "(function(game,vars)"
                indented->
                    lua "local ret"
                    lua transform(tree.value)
                    lua "return ret"
                lua "end)"
                return table.concat ret,"\n"

            when "Statement"
                return ind"ret = #{transform(tree.value,indent_level)}"

            when "FunctionCall"
                name_bits = {}
                for token in *tree.value
                    table.insert name_bits, if token.type == "Word" then token.value else "%"
                name = table.concat(name_bits, " ")
                args = [a for a in *tree.value when a.type != "Word"]

                if @macros[name]
                    -- TODO: figure out args
                    return @macros[name][1](args, transform,indent_level,@macros)

                if #args == 0
                    return ind"game:call(#{utils.repr(name, true)})"
                ret = {
                    ind"game:call(#{utils.repr(name, true)},"
                }
                for i,a in ipairs(args)
                    if a.type != "Word"
                        line = transform(a,indent_level+1)
                        if i != #args then line ..=","
                        table.insert ret, line
                table.insert ret, ind")"
                return table.concat ret, "\n"

            when "String"
                return ind"\"#{tree.value}\""

            when "Number"
                return ind(tree.value)

            when "List"
                if #tree.value == 0
                    return "{}"
                elseif #tree.value == 1
                    return ind"{#{transform(tree.value,0)}}"
                else
                    bits = [transform(i, indent_level+1) for i in *tree.value]
                    -- I like the trailing comma
                    return ind"{\n"..table.concat(bits, ",\n")..",\n"..ind"}"

            when "Var"
                return ind"vars[#{utils.repr(tree.value,true)}]"

            else
                error("Unknown/unimplemented thingy: #{utils.repr(tree)}")
        
        return table.concat ret_lines, "\n"

    compile: (src)=>
        tree = self\parse(src)
        code = self\transform(tree,0)
        return code



return Game
