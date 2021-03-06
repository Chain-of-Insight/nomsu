{:NomsuCode} = require "code_obj"
{:find, :sub, :match} = string
{:R,:P,:S} = require 'lpeg'
re = require 're'
pretty_error = require("pretty_errors")

MAX_LINE = 80
GOLDEN_RATIO = ((math.sqrt(5)-1)/2)

-- Parsing helper functions
utf8_char_patt = (
    R("\194\223")*R("\128\191") +
    R("\224\239")*R("\128\191")*R("\128\191") +
    R("\240\244")*R("\128\191")*R("\128\191")*R("\128\191"))
operator_char = S("#'`~@^&*+=<>?/%!|\\-") + (P("\xE2") * (R("\x88\x8B")+R("\xA8\xAB")) * R("\128\191"))
operator_patt = operator_char^1 * -1
identifier_patt = (R("az","AZ","09") + P("_") + (-operator_char*utf8_char_patt))^1 * -1

is_operator = (s)->
    return type(s) == 'string' and not not operator_patt\match(s)

is_identifier = (s)->
    return type(s) == 'string' and not not identifier_patt\match(s)

can_be_unary = (t)->
    t.type == "Action" and #t == 2 and is_operator(t[1]) and type(t[2]) != 'string' and t[2].type != "Block" and not (t[2].type == "Number" and t[1] == "-")

inline_escaper = re.compile("{~ (%utf8_char / ('\"' -> '\\\"') / ('\n' -> '\\n') / ('\t' -> '\\t') / ('\b' -> '\\b') / ('\a' -> '\\a') / ('\v' -> '\\v') / ('\f' -> '\\f') / ('\r' -> '\\r') / ('\\' -> '\\\\') / ([^ -~] -> escape) / .)* ~}", {utf8_char: utf8_char_patt, escape:(=> ("\\%03d")\format(@byte!))})
inline_escape = (s)->
    return inline_escaper\match(s)

escaper = re.compile("{~ (%utf8_char / ('\\' -> '\\\\') / [\n\r\t -~] / (. -> escape))* ~}",
    {utf8_char: utf8_char_patt, escape:(=> ("\\%03d")\format(@byte!))})
escape = (s)->
    return escaper\match(s)

tree_to_inline_nomsu = (tree)->
    switch tree.type
        when "Action"
            nomsu = NomsuCode\from(tree.source)
            if can_be_unary(tree)
                nomsu\add tree[1]
                arg_nomsu = tree_to_inline_nomsu(tree[2])
                if tree[2].type == "MethodCall" or tree[2].type == "Action"
                    arg_nomsu\parenthesize!
                nomsu\add arg_nomsu
                return nomsu

            num_args, num_words = 0, 0
            for i,bit in ipairs tree
                if type(bit) == "string"
                    num_words += 1
                    clump_words = if type(tree[i-1]) == 'string'
                        is_operator(bit) != is_operator(tree[i-1])
                    else bit == "'"
                    nomsu\add " " if i > 1 and not clump_words
                    nomsu\add bit
                else
                    num_args += 1
                    arg_nomsu = tree_to_inline_nomsu(bit)
                    if tree[i+1] == "'" and bit.type == "Action" and can_be_unary(bit)
                        arg_nomsu\parenthesize!
                    if bit.type == "Block"
                        if i != #tree
                            nomsu\add " " if i > 1
                            arg_nomsu\parenthesize!
                    else
                        nomsu\add " " if i > 1
                        if bit.type == "MethodCall"
                            arg_nomsu\parenthesize!
                        elseif bit.type == "Action" and not can_be_unary(bit)
                            arg_nomsu\parenthesize!
                    nomsu\add arg_nomsu
            if num_args == 1 and num_words == 0
                nomsu\add "()"
            return nomsu

        when "MethodCall"
            target_nomsu = tree_to_inline_nomsu(tree[1])
            if tree[1].type == "Block"
                target_nomsu\parenthesize!
            nomsu = NomsuCode\from(tree.source, target_nomsu, ", ")
            if #tree > 2 then nomsu\add "("
            for i=2,#tree
                nomsu\add "; " if i > 2
                nomsu\add tree_to_inline_nomsu(tree[i])
            if #tree > 2 then nomsu\add ")"
            return nomsu

        when "EscapedNomsu"
            inner_nomsu = tree_to_inline_nomsu(tree[1])
            unless tree[1].type == "List" or tree[1].type == "Dict" or tree[1].type == "Var"
                inner_nomsu\parenthesize!
            return NomsuCode\from(tree.source, "\\", inner_nomsu)

        when "Block"
            nomsu = NomsuCode\from(tree.source, ":")
            for i,line in ipairs tree
                nomsu\add(i == 1 and " " or "; ")
                nomsu\add tree_to_inline_nomsu(line)
            nomsu\parenthesize! if #tree > 1
            return nomsu

        when "Text"
            add_text = (nomsu, tree)->
                for i, bit in ipairs tree
                    if type(bit) == 'string'
                        escaped = inline_escape(bit)
                        nomsu\add inline_escape(bit)
                    elseif bit.type == "Text"
                        add_text(nomsu, bit)
                    else
                        interp_nomsu = tree_to_inline_nomsu(bit)
                        if bit.type != "Var" and bit.type != "List" and bit.type != "Dict"
                            interp_nomsu\parenthesize!
                        elseif bit.type == "Var" and type(bit[1]) == 'string' and
                        type(tree[i+1]) == 'string' and not match(tree[i+1], "^[ \n\t,.:;#(){}[%]]")
                            interp_nomsu\parenthesize!
                        nomsu\add "\\", interp_nomsu
            nomsu = NomsuCode\from(tree.source)
            add_text(nomsu, tree)
            return NomsuCode\from(tree.source, '"', nomsu, '"')

        when "List", "Dict"
            nomsu = NomsuCode\from(tree.source, (tree.type == "List" and "[" or "{"))
            for i, item in ipairs tree
                nomsu\add ", " if i > 1
                item_nomsu = tree_to_inline_nomsu(item, true)
                --if item.type == "Block" or item.type == "Action" or item.type == "MethodCall"
                --    item_nomsu\parenthesize!
                if item.type == "MethodCall" or (item.type == "Block" and i < #tree)
                    item_nomsu\parenthesize!
                nomsu\add item_nomsu
            nomsu\add(tree.type == "List" and "]" or "}")
            return nomsu
        
        when "DictEntry"
            key, value = tree[1], tree[2]
            nomsu = NomsuCode\from(tree.source)
            -- TODO: remove shim
            if key.type != "Index"
                key = {type:"Index", source:key.source, key}
            nomsu\add tree_to_inline_nomsu(key)
            if value
                nomsu\add " = "
                value_nomsu = tree_to_inline_nomsu(value)
                value_nomsu\parenthesize! if value.type == "Block" or value.type == "Action" or value.type == "MethodCall"
                nomsu\add value_nomsu
            return nomsu
        
        when "Index"
            key = tree[1]
            nomsu = NomsuCode\from(key.source, ".")
            key_nomsu = if key.type == "Text" and #key == 1 and is_identifier(key[1])
                key[1]
            else
                tree_to_inline_nomsu(key)
            switch key.type
                when "Block", "Action", "MethodCall", "IndexChain"
                    key_nomsu\parenthesize!
            return NomsuCode\from(key.source, ".", key_nomsu)
        
        when "IndexChain"
            nomsu = NomsuCode\from(tree.source)
            target = tree[1]
            target_nomsu = tree_to_inline_nomsu(target)
            switch target.type
                when "Action", "MethodCall", "EscapedNomsu"
                    target_nomsu\parenthesize!
                when "Number"
                    target_nomsu\parenthesize! if target_nomsu\text!\match("%.")
            nomsu\add target_nomsu
            for i=2,#tree
                -- TODO: remove shim?
                if tree[i].type != "Index"
                    tree[i] = {type:"Index", source:tree[i].source, tree[i]}
                nomsu\add tree_to_inline_nomsu(tree[i])
            return nomsu
        
        when "Number"
            -- Preserve original formatting, just make sure 0xdead_beef -> 0xDEAD_BEEF
            n = tostring(tree[1])
            s = if n\match("^-*0x")
                n\upper!\gsub("0X", "0x")
            elseif tree.hex and tonumber((n\gsub("_",""))) < 0
                ("-0x%X")\format(-tree[1])
            elseif tree.hex
                ("0x%X")\format(tree[1])
            else n
            return NomsuCode\from(tree.source, s)

        when "Var"
            varname = tree[1]
            if type(varname) == "string"
                return NomsuCode\from(tree.source, "$", varname)
            else
                return NomsuCode\from(tree.source, "$(", tree_to_inline_nomsu(varname), ")")

        when "FileChunks"
            error("Can't inline a FileChunks")
        
        when "Comment"
            -- TODO: implement?
            return NomsuCode\from(tree.source)
        
        when "Error"
            err_msg = pretty_error{
                title:"Parse error"
                error:tree.error, hint:tree.hint, source:tree\get_source_file!
                start:tree.source.start, stop:tree.source.stop, filename:tree.source.filename
            }
            -- Coroutine yield here?
            error(err_msg)
        
        else
            error("Unknown type: #{tree.type}")

tree_to_nomsu = (tree)->
    nomsu = NomsuCode\from(tree.source)

    -- For concision:
    recurse = (t, argnum=nil)->
        space = MAX_LINE - nomsu\trailing_line_len!
        try_inline = true
        for subtree in coroutine.wrap(-> (t\with(coroutine.yield) and nil))
            switch subtree.type
                when "Comment"
                    try_inline = false
                when "Block"
                    if #subtree > 1
                        try_inline = false
                when "Text"
                    indented = tree_to_nomsu(subtree)
                    indented_lines = [line for line in *indented\text!\lines! when line\match("^    +([^ ].*)")]
                    for i=#indented_lines,1,-1
                        if indented_lines[i]\match("^ *\\;$")
                            table.remove(indented_lines, i)
                    if #indented_lines > 1 or (#indented_lines == 1 and #indented_lines[1] > MAX_LINE + 8)
                        try_inline = false
        
        local inline_nomsu
        if try_inline
            inline_nomsu = tree_to_inline_nomsu(t)
            if t.type == "MethodCall"
                inline_nomsu\parenthesize!
            elseif t.type == "Action" and not can_be_unary(t)
                inline_nomsu\parenthesize!
            if #inline_nomsu\text! <= space or #inline_nomsu\text! <= 8
                if t.type != "Text"
                    return inline_nomsu
        indented = tree_to_nomsu(t)
        if t.type == "Action" or t.type == "MethodCall"
            if indented\is_multiline!
                if argnum == nil or argnum == 1
                    return NomsuCode\from(t.source, "(\n    ", indented, "\n)")
                else
                    return NomsuCode\from(t.source, "\n    ", indented)
            elseif argnum and argnum > 1
                return NomsuCode\from(t.source, "\n    ", indented)
            else
                indented\parenthesize!
        indented_lines = [line for line in *indented\text!\lines! when line\match("^    +([^ ].*)")]
        if t.type == "Text"
            for i=#indented_lines,1,-1
                if indented_lines[i]\match("^ *\\;$")
                    table.remove(indented_lines, i)
        if inline_nomsu and (#inline_nomsu\text! < MAX_LINE or #inline_nomsu\text! <= space) and
            #indented_lines <= 1
            return inline_nomsu
        return indented

    switch tree.type
        when "FileChunks"
            if tree.shebang
                nomsu\add tree.shebang, "\n"

            for chunk_no, chunk in ipairs tree
                nomsu\add "\n\n#{("~")\rep(80)}\n\n" if chunk_no > 1
                if chunk.type == "Block"
                    nomsu\add NomsuCode\from(chunk.source, table.unpack(tree_to_nomsu(chunk).bits, 2))
                else
                    nomsu\add tree_to_nomsu(chunk)

            return nomsu

        when "Action"
            if can_be_unary(tree) and not can_be_unary(tree[2])
                nomsu\add tree[1]
                nomsu\add recurse(tree[2])
                return nomsu

            next_space = ""
            word_buffer = {}
            num_args, num_words = 0, 0
            for i,bit in ipairs tree
                -- TODO: properly wrap super long chains of words
                if type(bit) == "string"
                    num_words += 1
                    if #word_buffer > 0 and is_operator(bit) == is_operator(word_buffer[#word_buffer])
                        table.insert word_buffer, " "
                    table.insert word_buffer, bit
                    continue

                if #word_buffer > 0
                    words = table.concat(word_buffer)
                    if next_space == " "
                        if nomsu\trailing_line_len! + #words > MAX_LINE and nomsu\trailing_line_len! > 8
                            next_space = "\n.."
                        elseif word_buffer[1] == "'"
                            next_space = ""
                    nomsu\add next_space, words
                    word_buffer = {}
                    next_space = " "

                num_args += 1
                bit_nomsu = recurse(bit, i)
                if tree[i+1] == "'" and bit.type == "Action" and not bit_nomsu\is_multiline! and can_be_unary(bit)
                    bit_nomsu\parenthesize!
                if bit.type == "Block"
                    -- Rule of thumb: nontrivial one-liner block arguments should be no more
                    -- than golden ratio * the length of the proceeding part of the line
                    if not bit_nomsu\is_multiline! and
                        (#bit_nomsu\text! > nomsu\trailing_line_len! * GOLDEN_RATIO and #bit_nomsu\text! > 8) or
                        #bit_nomsu\text! + nomsu\trailing_line_len! > MAX_LINE
                        bit_nomsu = tree_to_nomsu(bit)
                elseif (not bit_nomsu\is_multiline! and
                    nomsu\trailing_line_len! + #bit_nomsu\text! > MAX_LINE and
                    nomsu\trailing_line_len! > 8)
                    if next_space == " " and #bit_nomsu\text! < MAX_LINE
                        if i == #tree
                            bit_nomsu = tree_to_inline_nomsu(bit)
                            next_space = "\n    "
                        elseif bit.type == "List" or bit.type == "Dict"
                            bit_nomsu = tree_to_nomsu(bit)
                        else
                            next_space = "\n.."
                    elseif bit.type == 'Action' or bit.type == "MethodCall"
                        bit_nomsu = NomsuCode\from bit.source, "\n    ", tree_to_nomsu(bit)
                    else
                        bit_nomsu = tree_to_nomsu(bit)

                unless next_space == " " and bit_nomsu\text!\match("^[:\n]")
                    nomsu\add next_space

                nomsu\add bit_nomsu
                next_space = (bit.type == "Block" or bit_nomsu\text!\matches("\n    [^\n]*$")) and "\n.." or " "

            if #word_buffer > 0
                words = table.concat(word_buffer)
                if next_space == " "
                    if nomsu\trailing_line_len! + #words > MAX_LINE + 8 and nomsu\trailing_line_len! > 8
                        next_space = "\n.."
                    elseif word_buffer[1] == "'"
                        next_space = ""
                nomsu\add next_space, words
                next_space = " "

            if num_args == 1 and num_words == 0
                if next_space != " "
                    nomsu\add next_space
                nomsu\add "()"
            return nomsu

        when "MethodCall"
            target_nomsu = recurse(tree[1])
            if tree[1].type == "Block" and not target_nomsu\is_multiline!
                target_nomsu\parenthesize!
            nomsu\add target_nomsu, ", "
            inner_nomsu = NomsuCode!
            for i=2,#tree
                inner_nomsu\add "\n" if i > 2
                inner_nomsu\add tree_to_nomsu(tree[i])
            if #tree == 2 and nomsu\trailing_line_len! + #inner_nomsu\text!\match("^[^\n]*") < MAX_LINE
                nomsu\add inner_nomsu
            else
                nomsu\add "\n    ", inner_nomsu
            return nomsu

        when "EscapedNomsu"
            nomsu = recurse(tree[1])
            if tree[1].type == 'Block' and not nomsu\is_multiline!
                nomsu\parenthesize!
            return NomsuCode\from tree.source, "\\", nomsu

        when "Block"
            prev_line, needs_space = nil, {}
            for i, line in ipairs tree
                line_nomsu = tree_to_nomsu(line)
                if i > 1
                    nomsu\add "\n"
                    -- Rule of thumb: add a blank line between two lines if both are
                    -- multi-line non-comments, or if a comment comes after a non-comment,
                    -- or if the last line starts with ".."
                    if tree[i-1].type != "Comment"
                        needs_space[i] = (line_nomsu\is_multiline! and prev_line\is_multiline!)
                        if (tree[i].type == "Comment" or needs_space[i] or needs_space[i-1] or
                            prev_line\text!\match("\n        [^\n]*$"))
                            nomsu\add "\n"
                nomsu\add line_nomsu
                prev_line = line_nomsu
            return NomsuCode\from(tree.source, ":\n    ", nomsu)

        when "Text"
            -- Multi-line text has more generous wrap margins
            max_line = MAX_LINE + 8
            add_text = (tree)->
                for i, bit in ipairs tree
                    if type(bit) == 'string'
                        bit = escape(bit)
                        for j, line in ipairs bit\lines!
                            if j > 1
                                if nomsu\text!\match(" $")
                                    nomsu\add "\\;"
                                nomsu\add "\n"
                            elseif #line > 10 and nomsu\trailing_line_len! > max_line
                                nomsu\add "\\\n.."

                            while #line > 0
                                space = max_line - nomsu\trailing_line_len!
                                split = find(line, "[%p%s]", space)
                                if not split or split > space + 16
                                    split = space + 16
                                if #line - split < 16
                                    split = #line
                                bite, line = sub(line, 1, split), sub(line, split+1, -1)
                                nomsu\add bite
                                nomsu\add "\\\n.." if #line > 0
                    elseif bit.type == "Text"
                        add_text(bit)
                    else
                        nomsu\add "\\"
                        interp_nomsu = recurse(bit)
                        if interp_nomsu\is_multiline!
                            curr_indent = nomsu\text!\match("\n( *)[^\n]*$") or nomsu\text!\match("^( *)")
                            interp_nomsu = NomsuCode((interp_nomsu\text!\gsub("\n", "\n"..curr_indent)))
                        else
                            space = max_line - nomsu\trailing_line_len!
                            if bit.type == "Var"
                                next_str = tree[i+1]
                                while type(next_str) == 'table' and next_str.type == 'Text'
                                    next_str = next_str[1]
                                if type(next_str) == 'string' and not match(next_str, "^[ \n\t,.:;#(){}[%]]")
                                    interp_nomsu\parenthesize!
                            elseif #interp_nomsu\text! > space
                                interp_nomsu2 = if bit.type == "Action" or bit.type == "MethodCall"
                                    NomsuCode\from(bit.source, "(\n    ", tree_to_nomsu(bit), "\n)")
                                else
                                    tree_to_nomsu(bit)

                                if #interp_nomsu2\text!\lines! > 3 or #interp_nomsu2\text! >= MAX_LINE*GOLDEN_RATIO
                                    curr_indent = nomsu\text!\match("\n( *)[^\n]*$") or nomsu\text!\match("^( *)")
                                    interp_nomsu2 = NomsuCode((interp_nomsu2\text!\gsub("\n", "\n"..curr_indent)))
                                    interp_nomsu = interp_nomsu2
                                else
                                    nomsu\add "\n..\\"
                                    if bit.type == "EscapedNomsu" or bit.type == "Block" or bit.type == "IndexChain"
                                        interp_nomsu\parenthesize!
                            elseif bit.type == "EscapedNomsu" or bit.type == "Block" or bit.type == "IndexChain"
                                interp_nomsu\parenthesize!
                            elseif bit.type == "Action" and can_be_unary(bit)
                                interp_nomsu\parenthesize!
                        nomsu\add interp_nomsu
                        if interp_nomsu\is_multiline! and bit.type == "Block"
                            nomsu\add "\n.."
            add_text(tree)
            if nomsu\text!\match(" $")
                nomsu\add "\\;"
            return NomsuCode\from(tree.source, '("\n    ', nomsu, '\n")')

        when "List", "Dict"
            if #tree == 0
                nomsu\add(tree.type == "List" and "[]" or "{}")
                return nomsu

            if #tree == 1 and tree[1].type == "Block"
                block_lua = recurse(tree[1])
                if block_lua\is_multiline! then block_lua\add "\n"
                return if tree.type == "List" then
                    NomsuCode\from(tree.source, "[", block_lua, "]")
                else
                    NomsuCode\from(tree.source, "{", block_lua, "}")

            sep = ''
            prev_item, needs_space = nil, {}
            for i, item in ipairs tree
                local item_nomsu
                if item.type == 'MethodCall'
                    item_nomsu = recurse(item)
                elseif item.type == 'Comment'
                    item_nomsu = tree_to_nomsu(item)
                    sep = '\n' if i > 1
                elseif item.type == 'Block' and #item == 1
                    -- Comprehensions use the more concise ": for $ in $: ..." form
                    item_nomsu = tree_to_nomsu(item[1])
                    item_nomsu\prepend ": "
                    sep = '\n' if i > 1
                else
                    item_nomsu = tree_to_inline_nomsu(item)
                    if nomsu\trailing_line_len! + #item_nomsu\text! > MAX_LINE
                        sep = '\n' if i > 1
                        item_nomsu = item.type == "Action" and tree_to_nomsu(item) or recurse(item)
                nomsu\add sep
                if sep == '\n'
                    -- Rule of thumb: add a blank line between two lines if both are
                    -- multi-line non-comments, or if a comment comes after a non-comment,
                    -- or if the last line starts with ".."
                    if i > 1 and tree[i-1].type != "Comment"
                        needs_space[i] = (item_nomsu\is_multiline! and prev_item\is_multiline!)
                        if (tree[i].type == "Comment" or needs_space[i] or needs_space[i-1] or
                            prev_item\text!\match("\n        [^\n]*$"))
                            nomsu\add "\n"
                nomsu\add item_nomsu
                prev_item = item_nomsu
                if item_nomsu\is_multiline! or item.type == 'Comment' or item.type == "Block" or
                    nomsu\trailing_line_len! + #tostring(item_nomsu) >= MAX_LINE
                    sep = '\n'
                else
                    sep = ', '
            return if tree.type == "List" then
                NomsuCode\from(tree.source, "[\n    ", nomsu, "\n]")
            else
                NomsuCode\from(tree.source, "{\n    ", nomsu, "\n}")

        when "DictEntry"
            key, value = tree[1], tree[2]
            nomsu = NomsuCode\from(tree.source)
            -- TODO: remove shim
            if key.type != "Index"
                key = {type:"Index", source:key.source, key}
            nomsu\add tree_to_nomsu(key)
            if value
                value_nomsu = recurse(value)
                nomsu\add " = ", value_nomsu
            return nomsu

        when "Comment"
            nomsu\add "###", (tree[1]\gsub("\n", "\n    "))
            return nomsu
        
        when "IndexChain", "Index", "Number", "Var", "Comment", "Error"
            return tree_to_inline_nomsu tree
        
        else
            error("Unknown type: #{tree.type}")

return {:tree_to_nomsu, :tree_to_inline_nomsu}
