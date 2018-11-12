{:NomsuCode} = require "code_obj"
{:find, :sub, :match} = string
{:R,:P,:S} = require 'lpeg'
re = require 're'

MAX_LINE = 80
GOLDEN_RATIO = ((math.sqrt(5)-1)/2)

-- Parsing helper functions
utf8_char_patt = (
    R("\194\223")*R("\128\191") +
    R("\224\239")*R("\128\191")*R("\128\191") +
    R("\240\244")*R("\128\191")*R("\128\191")*R("\128\191"))
operator_patt = S("'`~!@$^&*+=|<>?/-")^1 * -1
identifier_patt = (R("az","AZ","09") + P("_") + utf8_char_patt)^1 * -1

is_operator = (s)->
    return not not operator_patt\match(s)

is_identifier = (s)->
    return not not identifier_patt\match(s)

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
            if tree.target
                inline_target = tree_to_inline_nomsu(tree.target)
                if tree.target.type == "Action"
                    inline_target\parenthesize!
                nomsu\append inline_target, "::"

            for i,bit in ipairs tree
                if type(bit) == "string"
                    clump_words = if type(tree[i-1]) == 'string'
                        is_operator(bit) != is_operator(tree[i-1])
                    else bit == "'"
                    nomsu\append " " if i > 1 and not clump_words
                    nomsu\append bit
                else
                    arg_nomsu = tree_to_inline_nomsu(bit)
                    if bit.type == "Block"
                        if i > 1 and i < #tree
                            nomsu\append " "
                        unless i == #tree
                            arg_nomsu\parenthesize!
                    else
                        nomsu\append " " if i > 1
                        if bit.type == "Action"
                            arg_nomsu\parenthesize!
                    nomsu\append arg_nomsu
            return nomsu

        when "EscapedNomsu"
            inner_nomsu = tree_to_inline_nomsu(tree[1])
            unless tree[1].type == "List" or tree[1].type == "Dict" or tree[1].type == "Var"
                inner_nomsu\parenthesize!
            return NomsuCode\from(tree.source, "\\", inner_nomsu)

        when "Block"
            nomsu = NomsuCode\from(tree.source, ":")
            for i,line in ipairs tree
                nomsu\append(i == 1 and " " or "; ")
                nomsu\append tree_to_inline_nomsu(line)
            nomsu\parenthesize! if #tree > 1
            return nomsu

        when "Text"
            add_text = (nomsu, tree)->
                for i, bit in ipairs tree
                    if type(bit) == 'string'
                        escaped = inline_escape(bit)
                        nomsu\append inline_escape(bit)
                    elseif bit.type == "Text"
                        add_text(nomsu, bit)
                    else
                        interp_nomsu = tree_to_inline_nomsu(bit)
                        if bit.type != "Var" and bit.type != "List" and bit.type != "Dict"
                            interp_nomsu\parenthesize!
                        elseif bit.type == "Var" and type(tree[i+1]) == 'string' and not match(tree[i+1], "^[ \n\t,.:;#(){}[%]]")
                            interp_nomsu\parenthesize!
                        nomsu\append "\\", interp_nomsu
            nomsu = NomsuCode\from(tree.source)
            add_text(nomsu, tree)
            return NomsuCode\from(tree.source, '"', nomsu, '"')

        when "List", "Dict"
            nomsu = NomsuCode\from(tree.source, (tree.type == "List" and "[" or "{"))
            for i, item in ipairs tree
                nomsu\append ", " if i > 1
                nomsu\append tree_to_inline_nomsu(item)
            nomsu\append(tree.type == "List" and "]" or "}")
            return nomsu
        
        when "DictEntry"
            key, value = tree[1], tree[2]
            nomsu = if key.type == "Text" and #key == 1 and is_identifier(key[1])
                NomsuCode\from(key.source, key[1])
            else tree_to_inline_nomsu(key)
            nomsu\parenthesize! if key.type == "Action" or key.type == "Block"
            if value
                nomsu\append ": "
                value_nomsu = tree_to_inline_nomsu(value)
                value_nomsu\parenthesize! if value.type == "Block"
                nomsu\append value_nomsu
            return nomsu
        
        when "IndexChain"
            nomsu = NomsuCode\from(tree.source)
            for i, bit in ipairs tree
                nomsu\append "." if i > 1
                local bit_nomsu
                bit_nomsu = if i > 1 and bit.type == "Text" and #bit == 1 and type(bit[1]) == 'string' and is_identifier(bit[1])
                    bit[1]
                else tree_to_inline_nomsu(bit)
                assert bit.type != "Block"
                if bit.type == "Action" or bit.type == "IndexChain" or (bit.type == "Number" and i < #tree)
                    bit_nomsu\parenthesize!
                nomsu\append bit_nomsu
            return nomsu
        
        when "Number"
            s = if tree.hex and tree[1] < 0
                ("-0x%X")\format(-tree[1])
            elseif tree.hex
                ("0x%X")\format(tree[1])
            else tostring(tree[1])
            return NomsuCode\from(tree.source, s)

        when "Var"
            return NomsuCode\from(tree.source, "%", tree[1])

        when "FileChunks"
            error("Can't inline a FileChunks")
        
        when "Comment"
            -- TODO: implement?
            return NomsuCode\from(tree.source)
        
        when "Error"
            error("Can't compile errors")
        
        else
            error("Unknown type: #{tree.type}")

tree_to_nomsu = (tree)->
    nomsu = NomsuCode\from(tree.source)

    -- For concision:
    recurse = (t)->
        space = MAX_LINE - nomsu\trailing_line_len!
        try_inline = true
        for subtree in coroutine.wrap(-> (t\map(coroutine.yield) and nil))
            if subtree.type == "Block"
                if #subtree > 1
                    try_inline = false
        
        local inline_nomsu
        if try_inline
            inline_nomsu = tree_to_inline_nomsu(t)
            if #inline_nomsu\text! <= space or #inline_nomsu\text! <= 8
                if t.type == "Action"
                    inline_nomsu\parenthesize!
                return inline_nomsu
        indented = tree_to_nomsu(t)
        if t.type == "Action"
            if indented\is_multiline!
                return NomsuCode\from(t.source, "(..)\n    ", indented)
            else indented\parenthesize!
        if inline_nomsu and indented\text!\match("^[^\n]*\n[^\n]*$") and nomsu\trailing_line_len! <= 8
            return inline_nomsu
        return indented

    switch tree.type
        when "FileChunks"
            if tree.shebang
                nomsu\append tree.shebang

            for chunk_no, chunk in ipairs tree
                nomsu\append "\n\n#{("~")\rep(80)}\n\n" if chunk_no > 1
                if chunk.type == "Block"
                    nomsu\append NomsuCode\from(chunk.source, table.unpack(tree_to_nomsu(chunk).bits, 2))
                else
                    nomsu\append tree_to_nomsu(chunk)

            nomsu\append('\n') unless nomsu\match("\n$")
            return nomsu

        when "Action"
            next_space = ""
            if tree.target
                target_nomsu = recurse(tree.target)
                if tree.target.type == "Block" and not target_nomsu\is_multiline!
                    target_nomsu\parenthesize!
                nomsu\append target_nomsu
                nomsu\append(target_nomsu\is_multiline! and "\n..::" or "::")

            word_buffer = {}
            for i,bit in ipairs tree
                if type(bit) == "string"
                    if #word_buffer > 0 and is_operator(bit) == is_operator(word_buffer[#word_buffer])
                        table.insert word_buffer, " "
                    table.insert word_buffer, bit
                    continue

                if #word_buffer > 0
                    words = table.concat(word_buffer)
                    if next_space == " "
                        if nomsu\trailing_line_len! + #words > MAX_LINE and nomsu\trailing_line_len! > 8
                            next_space = " \\\n.."
                        elseif word_buffer[1] == "'"
                            next_space = ""
                    nomsu\append next_space, words
                    word_buffer = {}
                    next_space = " "

                bit_nomsu = recurse(bit)
                if bit.type == "Block" and not bit_nomsu\is_multiline!
                    -- Rule of thumb: nontrivial one-liner block arguments should be no more
                    -- than golden ratio * the length of the proceeding part of the line
                    if #bit_nomsu\text! > nomsu\trailing_line_len! * GOLDEN_RATIO and #bit_nomsu\text! > 8
                        bit_nomsu = tree_to_nomsu(bit)

                if (next_space == " " and not bit_nomsu\is_multiline! and
                    nomsu\trailing_line_len! + #bit_nomsu\text! > MAX_LINE and
                    nomsu\trailing_line_len! > 8)
                    if bit.type == 'Action'
                        bit_nomsu = NomsuCode\from bit.source, "(..)\n    ", tree_to_nomsu(bit)
                    else
                        next_space = " \\\n.."
                unless next_space == " " and bit.type == "Block"
                    nomsu\append next_space

                nomsu\append bit_nomsu
                next_space = (bit_nomsu\is_multiline! or bit.type == 'Block') and "\n.." or " "

            if #word_buffer > 0
                words = table.concat(word_buffer)
                if next_space == " "
                    if nomsu\trailing_line_len! + #words > MAX_LINE and nomsu\trailing_line_len! > 8
                        next_space = " \\\n.."
                    elseif word_buffer[1] == "'"
                        next_space = ""
                nomsu\append next_space, words

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
                    nomsu\append "\n"
                    -- Rule of thumb: add a blank line between two lines if both are
                    -- multi-line non-comments, or if a comment comes after a non-comment.
                    if tree[i-1].type != "Comment"
                        needs_space[i] = (line_nomsu\is_multiline! and prev_line\is_multiline!)
                        if tree[i].type == "Comment" or needs_space[i] or needs_space[i-1]
                            nomsu\append "\n"
                nomsu\append line_nomsu
                prev_line = line_nomsu
            return NomsuCode\from(tree.source, ":\n    ", nomsu)

        when "Text"
            -- Multi-line text has more generous wrap margins
            max_line = math.floor(1.25*MAX_LINE)
            add_text = (tree)->
                for i, bit in ipairs tree
                    if type(bit) == 'string'
                        bit = escape(bit)
                        for j, line in ipairs bit\lines!
                            if j > 1
                                nomsu\append "\n"
                            elseif #line > 10 and nomsu\trailing_line_len! > max_line
                                nomsu\append "\\\n.."

                            while #line > 0
                                space = max_line - nomsu\trailing_line_len!
                                split = find(line, "[%p%s]", space)
                                if not split or split > space + 10
                                    split = space + 10
                                if #line - split < 10
                                    split = #line
                                bite, line = sub(line, 1, split), sub(line, split+1, -1)
                                nomsu\append bite
                                nomsu\append "\\\n.." if #line > 0
                    elseif bit.type == "Text"
                        add_text(bit)
                    else
                        nomsu\append "\\"
                        interp_nomsu = recurse(bit)
                        unless interp_nomsu\is_multiline!
                            if bit.type == "Var"
                                if type(tree[i+1]) == 'string' and not match(tree[i+1], "^[ \n\t,.:;#(){}[%]]")
                                    interp_nomsu\parenthesize!
                            elseif bit.type == "EscapedNomsu" or bit.type == "Block" or bit.type == "IndexChain"
                                interp_nomsu\parenthesize!
                        nomsu\append interp_nomsu
                        if interp_nomsu\is_multiline!
                            nomsu\append "\n.."
            add_text(tree)
            return NomsuCode\from(tree.source, '"\\\n    ..', nomsu, '"')

        when "List", "Dict"
            if #tree == 0
                nomsu\append(tree.type == "List" and "[]" or "{}")
                return nomsu
            sep = ''
            for i, item in ipairs tree
                item_nomsu = tree_to_inline_nomsu(item)
                if #item_nomsu\text! > MAX_LINE
                    item_nomsu = recurse(item)
                if item.type == 'Comment'
                    item_nomsu = tree_to_nomsu(item)
                nomsu\append sep
                nomsu\append item_nomsu
                if item_nomsu\is_multiline! or item.type == 'Comment' or nomsu\trailing_line_len! + #tostring(item_nomsu) >= MAX_LINE
                    sep = '\n'
                else
                    sep = ', '
            return if tree.type == "List" then
                NomsuCode\from(tree.source, "[..]\n    ", nomsu)
            else
                NomsuCode\from(tree.source, "{..}\n    ", nomsu)
        
        when "DictEntry"
            key, value = tree[1], tree[2]
            nomsu = if key.type == "Text" and #key == 1 and is_identifier(key[1])
                NomsuCode\from(key.source, key[1])
            else tree_to_inline_nomsu(key)
            nomsu\parenthesize! if key.type == "Block"
            if value
                value_nomsu = tree_to_nomsu(value)
                if (value.type == "Block" or value.type == "EscapedNomsu") and not value_nomsu\is_multiline!
                    value_nomsu\parenthesize!
                nomsu\append ": ", value_nomsu
            return nomsu

        when "Comment"
            nomsu\append "#", (tree[1]\gsub("\n", "\n    "))
            return nomsu
        
        when "IndexChain", "Number", "Var", "Comment", "Error"
            return tree_to_inline_nomsu tree
        
        else
            error("Unknown type: #{tree.type}")

return {:tree_to_nomsu, :tree_to_inline_nomsu}
