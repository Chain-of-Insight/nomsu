{:NomsuCode} = require "code_obj"
{:find, :sub, :match} = string
{:R,:P,:S} = require 'lpeg'
re = require 're'

MAX_LINE = 90

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
                    clump_words = (type(tree[i-1]) == 'string' and is_operator(bit) != is_operator(tree[i-1]))
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
            assert(value.type != "Block", "Didn't expect to find a Block as a value in a dict")
            nomsu\append ":"
            if value
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
            return NomsuCode\from(tree.source, tostring(tree[1]))

        when "Var"
            return NomsuCode\from(tree.source, "%", tree[1])

        when "FileChunks"
            error("Can't inline a FileChunks")
        
        when "Comment"
            -- TODO: implement?
            return nil
        
        when "Error"
            error("Can't compile errors")
        
        else
            error("Unknown type: #{tree.type}")

tree_to_nomsu = (tree)->
    nomsu = NomsuCode\from(tree.source)

    -- For concision:
    recurse = (t)->
        space = MAX_LINE - nomsu\trailing_line_len!
        inline = true
        for subtree in coroutine.wrap(-> (t\map(coroutine.yield) and nil))
            if subtree.type == "Block"
                if #subtree > 1 or #tree_to_inline_nomsu(subtree)\text! > 20
                    inline = false
        
        if inline
            inline_nomsu = tree_to_inline_nomsu(t)
            if #inline_nomsu\text! <= space
                if t.type == "Action"
                    inline_nomsu\parenthesize!
                return inline_nomsu
        indented = tree_to_nomsu(t)
        if t.type == "Action"
            if indented\is_multiline!
                return NomsuCode\from(t.source, "(..)\n    ", indented)
            else indented\parenthesize!
        return indented

    switch tree.type
        when "FileChunks"
            should_clump = (prev_line, line)->
                if prev_line.type == "Action" and line.type == "Action"
                    if prev_line.stub == "use" then return line.stub == "use"
                    if prev_line.stub == "test" then return true
                    if line.stub == "test" then return false
                return not recurse(prev_line)\is_multiline!

            for chunk_no, chunk in ipairs tree
                nomsu\append "\n\n#{("~")\rep(80)}\n\n" if chunk_no > 1
                if chunk.type == "Block"
                    for line_no, line in ipairs chunk
                        if line_no > 1
                            if should_clump(chunk[line_no-1], line)
                                nomsu\append "\n"
                            else
                                nomsu\append "\n\n"
                        nomsu\append tree_to_nomsu(line)
                else
                    nomsu\append tree_to_nomsu(chunk)
            nomsu\append('\n') unless nomsu\match("\n$")
            return nomsu

        when "Action"
            next_space = ""
            if tree.target
                target_nomsu = recurse(tree.target)
                if (tree.target.type == "Block" or tree.target.type == "EscapedNomsu") and not target_nomsu\is_multiline!
                    target_nomsu\parenthesize!
                nomsu\append target_nomsu
                nomsu\append(target_nomsu\is_multiline! and "\n..::" or "::")

            for i,bit in ipairs tree
                if type(bit) == "string"
                    unless next_space == " " and (type(tree[i-1]) == 'string' and is_operator(tree[i-1]) != is_operator(bit))
                        nomsu\append next_space
                    nomsu\append bit
                    next_space = nomsu\trailing_line_len! > MAX_LINE and " \\\n.." or " "
                else
                    bit_nomsu = recurse(bit)
                    if i < #tree and (bit.type == "Block" or bit.type == "EscapedNomsu") and not bit_nomsu\is_multiline!
                        bit_nomsu\parenthesize!

                    if next_space == " " and not bit_nomsu\is_multiline! and nomsu\trailing_line_len! + #bit_nomsu\text! > MAX_LINE
                        next_space = " \\\n.."
                    unless next_space == " " and bit.type == "Block"
                        nomsu\append next_space

                    nomsu\append bit_nomsu
                    next_space = bit_nomsu\is_multiline! and "\n.." or " "

            return nomsu

        when "EscapedNomsu"
            return NomsuCode tree.source, "\\", recurse(tree[1])

        when "Block"
            for i, line in ipairs tree
                line_nomsu = tree_to_nomsu(line)
                nomsu\append line_nomsu
                if i < #tree
                    -- number of lines > 2 (TODO: improve this)
                    nomsu\append(line_nomsu\match('\n[^\n]*\n') and "\n\n" or "\n")
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
                            elseif bit.type == "EscapedNomsu" or bit.type == "Block"
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
            for i, item in ipairs tree
                item_nomsu = tree_to_inline_nomsu(item)
                if #item_nomsu\text! > MAX_LINE
                    item_nomsu = recurse(item)
                elseif item.type == "Block" or item.type == "EscapedNomsu"
                    item_nomsu\parenthesize!
                nomsu\append item_nomsu
                if i < #tree
                    nomsu\append((item_nomsu\is_multiline! or nomsu\trailing_line_len! + #tostring(item_nomsu) >= MAX_LINE) and '\n' or ', ')
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
            value_nomsu = tree_to_nomsu(value)
            if (value.type == "Block" or value.type == "EscapedNomsu") and not value_nomsu\is_multiline!
                value_nomsu\parenthesize!
            nomsu\append ": ", value_nomsu
            return nomsu

        when "Comment"
            nomsu\append "#", tree[1]\gsub("\n", "\n    ")
            return nomsu
        
        when "IndexChain", "Number", "Var", "Comment", "Error"
            return tree_to_inline_nomsu tree
        
        else
            error("Unknown type: #{tree.type}")

return {:tree_to_nomsu, :tree_to_inline_nomsu}
