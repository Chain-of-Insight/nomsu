-- Expand the capabilities of the built-in strings
{:reverse, :upper, :lower, :find, :byte, :match, :gmatch, :gsub, :sub, :format, :rep, :char} = string

isplit = (sep='%s+')=>
    step = (i)=>
        start = @pos
        return unless start
        i += 1
        nl = find(@str, @sep, start)
        @pos = nl and (nl+1) or nil
        line = sub(@str, start, nl and (nl-1) or #@str)
        return i, line, start, (nl and (nl-1) or #@str)
    return step, {str:@, pos:1, :sep}, 0

lua_keywords = {
    "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if",
    "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"
}

string2 = {
    :isplit, uppercase:upper, lowercase:lower, reversed:reverse
    capitalized: => gsub(@, '%l', upper, 1)
    byte: byte, bytes: (i, j)=> {byte(@, i or 1, j or -1)}
    split: (sep)=> [chunk for i,chunk in isplit(@, sep)]
    lines: => [line for i,line in isplit(@, '\n')]
    line: (line_num)=>
        for i, line, start in isplit(@, '\n')
            return line if i == line_num

    line_at: (pos)=>
        assert(type(pos) == 'number', "Invalid string position")
        for i, line, start, stop in isplit(@, '\n')
            if stop+1 >= pos
                return line, i, (pos-start+1)

    wrap: (maxlen=80, buffer=8)=>
        lines = {}
        for line in *@lines!
            while #line > maxlen
                chunk = line\sub(1, maxlen)
                split = chunk\find(' ', maxlen-buffer, true) or maxlen
                chunk = line\sub(1, split)
                line = line\sub(split+1, -1)
                lines[#lines+1] = chunk
            lines[#lines+1] = line
        return table.concat(lines, "\n")

    -- Convert an arbitrary text into a valid Lua identifier. This function is injective,
    -- but not idempotent. In logic terms: (x != y) => (as_lua_id(x) != as_lua_id(y)),
    -- but not (as_lua_id(a) == b) => (as_lua_id(b) == b).
    as_lua_id: (str)->
        orig = str
        -- Empty strings are not valid lua identifiers, so treat them like " ",
        -- and treat " " as "  ", etc. to preserve injectivity.
        str = gsub str, "^ *$", "%1 "
        -- Escape 'x' (\x78) when it precedes something that looks like an uppercase hex sequence.
        -- This way, all Lua IDs can be unambiguously reverse-engineered, but normal usage
        -- of 'x' won't produce ugly Lua IDs.
        -- i.e. "x" -> "x", "oxen" -> "oxen", but "Hex2Dec" -> "Hex782Dec" and "He-ec" -> "Hex2Dec"
        str = gsub str, "x([0-9A-F][0-9A-F])", "x78%1"
        -- Map spaces to underscores, and everything else non-alphanumeric to hex escape sequences
        str = gsub str, "%W", (c)->
            if c == ' ' then '_'
            else format("x%02X", byte(c))
        -- Lua IDs can't start with numbers, so map "1" -> "_1", "_1" -> "__1", etc.
        str = gsub str, "^_*%d", "_%1"
        -- This pattern is guaranteed to match all keywords, but also matches some other stuff.
        if match str, "^_*[abdefgilnortuw][aefhilnoru][acdefiklnoprstu]*$"
            for kw in *lua_keywords
                if match str, ("^_*"..kw.."$")
                    str = "_"..str
        return str

    -- from_lua_id(as_lua_id(str)) == str, but behavior is unspecified for inputs that
    -- did not come from as_lua_id()
    from_lua_id: (str)->
        -- This pattern is guaranteed to match all keywords, but also matches some other stuff.
        if match str, "^_+[abdefgilnortuw][aefhilnoru][acdefiklnoprstu]*$"
            for kw in *lua_keywords
                if match str, ("^_+"..kw.."$")
                    str = str\sub(2,-1)
        str = gsub(str, "^_(_*%d.*)", "%1")
        str = gsub(str, "_", " ")
        str = gsub(str, "x([0-9A-F][0-9A-F])", (hex)-> char(tonumber(hex, 16)))
        str = gsub(str, "^ ([ ]*)$", "%1")
        return str
}
for k,v in pairs(string) do string2[k] or= v

return string2
