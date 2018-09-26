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
    ["and"]:true, ["break"]:true, ["do"]:true, ["else"]:true, ["elseif"]:true, ["end"]:true,
    ["false"]:true, ["for"]:true, ["function"]:true, ["goto"]:true, ["if"]:true,
    ["in"]:true, ["local"]:true, ["nil"]:true, ["not"]:true, ["or"]:true, ["repeat"]:true,
    ["return"]:true, ["then"]:true, ["true"]:true, ["until"]:true, ["while"]:true
}
is_lua_id = (str)->
    match(str, "^[_a-zA-Z][_a-zA-Z0-9]*$") and not lua_keywords[str]

string2 = {
    :isplit, uppercase:upper, lowercase:lower, reversed:reverse, :is_lua_id
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

    as_lua: =>
        escaped = gsub(@, "\\", "\\\\")
        escaped = gsub(escaped, "\n", "\\n")
        escaped = gsub(escaped, '"', '\\"')
        escaped = gsub(escaped, "[^ %g]", (c)-> format("\\%03d", byte(c, 1)))
        return '"'..escaped..'"'

    as_nomsu: =>
        escaped = gsub(@, "\\", "\\\\")
        escaped = gsub(escaped, "\n", "\\n")
        escaped = gsub(escaped, '"', '\\"')
        escaped = gsub(escaped, "[^ %g]", (c)-> format("\\%03d", byte(c, 1)))
        return '"'..escaped..'"'

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

        unless is_lua_id(str\match("^_*(.*)$"))
            str = "_"..str
        return str

    -- from_lua_id(as_lua_id(str)) == str, but behavior is unspecified for inputs that
    -- did not come from as_lua_id()
    from_lua_id: (str)->
        unless is_lua_id("^_+(.*)$")
            str = str\sub(2,-1)
        str = gsub(str, "_", " ")
        str = gsub(str, "x([0-9A-F][0-9A-F])", (hex)-> char(tonumber(hex, 16)))
        str = gsub(str, "^ ([ ]*)$", "%1")
        return str
}
for k,v in pairs(string) do string2[k] or= v

return string2
