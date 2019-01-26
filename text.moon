-- Expand the capabilities of the built-in strings
{:List, :Dict} = require "containers"
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

-- Convert an arbitrary text into a valid Lua identifier. This function is injective,
-- but not idempotent. In logic terms: (x != y) => (as_lua_id(x) != as_lua_id(y)),
-- but not (as_lua_id(a) == b) => (as_lua_id(b) == b).
as_lua_id = (str)->
    -- Escape 'x' (\x78) when it precedes something that looks like an uppercase hex sequence.
    -- This way, all Lua IDs can be unambiguously reverse-engineered, but normal usage
    -- of 'x' won't produce ugly Lua IDs.
    -- i.e. "x" -> "x", "oxen" -> "oxen", but "Hex2Dec" -> "Hex782Dec" and "He-ec" -> "Hex2Dec"
    str = gsub str, "x([0-9A-F][0-9A-F])", "x78%1"
    -- Map spaces to underscores, and everything else non-alphanumeric to hex escape sequences
    str = gsub str, "%W", (c)->
        if c == ' ' then '_'
        else format("x%02X", byte(c))

    unless is_lua_id(match(str, "^_*(.*)$"))
        str = "_"..str
    return str

-- from_lua_id(as_lua_id(str)) == str, but behavior is unspecified for inputs that
-- did not come from as_lua_id()
from_lua_id = (str)->
    unless is_lua_id(match(str, "^_*(.*)$"))
        str = sub(str,2,-1)
    str = gsub(str, "_", " ")
    str = gsub(str, "x([0-9A-F][0-9A-F])", (hex)-> char(tonumber(hex, 16)))
    return str

Text = {
    :isplit, uppercase:upper, lowercase:lower, reversed:reverse, :is_lua_id
    capitalized: => (gsub(@, '%l', upper, 1))
    byte: byte, as_a_number: tonumber, as_a_base_1_number: tonumber,
    bytes: (i, j)=> List{byte(@, i or 1, j or -1)}
    split: (sep)=> List[chunk for i,chunk in isplit(@, sep)]
    starts_with: (s)=> sub(@, 1, #s) == s
    ends_with: (s)=> #@ >= #s and sub(@, #@-#s, -1) == s
    lines: => List[line for i,line in isplit(@, '\n')]
    line: (line_num)=>
        for i, line, start in isplit(@, '\n')
            return line if i == line_num

    line_info_at: (pos)=>
        assert(type(pos) == 'number', "Invalid string position")
        for i, line, start, stop in isplit(@, '\n')
            if stop+1 >= pos
                return line, i, (pos-start+1)

    indented: (indent="    ")=>
        indent..(gsub(@, "\n", "\n"..indent))

    as_lua: =>
        escaped = gsub(@, "\\", "\\\\")
        escaped = gsub(escaped, "\n", "\\n")
        escaped = gsub(escaped, '"', '\\"')
        escaped = gsub(escaped, "[^ %g]", (c)-> format("\\%03d", byte(c, 1)))
        return '"'..escaped..'"'

    as_nomsu: => @as_lua!

    formatted_with:format, byte:byte,
    position_of:((...)->(find(...))), position_of_1_after:((...)->(find(...))),
    as_a_lua_identifier: as_lua_id, is_a_lua_identifier: is_lua_id,
    as_lua_id: as_lua_id, as_a_lua_id: as_lua_id, is_a_lua_id: is_lua_id,
    is_lua_id: is_lua_id, from_lua_id: from_lua_id,
    bytes_1_to: (start, stop)=> List{byte(@, start, stop)}
    [as_lua_id "with 1 ->"]: (...)-> (gsub(...))
    bytes: => List{byte(@, 1, -1)},
    wrapped_to: (maxlen=80, margin=8)=>
        lines = {}
        for line in *@lines!
            while #line > maxlen
                chunk = sub(line, 1, maxlen)
                split = find(chunk, ' ', maxlen-margin, true) or maxlen
                chunk = sub(line, 1, split)
                line = sub(line, split+1, -1)
                lines[#lines+1] = chunk
            lines[#lines+1] = line
        return table.concat(lines, "\n")
                            
    line_at: (i)=> (@line_info_at(i))
    line_number_at: (i)=> select(2, @line_info_at(i))
    line_position_at: (i)=> select(3, @line_info_at(i))
    matches: (patt)=> match(@, patt) and true or false
    matching: (patt)=> (match(@, patt))
    matching_groups: (patt)=> List{match(@, patt)}
    [as_lua_id "* 1"]: (n)=> rep(@, n)
    all_matches_of: (patt)=>
        result = {}
        stepper,x,i = gmatch(@, patt)
        while true
            tmp = List{stepper(x,i)}
            break if #tmp == 0
            i = tmp[1]
            result[#result+1] = (#tmp == 1) and tmp[1] or tmp
        return List(result)
    from_1_to: sub, from: sub,
    character: (i)=> sub(@, i, i)
}
for k,v in pairs(string) do Text[k] or= v

_1_as_text = (x)->
    if x == true then return "yes"
    if x == false then return "no"
    return tostring(x)

setmetatable Text,
    __call: (...)=>
        ret = {...}
        for i=1,select("#", ...)
            ret[i] = _1_as_text(ret[i])
        return table.concat(ret)

debug.setmetatable "",
    __type: "Text"
    __index: (k)=> Text[k] or (type(k) == 'number' and sub(@, k, k) or nil)
    __add: (x)=> _1_as_text(@).._1_as_text(x)

return Text
