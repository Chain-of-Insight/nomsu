-- This file contains the parser, which converts text into abstract syntax trees
lpeg = require 'lpeg'
re = require 're'
lpeg.setmaxstack 20000
{:P,:R,:S,:C,:Cmt,:Carg,:Cc} = lpeg

-- foldr {A{a1,a2,...},B{b1,b2,...},C{c1,c2,...}} -> C{B{A{a1,a2,...},b1,b2...},c1,c2...}
foldr = (...)->
    inner = select(1,...)
    for i=2,select('#',...) do
        assert inner.type
        outer = select(i,...)
        table.insert(outer, 1, inner)
        inner.start = outer.start
        inner = outer
    assert inner.type
    return inner

DEFS = with {}
    -- Newline supports either windows-style CR+LF or unix-style LF
    .nl = P("\r")^-1 * P("\n")
    .tab = P("\t")
    .tonumber = tonumber
    .tochar = string.char
    .unpack = unpack or table.unpack
    .nil = Cc(nil)
    .userdata = Carg(1)
    .utf8_char = (
        R("\194\223")*R("\128\191") +
        R("\224\239")*R("\128\191")*R("\128\191") +
        R("\240\244")*R("\128\191")*R("\128\191")*R("\128\191"))
    .Tree = (t, userdata)-> userdata.make_tree(t, userdata)
    .foldr = foldr

setmetatable(DEFS, {__index:(key)=>
    if i = key\match("^ascii_(%d+)$")
        c = string.char(tonumber(i))
        self[key] = c
        return c
    elseif i = key\match("^number_(%d+)$")
        p = Cc(tonumber(i))
        self[key] = p
        return p
})

-- Just for cleanliness, I put the language spec in its own file using a slightly
-- extended version of the lpeg.re syntax.
peg_tidier = re.compile [[
    file <- %nl* {~ (captured_def/line) (%nl+ (captured_def/line))* %nl* ~}
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    line <- [^%nl]*
    captured_def <-
        ({ident} (" "*) "(" {ident} ")" (" "*) "<-" {[^%nl]* (%nl+ " "+ [^%nl]*)*}) ->
"%1 <- ({| {:type:''->'%2':} {:start:{}:}
    %3
    {:stop:{}:} |} %%userdata) -> Tree"
]]

make_parser = (peg, make_tree=nil)->
    peg = assert(peg_tidier\match(peg))
    peg = assert(re.compile(peg, DEFS))
    return (input, filename='???')->
        input = tostring(input)
        tree_mt = {__index: {source:input, filename:filename}}
        userdata = {
            make_tree: make_tree or ((t)->setmetatable(t, tree_mt))
            :filename, source:input
        }
        tree = peg\match(input, nil, userdata)
        if not tree
            error "File #{filename} failed to parse:\n#{input}"
        return tree

return make_parser
