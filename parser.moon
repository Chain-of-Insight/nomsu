-- This file contains the parser, which converts Nomsu text into abstract syntax trees
lpeg = require 'lpeg'
re = require 're'
lpeg.setmaxstack 10000
{:P,:R,:S,:C,:Cmt,:Carg} = lpeg
{:match, :sub} = string
{:insert, :remove} = table
files = require 'files'
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
AST = require "nomsu_tree"

NOMSU_DEFS = with {}
    -- Newline supports either windows-style CR+LF or unix-style LF
    .nl = P("\r")^-1 * P("\n")
    .ws = S(" \t")
    .tonumber = tonumber
    .table = -> {}
    .unpack = unpack or table.unpack
    string_escapes = n:"\n", t:"\t", b:"\b", a:"\a", v:"\v", f:"\f", r:"\r"
    digit, hex = R('09'), R('09','af','AF')
    .escaped_char = (P("\\")*S("xX")*C(hex*hex)) / => string.char(tonumber(@, 16))
    .escaped_char += (P("\\")*C(digit*(digit^-2))) / => string.char(tonumber @)
    .escaped_char += (P("\\")*C(S("ntbavfr"))) / string_escapes
    .operator_char = S("'`~!@$^&*-+=|<>?/")
    .utf8_char = (
        R("\194\223")*R("\128\191") +
        R("\224\239")*R("\128\191")*R("\128\191") +
        R("\240\244")*R("\128\191")*R("\128\191")*R("\128\191"))
    .ident_char = R("az","AZ","09") + P("_") + .utf8_char

    .userdata = Carg(1)

    .add_comment = (src,end_pos,start_pos,comment,userdata)->
        userdata.comments[start_pos] = comment
        return true

    .error = (src,end_pos,start_pos,err_msg,userdata)->
        seen_errors = userdata.errors
        if seen_errors[start_pos]
            return true
        num_errors = 0
        for _ in pairs(seen_errors) do num_errors += 1
        if num_errors >= 10
            seen_errors[start_pos+1] = colored.bright colored.yellow colored.onred "Too many errors, canceling parsing..."
            return #src+1
        err_pos = start_pos
        line_no = files.get_line_number(src, err_pos)
        --src = files.read(userdata.source.filename)
        prev_line = line_no == 1 and "" or files.get_line(src, line_no-1)
        err_line = files.get_line(src, line_no)
        next_line = files.get_line(src, line_no+1)
        i = err_pos-files.get_line_starts(src)[line_no]
        pointer = ("-")\rep(i) .. "^"
        err_msg = colored.bright colored.yellow colored.onred (err_msg or "Parse error").." at #{userdata.source.filename}:#{line_no}:"
        if #prev_line > 0 then err_msg ..= "\n"..colored.dim(prev_line)
        err_line = colored.white(err_line\sub(1, i))..colored.bright(colored.red(err_line\sub(i+1,i+1)))..colored.dim(err_line\sub(i+2,-1))
        err_msg ..= "\n#{err_line}\n#{colored.red pointer}"
        if #next_line > 0 then err_msg ..= "\n"..colored.dim(next_line)
        seen_errors[start_pos] = err_msg
        return true

setmetatable(NOMSU_DEFS, {__index:(key)=>
    make_node = (start, value, stop, userdata)->
        if userdata.source
            with userdata.source
                value.source = Source(.filename, .start + start-1, .start + stop-1)
        setmetatable(value, AST[key])
        value.comments = userdata.comments
        if value.__init then value\__init!
        return value

    self[key] = make_node
    return make_node
})

Parser = {}
NOMSU_PATTERN = do
    -- Just for cleanliness, I put the language spec in its own file using a slightly modified
    -- version of the lpeg.re syntax.
    peg_tidier = re.compile [[
    file <- %nl* version %nl* {~ (def/comment) (%nl+ (def/comment))* %nl* ~}
    version <- "--" (!"version" [^%nl])* "version" (" ")* (([0-9])+ -> set_version) ([^%nl])*
    def <- anon_def / captured_def
    anon_def <- ({ident} (" "*) ":"
        {~ ((%nl " "+ def_line?)+) / def_line ~}) -> "%1 <- %2"
    captured_def <- ({ident} (" "*) "(" {ident} ")" (" "*) ":"
        {~ ((%nl " "+ def_line?)+) / def_line ~}) -> "%1 <- (({} {| %3 |} {} %%userdata) -> %2)"
    def_line <- (err / [^%nl])+
    err <- ("(!!" { (!("!!)") .)* } "!!)") -> "(({} (%1) %%userdata) => error)"
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    comment <- "--" [^%nl]*
    ]], {set_version: (v) -> Parser.version = tonumber(v), nl:NOMSU_DEFS.nl}
    peg_file = io.open("nomsu.peg")
    if not peg_file and package.nomsupath
        for path in package.nomsupath\gmatch("[^;]+")
            peg_file = io.open(path.."/nomsu.peg")
            break if peg_file
    assert(peg_file, "could not find nomsu.peg file")
    nomsu_peg = peg_tidier\match(peg_file\read('*a'))
    peg_file\close!
    re.compile(nomsu_peg, NOMSU_DEFS)

Parser.parse = (nomsu_code, source=nil)->
    source or= nomsu_code.source
    nomsu_code = tostring(nomsu_code)
    userdata = {
        errors: {}, :source, comments: {}
    }
    tree = NOMSU_PATTERN\match(nomsu_code, nil, userdata)
    unless tree
        error "In file #{colored.blue tostring(source or "<unknown>")} failed to parse:\n#{colored.onyellow colored.black nomsu_code}"
    if type(tree) == 'number'
        return nil

    if next(userdata.errors)
        keys = [k for k,v in pairs(userdata.errors)]
        table.sort(keys)
        errors = [userdata.errors[k] for k in *keys]
        error("Errors occurred while parsing:\n\n"..table.concat(errors, "\n\n"), 0)
    
    tree.version = userdata.version
    return tree

return Parser
