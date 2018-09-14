-- This file contains the parser, which converts Nomsu text into abstract syntax trees
lpeg = require 'lpeg'
re = require 're'
lpeg.setmaxstack 20000
{:P,:R,:S,:C,:Cmt,:Carg} = lpeg
{:match, :sub} = string
{:insert, :remove} = table
files = require 'files'
{:NomsuCode, :LuaCode, :Source} = require "code_obj"
AST = require "syntax_tree"

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
    .operator_char = S("'`~!@$^&*+=|<>?/-")
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
        prev_line = line_no == 1 and nil or files.get_line(src, line_no-1)
        err_line = files.get_line(src, line_no)
        next_line = files.get_line(src, line_no+1)
        i = err_pos-files.get_line_starts(src)[line_no]
        j = i + (end_pos-start_pos)
        pointer = ("-")\rep(i) .. "^"
        err_msg = colored.bright colored.yellow colored.onred (err_msg or "Parse error").." at #{userdata.source.filename}:#{line_no}:"
        if prev_line then err_msg ..= "\n"..colored.dim(prev_line)
        if err_line
            err_line = colored.white(err_line\sub(1, i))..colored.bright(colored.red(err_line\sub(i+1,j+1)))..colored.dim(err_line\sub(j+2,-1))
            err_msg ..= "\n#{err_line}\n#{colored.red pointer}"
        if next_line then err_msg ..= "\n"..colored.dim(next_line)
        seen_errors[start_pos] = err_msg
        return true

setmetatable(NOMSU_DEFS, {__index:(key)=>
    make_node = (start, value, stop, userdata)->
        if userdata.source
            with userdata.source
                value.source = Source(.filename, .start + start-1, .start + stop-1)
        setmetatable(value, AST[key])
        if value.__init then value\__init!
        return value

    self[key] = make_node
    return make_node
})

Parser = {version:4, patterns:{}}

Parser.is_operator = (s)->
    return not not (NOMSU_DEFS.operator_char^1 * -1)\match(s)

Parser.is_identifier = (s)->
    return not not (NOMSU_DEFS.ident_char^1 * -1)\match(s)

inline_escaper = re.compile "{~ (%utf8_char / ('\"' -> '\\\"') / ('\n' -> '\\n') / ('\t' -> '\\t') / ('\b' -> '\\b') / ('\a' -> '\\a') / ('\v' -> '\\v') / ('\f' -> '\\f') / ('\r' -> '\\r') / ('\\' -> '\\\\') / ([^ -~] -> escape) / .)* ~}", {utf8_char: NOMSU_DEFS.utf8_char, escape:(=> ("\\%03d")\format(@byte!))}
Parser.inline_escape = (s)->
    return inline_escaper\match(s)
escaper = re.compile "{~ (%utf8_char / ('\\' -> '\\\\') / [\n\r\t -~] / (. -> escape))* ~}", {utf8_char: NOMSU_DEFS.utf8_char, escape:(=> ("\\%03d")\format(@byte!))}
Parser.escape = (s)->
    return escaper\match(s)

return Parser
