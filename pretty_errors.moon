-- This file has code for converting errors to user-friendly format, with colors,
-- line numbers, code excerpts, and so on.
require "containers"
string2 = require 'string2'

box = (text)->
    max_line = 0
    for line in text\gmatch("[^\n]+")
        max_line = math.max(max_line, #(line\gsub("\027%[[0-9;]*m","")))
    ret = ("\n"..text)\gsub "\n([^\n]*)", (line)->
        line_len = #(line\gsub("\027%[[0-9;]*m",""))
        return "\n#{line}#{(" ")\rep(max_line-line_len)} \027[0m"
    return ret\sub(2,-1), max_line

format_error = (err)->
    context = err.context or 2
    err_line, err_linenum, err_linepos = string2.line_at(err.source, err.start)
    -- TODO: better handle multi-line errors
    err_size = math.min((err.stop - err.start), (#err_line-err_linepos) + 1)
    nl_indicator = (err_linepos > #err_line) and " " or ""
    fmt_str = " %#{#tostring(err_linenum+context)}d|"
    pointer = if err_size >= 2
        (" ")\rep(err_linepos+#fmt_str\format(0)-1).."╚#{("═")\rep(err_size-2)}╝"
    else
        (" ")\rep(err_linepos+#fmt_str\format(0)-1).."⬆"
    err_msg = "\027[33;41;1m#{err.title or "Error"} at #{err.filename or '???'}:#{err_linenum}\027[0m"
    for i=err_linenum-context,err_linenum-1
        if line = string2.line(err.source, i)
            err_msg ..= "\n\027[2m#{fmt_str\format(i)}\027[0m#{line}\027[0m"
    if err_line
        before = err_line\sub(1, err_linepos-1)
        during = err_line\sub(err_linepos,err_linepos+err_size-1)
        after = err_line\sub(err_linepos+err_size, -1)
        err_line = "\027[0m#{before}\027[41;30m#{during}#{nl_indicator}\027[0m#{after}"
        err_msg ..= "\n\027[2m#{fmt_str\format(err_linenum)}#{err_line}\027[0m"
    _, err_linenum_end, err_linepos_end = string2.line_at(err.source, err.stop)
    err_linenum_end or= err_linenum
    if err_linenum_end == err_linenum
        err_msg ..= "\n#{pointer}"
    else
        for i=err_linenum+1,err_linenum_end
            if line = string2.line(err.source, i)
                if i == err_linenum_end
                    during, after = line\sub(1,err_linepos_end-1), line\sub(err_linepos_end,-1)
                    err_msg ..= "\n\027[2m#{fmt_str\format(i)}\027[0;41;30m#{during}\027[0m#{after}"
                else
                    err_msg ..= "\n\027[2m#{fmt_str\format(i)}\027[0;41;30m#{line}\027[0m"
            if i > err_linenum+1 + 5
                err_msg ..= "\n       ...\n"
                break

    box_width = 70
    err_text = "\027[47;31;1m#{string2.wrap(" "..err.error, box_width, 16)\gsub("\n", "\n\027[47;31;1m ")}"
    if err.hint
        err_text ..= "\n\027[47;30m#{string2.wrap(" Suggestion: #{err.hint}", box_width, 16)\gsub("\n", "\n\027[47;30m ")}"
    err_msg ..= "\n\027[33;1m "..box(err_text)\gsub("\n", "\n ")

    for i=err_linenum_end+1,err_linenum_end+context
        if line = string2.line(err.source, i)
            err_msg ..= "\n\027[2m#{fmt_str\format(i)}\027[0m#{line}\027[0m"
    return err_msg

return format_error
