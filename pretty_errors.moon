-- This file has code for converting errors to user-friendly format, with colors,
-- line numbers, code excerpts, and so on.
require "containers"
Text = require 'text'
C = require 'colors'

box = (text)->
    max_line = 0
    for line in text\gmatch("[^\n]+")
        max_line = math.max(max_line, #(line\gsub("\027%[[0-9;]*m","")))
    ret = ("\n"..text)\gsub "\n([^\n]*)", (line)->
        line_len = #(line\gsub("\027%[[0-9;]*m",""))
        return "\n#{line}#{(" ")\rep(max_line-line_len)} "..C('reset')
    return ret\sub(2,-1), max_line

format_error = (err)->
    context = err.context or 2
    err_line, err_linenum, err_linepos = err.source\line_info_at(err.start)
    -- TODO: better handle multi-line errors
    err_size = math.min((err.stop - err.start), (#err_line-err_linepos) + 1)
    nl_indicator = (err_linepos > #err_line) and " " or ""
    fmt_str = " %#{#tostring(err_linenum+context)}d|"

    pointer = (" ")\rep(err_linepos+#fmt_str\format(0)-1)..("^")\rep(err_size)
    --pointer = if err_size >= 2
    --    (" ")\rep(err_linepos+#fmt_str\format(0)-1).."╚#{("═")\rep(err_size-2)}╝"
    --else
    --    (" ")\rep(err_linepos+#fmt_str\format(0)-1).."⬆"

    err_msg = C('bold red', err.title or "Error")..C('red', " at #{err.filename or '???'}:#{err_linenum},#{err_linepos}")
    err_msg ..= "\n" unless COLOR_ENABLED
    lines = err.source\lines!
    for i=err_linenum-context,err_linenum-1
        if line = lines[i]
            err_msg ..= "\n"..C('dim', fmt_str\format(i))..line
    if err_line
        before = err_line\sub(1, err_linepos-1)
        during = err_line\sub(err_linepos,err_linepos+err_size-1)
        after = err_line\sub(err_linepos+err_size, -1)
        err_line = before..C('black on red', during..nl_indicator)..after
        err_msg ..= "\n"..C('dim', fmt_str\format(err_linenum))..err_line
    _, err_linenum_end, err_linepos_end = err.source\line_info_at(err.stop)
    err_linenum_end or= err_linenum
    if err_linenum_end == err_linenum
        err_msg ..= "\n#{pointer}"
    else
        for i=err_linenum+1,err_linenum_end
            if line = lines[i]
                if i == err_linenum_end
                    during, after = line\sub(1,err_linepos_end-1), line\sub(err_linepos_end,-1)
                    err_msg ..= "\n"..C('dim', fmt_str\format(i))..C('black on red', during)..after
                else
                    err_msg ..= "\n"..C('dim', fmt_str\format(i))..C('black on red', line)
            if i > err_linenum+1 + 5
                err_msg ..= "\n       ...\n"
                break

    box_width = 70
    err_text = C('black on white', " "..err.error\wrapped_to(box_width, 16)\gsub("\n", "\n"..C('black on white').." "))
    if err.hint
        err_text ..= "\n"..C('italic black on white', (" Suggestion: #{err.hint}")\wrapped_to(box_width, 16)\gsub("\n", "\n"..C('italic black on white').." "))
    err_msg ..= "\n "..box(err_text)\gsub("\n", "\n ")
    err_msg ..= "\n" unless COLOR_ENABLED

    for i=err_linenum_end+1,err_linenum_end+context
        if line = lines[i]
            err_msg ..= "\n"..C('dim', fmt_str\format(i))..line
    err_msg ..= "\n" unless COLOR_ENABLED
    return err_msg

return format_error
