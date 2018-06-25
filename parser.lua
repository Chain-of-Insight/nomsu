local lpeg = require('lpeg')
local re = require('re')
lpeg.setmaxstack(10000)
local P, R, V, S, Cg, C, Cp, B, Cmt, Carg
P, R, V, S, Cg, C, Cp, B, Cmt, Carg = lpeg.P, lpeg.R, lpeg.V, lpeg.S, lpeg.Cg, lpeg.C, lpeg.Cp, lpeg.B, lpeg.Cmt, lpeg.Carg
local utils = require('utils')
local match, sub
do
  local _obj_0 = string
  match, sub = _obj_0.match, _obj_0.sub
end
local NomsuCode, LuaCode, Source
do
  local _obj_0 = require("code_obj")
  NomsuCode, LuaCode, Source = _obj_0.NomsuCode, _obj_0.LuaCode, _obj_0.Source
end
local AST = require("nomsu_tree")
local NOMSU_DEFS
do
  local _with_0 = { }
  _with_0.nl = P("\r") ^ -1 * P("\n")
  _with_0.ws = S(" \t")
  _with_0.tonumber = tonumber
  local string_escapes = {
    n = "\n",
    t = "\t",
    b = "\b",
    a = "\a",
    v = "\v",
    f = "\f",
    r = "\r"
  }
  local digit, hex = R('09'), R('09', 'af', 'AF')
  _with_0.escaped_char = (P("\\") * S("xX") * C(hex * hex)) / function(self)
    return string.char(tonumber(self, 16))
  end
  _with_0.escaped_char = _with_0.escaped_char + ((P("\\") * C(digit * (digit ^ -2))) / function(self)
    return string.char(tonumber(self))
  end)
  _with_0.escaped_char = _with_0.escaped_char + ((P("\\") * C(S("ntbavfr"))) / string_escapes)
  _with_0.operator_char = S("'`~!@$^&*-+=|<>?/")
  _with_0.utf8_char = (R("\194\223") * R("\128\191") + R("\224\239") * R("\128\191") * R("\128\191") + R("\240\244") * R("\128\191") * R("\128\191") * R("\128\191"))
  _with_0.ident_char = R("az", "AZ", "09") + P("_") + _with_0.utf8_char
  _with_0.indent = Cmt(Carg(1), function(self, start, userdata)
    local indented = userdata.indent .. '    '
    if sub(self, start, start + #indented - 1) == indented then
      userdata.indent = indented
      return start + #indented
    end
  end)
  _with_0.dedent = Cmt(Carg(1), function(self, start, userdata)
    local dedented = sub(userdata.indent, 1, -5)
    if #match(self, "^[ ]*", start) <= #dedented then
      userdata.indent = dedented
      return start
    end
  end)
  _with_0.nodent = Cmt(Carg(1), function(self, start, userdata)
    if sub(self, start, start + #userdata.indent - 1) == userdata.indent then
      return start + #userdata.indent
    end
  end)
  _with_0.userdata = Carg(1)
  _with_0.error = function(src, end_pos, start_pos, err_msg, userdata)
    local seen_errors = userdata.errors
    if seen_errors[start_pos] then
      return true
    end
    if utils.size(seen_errors) >= 10 then
      seen_errors[start_pos + 1] = colored.bright(colored.yellow(colored.onred("Too many errors, canceling parsing...")))
      return #src + 1
    end
    local err_pos = start_pos
    local line_no = pos_to_line(src, err_pos)
    local line_starts = LINE_STARTS[src]
    local prev_line = line_no == 1 and "" or src:sub(line_starts[line_no - 1] or 1, line_starts[line_no] - 2)
    local err_line = src:sub(line_starts[line_no], (line_starts[line_no + 1] or 0) - 2)
    local next_line = src:sub(line_starts[line_no + 1] or -1, (line_starts[line_no + 2] or 0) - 2)
    local i = err_pos - line_starts[line_no]
    local pointer = ("-"):rep(i) .. "^"
    err_msg = colored.bright(colored.yellow(colored.onred((err_msg or "Parse error") .. " at " .. tostring(userdata.source.filename) .. ":" .. tostring(line_no) .. ":")))
    if #prev_line > 0 then
      err_msg = err_msg .. ("\n" .. colored.dim(prev_line))
    end
    err_line = colored.white(err_line:sub(1, i)) .. colored.bright(colored.red(err_line:sub(i + 1, i + 1))) .. colored.dim(err_line:sub(i + 2, -1))
    err_msg = err_msg .. "\n" .. tostring(err_line) .. "\n" .. tostring(colored.red(pointer))
    if #next_line > 0 then
      err_msg = err_msg .. ("\n" .. colored.dim(next_line))
    end
    seen_errors[start_pos] = err_msg
    return true
  end
  NOMSU_DEFS = _with_0
end
setmetatable(NOMSU_DEFS, {
  __index = function(self, key)
    local make_node
    make_node = function(start, value, stop, userdata)
      if userdata.source then
        do
          local _with_0 = userdata.source
          value.source = Source(_with_0.filename, _with_0.start + start - 1, _with_0.start + stop - 1)
        end
      end
      setmetatable(value, AST[key])
      if value.__init then
        value:__init()
      end
      for i = 1, #value do
        assert(value[i])
      end
      return value
    end
    self[key] = make_node
    return make_node
  end
})
local Parser = { }
local NOMSU_PATTERN
do
  local peg_tidier = re.compile([[    file <- %nl* version? %nl* {~ (def/comment) (%nl+ (def/comment))* %nl* ~}
    version <- "--" (!"version" [^%nl])* "version" ([ ])* (([0-9])+ -> set_version) ([^%nl])*
    def <- anon_def / captured_def
    anon_def <- ({ident} (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- %2"
    captured_def <- ({ident} (" "*) "(" {ident} ")" (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- (({} %3 {} %%userdata) -> %2)"
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    comment <- "--" [^%nl]*
    ]], {
    set_version = function(v)
      Parser.version = tonumber(v)
    end
  })
  local peg_file = io.open("nomsu.peg")
  if not peg_file and package.nomsupath then
    for path in package.nomsupath:gmatch("[^;]+") do
      peg_file = io.open(path .. "/nomsu.peg")
      if peg_file then
        break
      end
    end
  end
  assert(peg_file, "could not find nomsu.peg file")
  local nomsu_peg = peg_tidier:match(peg_file:read('*a'))
  peg_file:close()
  NOMSU_PATTERN = re.compile(nomsu_peg, NOMSU_DEFS)
end
Parser.parse = function(nomsu_code, source)
  if source == nil then
    source = nil
  end
  nomsu_code = tostring(nomsu_code)
  local userdata = {
    indent = "",
    errors = { },
    source = source
  }
  local tree = NOMSU_PATTERN:match(nomsu_code, nil, userdata)
  if not (tree) then
    error("In file " .. tostring(colored.blue(tostring(source or "<unknown>"))) .. " failed to parse:\n" .. tostring(colored.onyellow(colored.black(nomsu_code))))
  end
  if type(tree) == 'number' then
    tree = nil
  end
  if next(userdata.errors) then
    local keys = utils.keys(userdata.errors)
    table.sort(keys)
    local errors
    do
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #keys do
        local k = keys[_index_0]
        _accum_0[_len_0] = userdata.errors[k]
        _len_0 = _len_0 + 1
      end
      errors = _accum_0
    end
    error("Errors occurred while parsing:\n\n" .. table.concat(errors, "\n\n"), 0)
  end
  return tree
end
return Parser
