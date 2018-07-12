local lpeg = require('lpeg')
local re = require('re')
lpeg.setmaxstack(10000)
local P, R, S, C, Cmt, Carg
P, R, S, C, Cmt, Carg = lpeg.P, lpeg.R, lpeg.S, lpeg.C, lpeg.Cmt, lpeg.Carg
local match, sub
do
  local _obj_0 = string
  match, sub = _obj_0.match, _obj_0.sub
end
local insert, remove
do
  local _obj_0 = table
  insert, remove = _obj_0.insert, _obj_0.remove
end
local files = require('files')
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
  _with_0.userdata = Carg(1)
  _with_0.error = function(src, end_pos, start_pos, err_msg, userdata)
    local seen_errors = userdata.errors
    if seen_errors[start_pos] then
      return true
    end
    local num_errors = 0
    for _ in pairs(seen_errors) do
      num_errors = num_errors + 1
    end
    if num_errors >= 10 then
      seen_errors[start_pos + 1] = colored.bright(colored.yellow(colored.onred("Too many errors, canceling parsing...")))
      return #src + 1
    end
    local err_pos = start_pos
    local line_no = files.get_line_number(src, err_pos)
    local prev_line = line_no == 1 and "" or files.get_line(src, line_no - 1)
    local err_line = files.get_line(src, line_no)
    local next_line = files.get_line(src, line_no + 1)
    local i = err_pos - files.get_line_starts(src)[line_no]
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
      if key == "Comment" then
        value = value[1]
      else
        local comments = { }
        for i = #value, 1, -1 do
          if type(value[i]) == 'table' and value[i].type == "Comment" then
            insert(comments, remove(value, i))
          end
        end
        if #comments > 0 then
          value.comments = comments
        end
      end
      setmetatable(value, AST[key])
      if value.__init then
        value:__init()
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
  local peg_tidier = re.compile([[    file <- %nl* version %nl* {~ (def/comment) (%nl+ (def/comment))* %nl* ~}
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
    ]], {
    set_version = function(v)
      Parser.version = tonumber(v), {
        nl = NOMSU_DEFS.nl
      }
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
  source = source or nomsu_code.source
  nomsu_code = tostring(nomsu_code)
  local userdata = {
    indent = "",
    errors = { },
    source = source,
    comments = { }
  }
  local tree = NOMSU_PATTERN:match(nomsu_code, nil, userdata)
  if not (tree) then
    error("In file " .. tostring(colored.blue(tostring(source or "<unknown>"))) .. " failed to parse:\n" .. tostring(colored.onyellow(colored.black(nomsu_code))))
  end
  if type(tree) == 'number' then
    return nil
  end
  if next(userdata.errors) then
    local keys
    do
      local _accum_0 = { }
      local _len_0 = 1
      for k, v in pairs(userdata.errors) do
        _accum_0[_len_0] = k
        _len_0 = _len_0 + 1
      end
      keys = _accum_0
    end
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
  tree.comments = userdata.comments
  tree.version = userdata.version
  return tree
end
return Parser
