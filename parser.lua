local lpeg = require('lpeg')
local re = require('re')
lpeg.setmaxstack(20000)
local P, R, S, C, Cmt, Carg, Cc
P, R, S, C, Cmt, Carg, Cc = lpeg.P, lpeg.R, lpeg.S, lpeg.C, lpeg.Cmt, lpeg.Carg, lpeg.Cc
local foldr
foldr = function(...)
  local inner = select(1, ...)
  for i = 2, select('#', ...) do
    assert(inner.type)
    local outer = select(i, ...)
    table.insert(outer, 1, inner)
    inner.start = outer.start
    inner = outer
  end
  assert(inner.type)
  return inner
end
local DEFS
do
  local _with_0 = { }
  _with_0.nl = P("\r") ^ -1 * P("\n")
  _with_0.tab = P("\t")
  _with_0.tonumber = tonumber
  _with_0.tochar = string.char
  _with_0.unpack = unpack or table.unpack
  _with_0["nil"] = Cc(nil)
  _with_0.userdata = Carg(1)
  _with_0.indentation = lpeg.Cmt(P(0), function(s, i)
    local sub = string.sub
    while i > 1 and sub(s, i - 1, i - 1) ~= '\n' do
      i = i - 1
    end
    return true, (s:match("^ *", i))
  end)
  _with_0.utf8_char = (R("\194\223") * R("\128\191") + R("\224\239") * R("\128\191") * R("\128\191") + R("\240\244") * R("\128\191") * R("\128\191") * R("\128\191"))
  _with_0.Tree = function(t, userdata)
    return userdata.make_tree(t, userdata)
  end
  _with_0.foldr = foldr
  DEFS = _with_0
end
setmetatable(DEFS, {
  __index = function(self, key)
    do
      local i = key:match("^ascii_(%d+)$")
      if i then
        local c = string.char(tonumber(i))
        self[key] = c
        return c
      else
        do
          i = key:match("^number_(%d+)$")
          if i then
            local p = Cc(tonumber(i))
            self[key] = p
            return p
          end
        end
      end
    end
  end
})
local peg_tidier = re.compile([[    file <- %nl* {~ (captured_def/line) (%nl+ (captured_def/line))* %nl* ~}
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    line <- [^%nl]*
    captured_def <-
        ({ident} (" "*) "(" {ident} ")" (" "*) "<-" {[^%nl]* (%nl+ " "+ [^%nl]*)*}) ->
"%1 <- ({| {:type:''->'%2':} {:start:{}:}
    (%3)
    {:stop:{}:} |} %%userdata) -> Tree"
]])
local make_parser
make_parser = function(peg, make_tree)
  if make_tree == nil then
    make_tree = nil
  end
  peg = assert(peg_tidier:match(peg))
  peg = assert(re.compile(peg, DEFS))
  return function(input, filename)
    if filename == nil then
      filename = '???'
    end
    input = tostring(input)
    local tree_mt = {
      __index = {
        source = input,
        filename = filename
      }
    }
    local userdata = {
      make_tree = make_tree or (function(t)
        return setmetatable(t, tree_mt)
      end),
      filename = filename,
      file = input
    }
    local tree = peg:match(input, nil, userdata)
    if not tree then
      error("File " .. tostring(filename) .. " failed to parse:\n" .. tostring(input))
    end
    return tree
  end
end
return make_parser
