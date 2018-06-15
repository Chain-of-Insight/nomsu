local _pairs, _ipairs = pairs, ipairs
if jit then
  package.cpath = "./luajit_lpeg/?.so;" .. package.cpath
  package.path = "./luajit_lpeg/?.lua;" .. package.path
  bit32 = require('bit')
end
lpeg = require('lpeg')
re = require('re')
lpeg.setmaxstack(10000)
local P, R, V, S, Cg, C, Cp, B, Cmt, Carg
P, R, V, S, Cg, C, Cp, B, Cmt, Carg = lpeg.P, lpeg.R, lpeg.V, lpeg.S, lpeg.Cg, lpeg.C, lpeg.Cp, lpeg.B, lpeg.Cmt, lpeg.Carg
local utils = require('utils')
local new_uuid = require('uuid')
local repr, stringify, min, max, equivalent, set, is_list, sum
repr, stringify, min, max, equivalent, set, is_list, sum = utils.repr, utils.stringify, utils.min, utils.max, utils.equivalent, utils.set, utils.is_list, utils.sum
local colors = setmetatable({ }, {
  __index = function()
    return ""
  end
})
colored = setmetatable({ }, {
  __index = function(_, color)
    return (function(msg)
      return colors[color] .. tostring(msg or '') .. colors.reset
    end)
  end
})
local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local match, sub, rep, gsub, format, byte, find
do
  local _obj_0 = string
  match, sub, rep, gsub, format, byte, match, find = _obj_0.match, _obj_0.sub, _obj_0.rep, _obj_0.gsub, _obj_0.format, _obj_0.byte, _obj_0.match, _obj_0.find
end
local debug_getinfo = debug.getinfo
local Nomsu, Lua, Source
do
  local _obj_0 = require("code_obj")
  Nomsu, Lua, Source = _obj_0.Nomsu, _obj_0.Lua, _obj_0.Source
end
local STDIN, STDOUT, STDERR = "/dev/fd/0", "/dev/fd/1", "/dev/fd/2"
string.as_lua_id = function(str)
  local argnum = 0
  str = gsub(str, "x([0-9A-F][0-9A-F])", "x\0%1")
  str = gsub(str, "%W", function(c)
    if c == ' ' then
      return '_'
    elseif c == '%' then
      argnum = argnum + 1
      return tostring(argnum)
    else
      return format("x%02X", byte(c))
    end
  end)
  return '_' .. str
end
table.map = function(self, fn)
  local _accum_0 = { }
  local _len_0 = 1
  for _, v in ipairs(self) do
    _accum_0[_len_0] = fn(v)
    _len_0 = _len_0 + 1
  end
  return _accum_0
end
FILE_CACHE = setmetatable({ }, {
  __index = function(self, filename)
    local file = io.open(filename)
    if not (file) then
      return nil
    end
    local contents = file:read("*a")
    file:close()
    self[filename] = contents
    return contents
  end
})
local iterate_single
iterate_single = function(item, prev)
  if item == prev then
    return nil
  else
    return item
  end
end
local all_files
all_files = function(path)
  if match(path, "%.nom$") or match(path, "%.lua$") or match(path, "^/dev/fd/[012]$") then
    return iterate_single, path
  end
  path = gsub(path, "\\", "\\\\")
  path = gsub(path, "`", "")
  path = gsub(path, '"', '\\"')
  path = gsub(path, "$", "")
  return coroutine.wrap(function()
    local f = io.popen('find -L "' .. path .. '" -not -path "*/\\.*" -type f -name "*.nom"')
    for line in f:lines() do
      coroutine.yield(line)
    end
    local success = f:close()
    if not (success) then
      return error("Invalid file path: " .. tostring(path))
    end
  end)
end
local line_counter = re.compile([[    lines <- {| line (%nl line)* |}
    line <- {} (!%nl .)*
]], {
  nl = P("\r") ^ -1 * P("\n")
})
local get_lines = re.compile([[    lines <- {| line (%nl line)* |}
    line <- {[^%nl]*}
]], {
  nl = P("\r") ^ -1 * P("\n")
})
LINE_STARTS = setmetatable({ }, {
  __mode = "k",
  __index = function(self, k)
    if type(k) ~= 'string' then
      k = tostring(k)
      do
        local v = rawget(self, k)
        if v then
          return v
        end
      end
    end
    local line_starts = line_counter:match(k)
    self[k] = line_starts
    return line_starts
  end
})
local pos_to_line
pos_to_line = function(str, pos)
  local line_starts = LINE_STARTS[str]
  local lo, hi = 1, #line_starts
  while lo <= hi do
    local mid = math.floor((lo + hi) / 2)
    if line_starts[mid] > pos then
      hi = mid - 1
    else
      lo = mid + 1
    end
  end
  return hi
end
do
  local STRING_METATABLE = getmetatable("")
  STRING_METATABLE.__add = function(self, other)
    return self .. stringify(other)
  end
  STRING_METATABLE.__index = function(self, i)
    local ret = string[i]
    if ret ~= nil then
      return ret
    end
    if type(i) == 'number' then
      return sub(self, i, i)
    elseif type(i) == 'table' then
      return sub(self, i[1], i[2])
    end
  end
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
  _with_0.operator_char = S("'~`!@$^&*-+=|<>?/")
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
    src = FILE_CACHE[userdata.source.filename]
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
      local source
      do
        local _with_0 = userdata.source
        source = Source(_with_0.filename, _with_0.start + start - 1, _with_0.start + stop - 1)
      end
      value.source = source
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
local NOMSU_PATTERN
do
  local peg_tidier = re.compile([[    file <- {~ %nl* (def/comment) (%nl+ (def/comment))* %nl* ~}
    def <- anon_def / captured_def
    anon_def <- ({ident} (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- %2"
    captured_def <- ({ident} (" "*) "(" {ident} ")" (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- (({} %3 {} %%userdata) -> %2)"
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    comment <- "--" [^%nl]*
    ]])
  local nomsu_peg = peg_tidier:match(FILE_CACHE["nomsu.peg"])
  NOMSU_PATTERN = re.compile(nomsu_peg, NOMSU_DEFS)
end
local NomsuCompiler
do
  local _class_0
  local compile_error, _running_files, MAX_LINE, math_expression
  local _base_0 = {
    parse = function(self, nomsu_code)
      assert(type(nomsu_code) ~= 'string')
      local userdata = {
        source_code = nomsu_code,
        indent = "",
        errors = { },
        source = nomsu_code.source
      }
      local tree = NOMSU_PATTERN:match(tostring(nomsu_code), nil, userdata)
      if not (tree) then
        error("In file " .. tostring(colored.blue(filename)) .. " failed to parse:\n" .. tostring(colored.onyellow(colored.black(nomsu_code))))
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
        io.stderr:write(concat(errors, "\n\n") .. "\n")
        os.exit()
      end
      return tree
    end,
    run = function(self, nomsu_code, compile_fn)
      if compile_fn == nil then
        compile_fn = nil
      end
      local tree = assert(self:parse(nomsu_code))
      if type(tree) == 'number' then
        return nil
      end
      local lua = self:tree_to_lua(tree):as_statements()
      lua:declare_locals()
      lua:prepend("-- File: " .. tostring(nomsu_code.source or "") .. "\n")
      if compile_fn then
        compile_fn(lua)
      end
      return self:run_lua(lua)
    end,
    run_file = function(self, filename, compile_fn)
      if compile_fn == nil then
        compile_fn = nil
      end
      local loaded = self.environment.LOADED
      if loaded[filename] then
        return loaded[filename]
      end
      local ret = nil
      for filename in all_files(filename) do
        local _continue_0 = false
        repeat
          if loaded[filename] then
            ret = loaded[filename]
            _continue_0 = true
            break
          end
          for i, running in ipairs(_running_files) do
            if running == filename then
              local loop
              do
                local _accum_0 = { }
                local _len_0 = 1
                for j = i, #_running_files do
                  _accum_0[_len_0] = _running_files[j]
                  _len_0 = _len_0 + 1
                end
                loop = _accum_0
              end
              insert(loop, filename)
              error("Circular import, this loops forever: " .. tostring(concat(loop, " -> ")))
            end
          end
          insert(_running_files, filename)
          if match(filename, "%.lua$") then
            local file = assert(FILE_CACHE[filename], "Could not find file: " .. tostring(filename))
            ret = self:run_lua(Lua(Source(filename, 1, #file), file))
          elseif match(filename, "%.nom$") or match(filename, "^/dev/fd/[012]$") then
            if not self.skip_precompiled then
              local lua_filename = gsub(filename, "%.nom$", ".lua")
              local file = FILE_CACHE[lua_filename]
              if file then
                ret = self:run_lua(Lua(Source(filename, 1, #file), file))
                remove(_running_files)
                _continue_0 = true
                break
              end
            end
            local file = file or FILE_CACHE[filename]
            if not file then
              error("File does not exist: " .. tostring(filename), 0)
            end
            ret = self:run(Nomsu(Source(filename, 1, #file), file), compile_fn)
          else
            error("Invalid filetype for " .. tostring(filename), 0)
          end
          loaded[filename] = ret or true
          remove(_running_files)
          _continue_0 = true
        until true
        if not _continue_0 then
          break
        end
      end
      loaded[filename] = ret or true
      return ret
    end,
    run_lua = function(self, lua)
      assert(type(lua) ~= 'string', "Attempt to run lua string instead of Lua (object)")
      local lua_string = tostring(lua)
      local run_lua_fn, err = load(lua_string, tostring(lua.source), "t", self.environment)
      if not run_lua_fn then
        local n = 1
        local fn
        fn = function()
          n = n + 1
          return ("\n%-3d|"):format(n)
        end
        local line_numbered_lua = "1  |" .. lua_string:gsub("\n", fn)
        error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(colored.onblack(line_numbered_lua)))) .. "\n\n" .. tostring(err), 0)
      end
      local source_key = tostring(lua.source)
      if not (self.source_map[source_key]) then
        local map = { }
        local offset = 1
        local source = lua.source
        local nomsu_str = tostring(FILE_CACHE[source.filename]:sub(source.start, source.stop))
        local lua_line = 1
        local nomsu_line = pos_to_line(nomsu_str, lua.source.start)
        local fn
        fn = function(s)
          if type(s) == 'string' then
            for nl in s:gmatch("\n") do
              map[lua_line] = map[lua_line] or nomsu_line
              lua_line = lua_line + 1
            end
          else
            local old_line = nomsu_line
            if s.source then
              nomsu_line = pos_to_line(nomsu_str, s.source.start)
            end
            local _list_0 = s.bits
            for _index_0 = 1, #_list_0 do
              local b = _list_0[_index_0]
              fn(b)
            end
          end
        end
        fn(lua)
        map[lua_line] = map[lua_line] or nomsu_line
        map[0] = 0
        self.source_map[source_key] = map
      end
      return run_lua_fn()
    end,
    tree_to_lua = function(self, tree)
      local _exp_0 = tree.type
      if "Action" == _exp_0 then
        local stub = tree.stub
        local action = self.environment['A' .. string.as_lua_id(stub)]
        if action and self.environment.COMPILE_TIME[action] then
          local args
          do
            local _accum_0 = { }
            local _len_0 = 1
            for _index_0 = 1, #tree do
              local arg = tree[_index_0]
              if type(arg) ~= "string" then
                _accum_0[_len_0] = arg
                _len_0 = _len_0 + 1
              end
            end
            args = _accum_0
          end
          do
            local arg_orders = self.environment.ARG_ORDERS[stub]
            if arg_orders then
              do
                local _accum_0 = { }
                local _len_0 = 1
                for _index_0 = 1, #arg_orders do
                  local p = arg_orders[_index_0]
                  _accum_0[_len_0] = args[p]
                  _len_0 = _len_0 + 1
                end
                args = _accum_0
              end
            end
          end
          local ret = action(tree, unpack(args))
          if not ret then
            compile_error(tree, "Compile-time action:\n%s\nfailed to produce any Lua")
          end
          return ret
        end
        local lua = Lua.Value(tree.source)
        if not action and math_expression:match(stub) then
          for i, tok in ipairs(tree) do
            if type(tok) == 'string' then
              lua:append(tok)
            else
              local tok_lua = self:tree_to_lua(tok)
              if not (tok_lua.is_value) then
                compile_error(tok, "Non-expression value inside math expression:\n%s")
              end
              if tok.type == "Action" then
                tok_lua:parenthesize()
              end
              lua:append(tok_lua)
            end
            if i < #tree then
              lua:append(" ")
            end
          end
          return lua
        end
        local args = { }
        for i, tok in ipairs(tree) do
          local _continue_0 = false
          repeat
            if type(tok) == "string" then
              _continue_0 = true
              break
            end
            local arg_lua = self:tree_to_lua(tok)
            if not (arg_lua.is_value) then
              compile_error(tok, "Cannot use:\n%s\nas an argument to %s, since it's not an expression, it produces: %s", stub, repr(arg_lua))
            end
            insert(args, arg_lua)
            _continue_0 = true
          until true
          if not _continue_0 then
            break
          end
        end
        if action then
          do
            local arg_orders = self.environment.ARG_ORDERS[stub]
            if arg_orders then
              do
                local _accum_0 = { }
                local _len_0 = 1
                for _index_0 = 1, #arg_orders do
                  local p = arg_orders[_index_0]
                  _accum_0[_len_0] = args[p]
                  _len_0 = _len_0 + 1
                end
                args = _accum_0
              end
            end
          end
        end
        lua:append("A", string.as_lua_id(stub), "(")
        for i, arg in ipairs(args) do
          lua:append(arg)
          if i < #args then
            lua:append(", ")
          end
        end
        lua:append(")")
        return lua
      elseif "EscapedNomsu" == _exp_0 then
        local make_tree
        make_tree = function(t)
          if not (AST.is_syntax_tree(t)) then
            return repr(t)
          end
          local bits
          do
            local _accum_0 = { }
            local _len_0 = 1
            for _index_0 = 1, #t do
              local bit = t[_index_0]
              _accum_0[_len_0] = make_tree(bit)
              _len_0 = _len_0 + 1
            end
            bits = _accum_0
          end
          return t.type .. "(" .. repr(tostring(t.source)) .. ", " .. table.concat(bits, ", ") .. ")"
        end
        return Lua.Value(tree.source, make_tree(tree[1]))
      elseif "Block" == _exp_0 then
        local lua = Lua(tree.source)
        for i, line in ipairs(tree) do
          local line_lua = self:tree_to_lua(line)
          if i > 1 then
            lua:append("\n")
          end
          lua:append(line_lua:as_statements())
        end
        return lua
      elseif "Text" == _exp_0 then
        local lua = Lua.Value(tree.source)
        local string_buffer = ""
        for i, bit in ipairs(tree) do
          local _continue_0 = false
          repeat
            if type(bit) == "string" then
              string_buffer = string_buffer .. bit
              _continue_0 = true
              break
            end
            if string_buffer ~= "" then
              if #lua.bits > 0 then
                lua:append("..")
              end
              lua:append(repr(string_buffer))
              string_buffer = ""
            end
            local bit_lua = self:tree_to_lua(bit)
            if not (bit_lua.is_value) then
              local src = '    ' .. gsub(tostring(self:tree_to_nomsu(bit)), '\n', '\n    ')
              local line = tostring(bit.source.filename) .. ":" .. tostring(pos_to_line(FILE_CACHE[bit.source.filename], bit.source.start))
              compile_error(bit, "Cannot use:\n%s\nas a string interpolation value, since it's not an expression.")
            end
            if #lua.bits > 0 then
              lua:append("..")
            end
            if bit.type ~= "Text" then
              bit_lua = Lua.Value(bit.source, "stringify(", bit_lua, ")")
            end
            lua:append(bit_lua)
            _continue_0 = true
          until true
          if not _continue_0 then
            break
          end
        end
        if string_buffer ~= "" or #lua.bits == 0 then
          if #lua.bits > 0 then
            lua:append("..")
          end
          lua:append(repr(string_buffer))
        end
        if #lua.bits > 1 then
          lua:parenthesize()
        end
        return lua
      elseif "List" == _exp_0 then
        local lua = Lua.Value(tree.source, "list{")
        local line_length = 0
        for i, item in ipairs(tree) do
          local item_lua = self:tree_to_lua(item)
          if not (item_lua.is_value) then
            compile_error(item, "Cannot use:\n%s\nas a list item, since it's not an expression.")
          end
          lua:append(item_lua)
          local item_string = tostring(item_lua)
          local last_line = match(item_string, "[^\n]*$")
          if match(item_string, "\n") then
            line_length = #last_line
          else
            line_length = line_length + #last_line
          end
          if i < #tree then
            if line_length >= MAX_LINE then
              lua:append(",\n  ")
              line_length = 0
            else
              lua:append(", ")
              line_length = line_length + 2
            end
          end
        end
        lua:append("}")
        return lua
      elseif "Dict" == _exp_0 then
        local lua = Lua.Value(tree.source, "dict{")
        local line_length = 0
        for i, entry in ipairs(tree) do
          local entry_lua = self:tree_to_lua(entry)
          lua:append(entry_lua)
          local entry_lua_str = tostring(entry_lua)
          local last_line = match(entry_lua_str, "\n([^\n]*)$")
          if last_line then
            line_length = #last_line
          else
            line_length = line_length + #entry_lua_str
          end
          if i < #tree then
            if line_length >= MAX_LINE then
              lua:append(",\n  ")
              line_length = 0
            else
              lua:append(", ")
              line_length = line_length + 2
            end
          end
        end
        lua:append("}")
        return lua
      elseif "DictEntry" == _exp_0 then
        local key, value = tree[1], tree[2]
        local key_lua = self:tree_to_lua(key)
        if not (key_lua.is_value) then
          compile_error(tree[1], "Cannot use:\n%s\nas a dict key, since it's not an expression.")
        end
        local value_lua = value and self:tree_to_lua(value) or Lua.Value(key.source, "true")
        if not (value_lua.is_value) then
          compile_error(tree[2], "Cannot use:\n%s\nas a dict value, since it's not an expression.")
        end
        local key_str = match(tostring(key_lua), [=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
        if key_str then
          return Lua(tree.source, key_str, "=", value_lua)
        elseif sub(tostring(key_lua), 1, 1) == "[" then
          return Lua(tree.source, "[ ", key_lua, "]=", value_lua)
        else
          return Lua(tree.source, "[", key_lua, "]=", value_lua)
        end
      elseif "IndexChain" == _exp_0 then
        local lua = self:tree_to_lua(tree[1])
        if not (lua.is_value) then
          compile_error(tree[1], "Cannot index:\n%s\nsince it's not an expression.")
        end
        local first_char = sub(tostring(lua), 1, 1)
        if first_char == "{" or first_char == '"' or first_char == "[" then
          lua:parenthesize()
        end
        for i = 2, #tree do
          local key = tree[i]
          local key_lua = self:tree_to_lua(key)
          if not (key_lua.is_value) then
            compile_error(key, "Cannot use:\n%s\nas an index, since it's not an expression.")
          end
          local key_lua_str = tostring(key_lua)
          do
            local lua_id = match(key_lua_str, "^['\"]([a-zA-Z_][a-zA-Z0-9_]*)['\"]$")
            if lua_id then
              lua:append("." .. tostring(lua_id))
            elseif sub(key_lua_str, 1, 1) == '[' then
              lua:append("[ ", key_lua, " ]")
            else
              lua:append("[", key_lua, "]")
            end
          end
        end
        return lua
      elseif "Number" == _exp_0 then
        return Lua.Value(tree.source, tostring(tree[1]))
      elseif "Var" == _exp_0 then
        return Lua.Value(tree.source, string.as_lua_id(tree[1]))
      else
        return error("Unknown type: " .. tostring(tree.type))
      end
    end,
    tree_to_nomsu = function(self, tree, inline, can_use_colon)
      if inline == nil then
        inline = false
      end
      if can_use_colon == nil then
        can_use_colon = false
      end
      local _exp_0 = tree.type
      if "Action" == _exp_0 then
        if inline then
          local nomsu = Nomsu(tree.source)
          for i, bit in ipairs(tree) do
            if type(bit) == "string" then
              if i > 1 then
                nomsu:append(" ")
              end
              nomsu:append(bit)
            else
              local arg_nomsu = self:tree_to_nomsu(bit, true)
              if not (arg_nomsu) then
                return nil
              end
              if not (i == 1) then
                nomsu:append(" ")
              end
              if bit.type == "Action" or bit.type == "Block" then
                arg_nomsu:parenthesize()
              end
              nomsu:append(arg_nomsu)
            end
          end
          return nomsu
        else
          local nomsu = Nomsu(tree.source)
          local next_space = ""
          local line_len, last_colon = 0, nil
          for i, bit in ipairs(tree) do
            if type(bit) == "string" then
              line_len = line_len + #next_space + #bit
              nomsu:append(next_space, bit)
              next_space = " "
            else
              local arg_nomsu
              if last_colon == i - 1 and bit.type == "Action" then
                arg_nomsu = nil
              elseif bit.type == "Block" then
                arg_nomsu = nil
              else
                arg_nomsu = self:tree_to_nomsu(bit, true)
              end
              if arg_nomsu and line_len + #tostring(arg_nomsu) < MAX_LINE then
                if bit.type == "Action" then
                  if can_use_colon and i > 1 then
                    nomsu:append(match(next_space, "[^ ]*"), ": ", arg_nomsu)
                    next_space = "\n.."
                    line_len = 2
                    last_colon = i
                  else
                    nomsu:append(next_space, "(", arg_nomsu, ")")
                    line_len = line_len + #next_space + 2 + #tostring(arg_nomsu)
                    next_space = " "
                  end
                else
                  nomsu:append(next_space, arg_nomsu)
                  line_len = line_len + #next_space + #tostring(arg_nomsu)
                  next_space = " "
                end
              else
                arg_nomsu = self:tree_to_nomsu(bit, nil, true)
                if not (nomsu) then
                  return nil
                end
                if bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
                  if i == 1 then
                    arg_nomsu = Nomsu(bit.source, "(..)\n    ", arg_nomsu)
                  else
                    arg_nomsu = Nomsu(bit.source, "\n    ", arg_nomsu)
                  end
                end
                if last_colon == i - 1 and (bit.type == "Action" or bit.type == "Block") then
                  next_space = ""
                end
                nomsu:append(next_space, arg_nomsu)
                next_space = "\n.."
                line_len = 2
              end
              if next_space == " " and #(match(tostring(nomsu), "[^\n]*$")) > MAX_LINE then
                next_space = "\n.."
              end
            end
          end
          return nomsu
        end
      elseif "EscapedNomsu" == _exp_0 then
        local nomsu = self:tree_to_nomsu(tree[1], true)
        if nomsu == nil and not inline then
          nomsu = self:tree_to_nomsu(tree[1])
          return nomsu and Nomsu(tree.source, "\\:\n    ", nomsu)
        end
        return nomsu and Nomsu(tree.source, "\\(", nomsu, ")")
      elseif "Block" == _exp_0 then
        if inline then
          local nomsu = Nomsu(tree.source)
          for i, line in ipairs(tree) do
            if i > 1 then
              nomsu:append("; ")
            end
            local line_nomsu = self:tree_to_nomsu(line, true)
            if not (line_nomsu) then
              return nil
            end
            nomsu:append(line_nomsu)
          end
          return nomsu
        end
        local nomsu = Nomsu(tree.source)
        for i, line in ipairs(tree) do
          line = assert(self:tree_to_nomsu(line, nil, true), "Could not convert line to nomsu")
          nomsu:append(line)
          if i < #tree then
            nomsu:append("\n")
            if match(tostring(line), "\n") then
              nomsu:append("\n")
            end
          end
        end
        return nomsu
      elseif "Text" == _exp_0 then
        if inline then
          local nomsu = Nomsu(tree.source, '"')
          for _index_0 = 1, #tree do
            local bit = tree[_index_0]
            if type(bit) == 'string' then
              nomsu:append((gsub(gsub(gsub(bit, "\\", "\\\\"), "\n", "\\n"), '"', '\\"')))
            else
              local interp_nomsu = self:tree_to_nomsu(bit, true)
              if interp_nomsu then
                if bit.type ~= "Var" and bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
                  interp_nomsu:parenthesize()
                end
                nomsu:append("\\", interp_nomsu)
              else
                return nil
              end
            end
          end
          nomsu:append('"')
          return nomsu
        else
          local inline_version = self:tree_to_nomsu(tree, true)
          if inline_version and #inline_version <= MAX_LINE then
            return inline_version
          end
          local nomsu = Nomsu(tree.source, '".."\n    ')
          for i, bit in ipairs(tree) do
            if type(bit) == 'string' then
              local bit_lines = get_lines:match(bit)
              for j, line in ipairs(bit_lines) do
                if j > 1 then
                  nomsu:append("\n    ")
                end
                if #line > 1.25 * MAX_LINE then
                  local remainder = line
                  while #remainder > 0 do
                    local split = find(remainder, " ", MAX_LINE, true)
                    if split then
                      local chunk
                      chunk, remainder = sub(remainder, 1, split), sub(remainder, split + 1, -1)
                      nomsu:append(chunk)
                    elseif #remainder > 1.75 * MAX_LINE then
                      split = math.floor(1.5 * MAX_LINE)
                      local chunk
                      chunk, remainder = sub(remainder, 1, split), sub(remainder, split + 1, -1)
                      nomsu:append(chunk)
                    else
                      nomsu:append(remainder)
                      break
                    end
                    if #remainder > 0 then
                      nomsu:append("\\\n    ..")
                    end
                  end
                else
                  nomsu:append(line)
                end
              end
            else
              local interp_nomsu = self:tree_to_nomsu(bit, true)
              if interp_nomsu then
                if bit.type ~= "Var" and bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
                  interp_nomsu:parenthesize()
                end
                nomsu:append("\\", interp_nomsu)
              else
                interp_nomsu = assert(self:tree_to_nomsu(bit))
                if not (interp_nomsu) then
                  return nil
                end
                nomsu:append("\\\n        ", interp_nomsu)
                if i < #tree then
                  nomsu:append("\n    ..")
                end
              end
            end
          end
          return nomsu
        end
      elseif "List" == _exp_0 then
        if inline then
          local nomsu = Nomsu(tree.source, "[")
          for i, item in ipairs(tree) do
            local item_nomsu = self:tree_to_nomsu(item, true)
            if not (item_nomsu) then
              return nil
            end
            if i > 1 then
              nomsu:append(", ")
            end
            nomsu:append(item_nomsu)
          end
          nomsu:append("]")
          return nomsu
        else
          local inline_version = self:tree_to_nomsu(tree, true)
          if inline_version and #inline_version <= MAX_LINE then
            return inline_version
          end
          local nomsu = Nomsu(tree.source, "[..]")
          local line = Nomsu(tree.source, "\n    ")
          for _index_0 = 1, #tree do
            local item = tree[_index_0]
            local item_nomsu = self:tree_to_nomsu(item, true)
            if item_nomsu and #line + #", " + #item_nomsu <= MAX_LINE then
              if #line.bits > 1 then
                line:append(", ")
              end
              line:append(item_nomsu)
            else
              if not (item_nomsu) then
                item_nomsu = self:tree_to_nomsu(item)
                if not (item_nomsu) then
                  return nil
                end
              end
              if #line.bits > 1 then
                nomsu:append(line)
                line = Nomsu(line.source, "\n    ")
              end
              line:append(item_nomsu)
            end
          end
          if #line.bits > 1 then
            nomsu:append(line)
          end
          return nomsu
        end
      elseif "Dict" == _exp_0 then
        if inline then
          local nomsu = Nomsu(tree.source, "{")
          for i, entry in ipairs(tree) do
            local entry_nomsu = self:tree_to_nomsu(entry, true)
            if not (entry_nomsu) then
              return nil
            end
            if i > 1 then
              nomsu:append(", ")
            end
            nomsu:append(entry_nomsu)
          end
          nomsu:append("}")
          return nomsu
        else
          local inline_version = self:tree_to_nomsu(tree, true)
          if inline_version then
            return inline_version
          end
          local nomsu = Nomsu(tree.source, "{..}")
          local line = Nomsu(tree.source, "\n    ")
          for _index_0 = 1, #tree do
            local entry = tree[_index_0]
            local entry_nomsu = self:tree_to_nomsu(entry)
            if not (entry_nomsu) then
              return nil
            end
            if #line + #tostring(entry_nomsu) <= MAX_LINE then
              if #line.bits > 1 then
                line:append(", ")
              end
              line:append(entry_nomsu)
            else
              if #line.bits > 1 then
                nomsu:append(line)
                line = Nomsu(line.source, "\n    ")
              end
              line:append(entry_nomsu)
            end
          end
          if #line.bits > 1 then
            nomsu:append(line)
          end
          return nomsu
        end
      elseif "DictEntry" == _exp_0 then
        local key, value = tree[1], tree[2]
        local key_nomsu = self:tree_to_nomsu(key, true)
        if not (key_nomsu) then
          return nil
        end
        if key.type == "Action" or key.type == "Block" then
          key_nomsu:parenthesize()
        end
        local value_nomsu
        if value then
          value_nomsu = self:tree_to_nomsu(value, true)
        else
          value_nomsu = Nomsu(tree.source, "")
        end
        if inline and not value_nomsu then
          return nil
        end
        if not value_nomsu then
          if inline then
            return nil
          end
          value_nomsu = self:tree_to_nomsu(value)
          if not (value_nomsu) then
            return nil
          end
        end
        return Nomsu(tree.source, key_nomsu, ":", value_nomsu)
      elseif "IndexChain" == _exp_0 then
        local nomsu = Nomsu(tree.source)
        for i, bit in ipairs(tree) do
          if i > 1 then
            nomsu:append(".")
          end
          local bit_nomsu = self:tree_to_nomsu(bit, true)
          if not (bit_nomsu) then
            return nil
          end
          if bit.type == "Action" or bit.type == "Block" then
            bit_nomsu:parenthesize()
          end
          nomsu:append(bit_nomsu)
        end
        return nomsu
      elseif "Number" == _exp_0 then
        return Nomsu(tree.source, tostring(tree[1]))
      elseif "Var" == _exp_0 then
        return Nomsu(tree.source, "%", tree[1])
      else
        return error("Unknown type: " .. tostring(tree.type))
      end
    end,
    initialize_core = function(self)
      local nomsu = self
      do
        local _with_0 = nomsu.environment
        _with_0.A_immediately_1 = _with_0.compile_time(function(self, _block)
          local lua = nomsu:tree_to_lua(_block):as_statements()
          lua:declare_locals()
          nomsu:run_lua(lua)
          return Lua(_block.source, "if IMMEDIATE then\n    ", lua, "\nend")
        end)
        local add_lua_string_bits
        add_lua_string_bits = function(lua, code)
          local line_len = 0
          if code.type ~= "Text" then
            lua:append(", ", nomsu:tree_to_lua(code))
            return 
          end
          for _index_0 = 1, #code do
            local bit = code[_index_0]
            local bit_lua
            if type(bit) == "string" then
              bit_lua = repr(bit)
            else
              bit_lua = nomsu:tree_to_lua(bit)
              if not (bit_lua.is_value) then
                compile_error(bit, "Cannot use:\n%s\nas a string interpolation value, since it's not an expression.")
              end
              bit_lua = bit_lua
            end
            line_len = line_len + #tostring(bit_lua)
            if line_len > MAX_LINE then
              lua:append(",\n    ")
              line_len = 4
            else
              lua:append(", ")
            end
            lua:append(bit_lua)
          end
        end
        _with_0.A_Lua_1 = _with_0.compile_time(function(self, _code)
          local lua = Lua.Value(_code.source, "Lua(", repr(tostring(_code.source)))
          add_lua_string_bits(lua, _code)
          lua:append(")")
          return lua
        end)
        _with_0.A_Lua_value_1 = _with_0.compile_time(function(self, _code)
          local lua = Lua.Value(_code.source, "Lua.Value(", repr(tostring(_code.source)))
          add_lua_string_bits(lua, _code)
          lua:append(")")
          return lua
        end)
        local add_lua_bits
        add_lua_bits = function(lua, code)
          for _index_0 = 1, #code do
            local bit = code[_index_0]
            if type(bit) == "string" then
              lua:append(bit)
            else
              local bit_lua = nomsu:tree_to_lua(bit)
              if not (bit_lua.is_value) then
                compile_error(bit, "Cannot use:\n%s\nas a string interpolation value, since it's not an expression.")
              end
              lua:append(bit_lua)
            end
          end
          return lua
        end
        nomsu.environment["A" .. string.as_lua_id("lua > 1")] = _with_0.compile_time(function(self, _code)
          if _code.type ~= "Text" then
            return Lua(self.source, "nomsu:run_lua(", nomsu:tree_to_lua(_code), ");")
          end
          return add_lua_bits(Lua(self.source), _code)
        end)
        nomsu.environment["A" .. string.as_lua_id("= lua 1")] = _with_0.compile_time(function(self, _code)
          if _code.type ~= "Text" then
            return Lua.Value(self.source, "nomsu:run_lua(", nomsu:tree_to_lua(_code), ":as_statements('return '))")
          end
          return add_lua_bits(Lua.Value(self.source), _code)
        end)
        _with_0.A_use_1 = _with_0.compile_time(function(self, _path)
          if not (_path.type == 'Text' and #_path == 1 and type(_path[1]) == 'string') then
            return Lua(_path.source, "nomsu:run_file(" .. tostring(nomsu:tree_to_lua(_path)) .. ");")
          end
          local path = _path[1]
          nomsu:run_file(path)
          return Lua(_path.source, "nomsu:run_file(" .. tostring(repr(path)) .. ");")
        end)
        return _with_0
      end
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self)
      local NaN_surrogate = { }
      local nil_surrogate = { }
      self.ids = setmetatable({ }, {
        __mode = "k",
        __index = function(self, key)
          if key == nil then
            return self[nil_surrogate]
          elseif key ~= key then
            return self[NaN_surrogate]
          end
          local id = new_uuid()
          self[key] = id
          return id
        end
      })
      self.source_map = { }
      local _list_mt = {
        __eq = utils.equivalent,
        __tostring = function(self)
          return "[" .. concat((function()
            local _accum_0 = { }
            local _len_0 = 1
            for _index_0 = 1, #self do
              local b = self[_index_0]
              _accum_0[_len_0] = repr(b)
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)(), ", ") .. "]"
        end
      }
      local list
      list = function(t)
        return setmetatable(t, _list_mt)
      end
      local _dict_mt = {
        __eq = utils.equivalent,
        __tostring = function(self)
          return "{" .. concat((function()
            local _accum_0 = { }
            local _len_0 = 1
            for k, v in pairs(self) do
              _accum_0[_len_0] = tostring(repr(k)) .. ": " .. tostring(repr(v))
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)(), ", ") .. "}"
        end
      }
      local dict
      dict = function(t)
        return setmetatable(t, _dict_mt)
      end
      self.environment = {
        nomsu = self,
        repr = repr,
        stringify = stringify,
        utils = utils,
        lpeg = lpeg,
        re = re,
        compile_error = compile_error,
        next = next,
        unpack = unpack,
        setmetatable = setmetatable,
        coroutine = coroutine,
        rawequal = rawequal,
        getmetatable = getmetatable,
        pcall = pcall,
        error = error,
        package = package,
        os = os,
        require = require,
        tonumber = tonumber,
        tostring = tostring,
        string = string,
        xpcall = xpcall,
        module = module,
        print = print,
        loadfile = loadfile,
        rawset = rawset,
        _VERSION = _VERSION,
        collectgarbage = collectgarbage,
        rawget = rawget,
        bit32 = bit32,
        rawlen = rawlen,
        table = table,
        assert = assert,
        dofile = dofile,
        loadstring = loadstring,
        type = type,
        select = select,
        debug = debug,
        math = math,
        io = io,
        load = load,
        pairs = pairs,
        ipairs = ipairs,
        list = list,
        dict = dict
      }
      for k, v in pairs(AST) do
        self.environment[k] = v
      end
      self.environment.Lua = Lua
      self.environment.Nomsu = Nomsu
      self.environment.Source = Source
      self.environment.ARG_ORDERS = setmetatable({ }, {
        __mode = "k"
      })
      self.environment.ALIASES = setmetatable({ }, {
        __mode = "k"
      })
      self.environment.compile_time = function(fn)
        self.environment.COMPILE_TIME[fn] = true
        return fn
      end
      self.environment.COMPILE_TIME = { }
      self.environment.LOADED = { }
      self.environment.AST = AST
      self.environment._ENV = self.environment
      setmetatable(self.environment, {
        __index = function(self, k)
          do
            local _self = rawget(self, "self")
            if _self then
              return _self[k]
            end
          end
        end
      })
      return self:initialize_core()
    end,
    __base = _base_0,
    __name = "NomsuCompiler"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  local self = _class_0
  compile_error = function(tok, err_format_string, ...)
    local file = FILE_CACHE[tok.source.filename]
    local line_no = pos_to_line(file, tok.source.start)
    local line_start = LINE_STARTS[file][line_no]
    local src = colored.dim(file:sub(line_start, tok.source.start - 1))
    src = src .. colored.underscore(colored.bright(colored.red(file:sub(tok.source.start, tok.source.stop - 1))))
    local end_of_line = (LINE_STARTS[file][pos_to_line(file, tok.source.stop) + 1] or 0) - 1
    src = src .. colored.dim(file:sub(tok.source.stop, end_of_line - 1))
    src = '    ' .. src:gsub('\n', '\n    ')
    local err_msg = err_format_string:format(src, ...)
    return error(tostring(tok.source.filename) .. ":" .. tostring(line_no) .. ": " .. err_msg, 0)
  end
  _running_files = { }
  MAX_LINE = 80
  math_expression = re.compile([[ ([+-] " ")* "%" (" " [*/^+-] (" " [+-])* " %")+ !. ]])
  NomsuCompiler = _class_0
end
if arg and debug_getinfo(2).func ~= require then
  colors = require('consolecolors')
  local parser = re.compile([[        args <- {| (flag ";")* {:inputs: {| ({file} ";")* |} :} {:nomsu_args: {| ("--;" ({[^;]*} ";")*)? |} :} ";"? |} !.
        flag <-
            {:interactive: ("-i" -> true) :}
          / {:optimized: ("-O" -> true) :}
          / {:format: ("-f" -> true) :}
          / {:syntax: ("-s" -> true) :}
          / {:print_file: "-p" ";" {file} :}
          / {:output_file: "-o" ";" {file} :}
          / {:help: (("-h" / "--help") -> true) :}
        file <- "-" / [^;]+
    ]], {
    ["true"] = function()
      return true
    end
  })
  local args = concat(arg, ";") .. ";"
  args = parser:match(args)
  if not args or args.help then
    print([=[Nomsu Compiler

Usage: (lua nomsu.lua | moon nomsu.moon) [-i] [-O] [-f] [-s] [--help] [-o output] [-p print_file] file1 file2... [-- nomsu args...]

OPTIONS
    -i Run the compiler in interactive mode (REPL)
    -O Run the compiler in optimized mode (use precompiled .lua versions of Nomsu files, when available)
    -f Auto-format the given Nomsu file and print the result.
    -s Check the program for syntax errors.
    -h/--help Print this message.
    -o <file> Output the compiled Lua file to the given file (use "-" to output to stdout; if outputting to stdout and -p is not specified, -p will default to /dev/null)
    -p <file> Print to the specified file instead of stdout.
    <input> Input file can be "-" to use stdin.
]=])
    os.exit()
  end
  local nomsu = NomsuCompiler()
  nomsu.environment.arg = args.nomsu_args
  local ok, to_lua = pcall(function()
    return require('moonscript.base').to_lua
  end)
  if not ok then
    to_lua = nil
  end
  local moonscript_line_tables = setmetatable({ }, {
    __index = function(self, filename)
      if not (to_lua) then
        return nil
      end
      local _, line_table = to_lua(FILE_CACHE[filename])
      self[filename] = line_table
      return line_table
    end
  })
  debug.getinfo = function(thread, f, what)
    if what == nil then
      f, what, thread = thread, f, nil
    end
    if type(f) == 'number' then
      f = f + 1
    end
    local info
    if thread == nil then
      info = debug_getinfo(f, what)
    else
      info = debug_getinfo(thread, f, what)
    end
    if not info or not info.func then
      return info
    end
    if info.short_src or info.source or info.linedefine or info.currentline then
      do
        local arg_orders = nomsu.environment.ARG_ORDERS[info.func]
        if arg_orders then
          info.name = next(arg_orders)
        end
      end
      do
        local map = nomsu.source_map[info.source]
        if map then
          if info.currentline then
            info.currentline = assert(map[info.currentline])
          end
          if info.linedefined then
            info.linedefined = assert(map[info.linedefined])
          end
          if info.lastlinedefined then
            info.lastlinedefined = assert(map[info.lastlinedefined])
          end
        end
      end
    end
    return info
  end
  local print_err_msg
  print_err_msg = function(error_message, stack_offset)
    if stack_offset == nil then
      stack_offset = 3
    end
    io.stderr:write(tostring(colored.red("ERROR:")) .. " " .. tostring(colored.bright(colored.red((error_message or "")))) .. "\n")
    io.stderr:write("stack traceback:\n")
    ok, to_lua = pcall(function()
      return require('moonscript.base').to_lua
    end)
    if not ok then
      to_lua = function()
        return nil
      end
    end
    local nomsu_source = FILE_CACHE["nomsu.moon"]
    local LINE_TABLES = setmetatable({ }, {
      __index = function(self, file)
        local _, line_table = to_lua(file)
        self[file] = line_table or false
        return line_table or false
      end
    })
    local get_line
    get_line = function(file, line_no)
      local start = LINE_STARTS[file][line_no] or 1
      local stop = (LINE_STARTS[file][line_no + 1] or 0) - 1
      return file:sub(start, stop)
    end
    local level = stack_offset
    while true do
      local _continue_0 = false
      repeat
        local calling_fn = debug_getinfo(level)
        if not calling_fn then
          break
        end
        if calling_fn.func == run then
          break
        end
        level = level + 1
        local name = calling_fn.name and "function '" .. tostring(calling_fn.name) .. "'" or nil
        if calling_fn.linedefined == 0 then
          name = "main chunk"
        end
        if name == "run_lua_fn" then
          _continue_0 = true
          break
        end
        local line = nil
        do
          local map = nomsu.source_map[calling_fn.source]
          if map then
            if calling_fn.currentline then
              calling_fn.currentline = assert(map[calling_fn.currentline])
            end
            if calling_fn.linedefined then
              calling_fn.linedefined = assert(map[calling_fn.linedefined])
            end
            if calling_fn.lastlinedefined then
              calling_fn.lastlinedefined = assert(map[calling_fn.lastlinedefined])
            end
            local filename, start, stop = calling_fn.source:match('@([^[]*)%[([0-9]+):([0-9]+)]')
            assert(filename)
            local file = FILE_CACHE[filename]:sub(tonumber(start), tonumber(stop))
            local err_line = get_line(file, calling_fn.currentline):sub(1, -2)
            local offending_statement = colored.bright(colored.red(err_line:match("^[ ]*(.*)")))
            do
              local arg_orders = nomsu.environment.ARG_ORDERS[calling_fn.func]
              if arg_orders then
                name = "action '" .. tostring(next(arg_orders)) .. "'"
              else
                name = "main chunk"
              end
            end
            line = colored.yellow(tostring(filename) .. ":" .. tostring(calling_fn.currentline) .. " in " .. tostring(name) .. "\n        " .. tostring(offending_statement))
          else
            local file
            ok, file = pcall(function()
              return FILE_CACHE[calling_fn.short_src]
            end)
            if not ok then
              file = nil
            end
            local line_num
            if name == nil then
              local search_level = level
              local _info = debug.getinfo(search_level)
              while _info and (_info.func == pcall or _info.func == xpcall) do
                search_level = search_level + 1
                _info = debug.getinfo(search_level)
              end
              if _info then
                for i = 1, 999 do
                  local varname, val = debug.getlocal(search_level, i)
                  if not varname then
                    break
                  end
                  if val == calling_fn.func then
                    name = "local '" .. tostring(varname) .. "'"
                    if not varname:match("%(") then
                      break
                    end
                  end
                end
                if not (name) then
                  for i = 1, _info.nups do
                    local varname, val = debug.getupvalue(_info.func, i)
                    if not varname then
                      break
                    end
                    if val == calling_fn.func then
                      name = "upvalue '" .. tostring(varname) .. "'"
                      if not varname:match("%(") then
                        break
                      end
                    end
                  end
                end
              end
            end
            if file and calling_fn.short_src:match(".moon$") and LINE_TABLES[file] then
              local char = LINE_TABLES[file][calling_fn.currentline]
              line_num = 1
              for _ in file:sub(1, char):gmatch("\n") do
                line_num = line_num + 1
              end
              line = colored.cyan(tostring(calling_fn.short_src) .. ":" .. tostring(line_num) .. " in " .. tostring(name or '?'))
            else
              line_num = calling_fn.currentline
              if calling_fn.short_src == '[C]' then
                line = colored.green(tostring(calling_fn.short_src) .. " in " .. tostring(name or '?'))
              else
                line = colored.blue(tostring(calling_fn.short_src) .. ":" .. tostring(calling_fn.currentline) .. " in " .. tostring(name or '?'))
              end
            end
            if file then
              local err_line = get_line(file, line_num):sub(1, -2)
              local offending_statement = colored.bright(colored.red(err_line:match("^[ ]*(.*)$")))
              line = line .. ("\n        " .. offending_statement)
            end
          end
        end
        io.stderr:write("    " .. tostring(line) .. "\n")
        if calling_fn.istailcall then
          io.stderr:write("    " .. tostring(colored.dim(colored.white("  (...tail calls...)"))) .. "\n")
        end
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
    return io.stderr:flush()
  end
  local run
  run = function()
    for i, input in ipairs(args.inputs) do
      if input == "-" then
        args.inputs[i] = STDIN
      end
    end
    if #args.inputs == 0 and not args.interactive then
      args.inputs = {
        "core"
      }
      args.interactive = true
    end
    local print_file
    if args.print_file == "-" then
      print_file = io.stdout
    elseif args.print_file then
      print_file = io.open(args.print_file, 'w')
    elseif args.output_file == '-' then
      print_file = nil
    else
      print_file = io.stdout
    end
    nomsu.skip_precompiled = not args.optimized
    if print_file == nil then
      nomsu.environment.print = function() end
    elseif print_file ~= io.stdout then
      nomsu.environment.print = function(...)
        local N = select("#", ...)
        if N > 0 then
          print_file:write(tostring(select(1, ...)))
          for i = 2, N do
            print_file:write('\t', tostring(select(1, ...)))
          end
        end
        print_file:write('\n')
        return print_file:flush()
      end
    end
    local compile_fn
    if args.output_file then
      compile_fn = function(code)
        local output_file
        if args.output_file == "-" then
          output_file = io.stdout
        elseif args.output_file then
          output_file = io.open(args.output_file, 'w')
        end
        output_file:write("local IMMEDIATE = true;\n" .. tostring(code))
        return output_file:flush()
      end
    else
      compile_fn = nil
    end
    local parse_errs = { }
    local _list_0 = args.inputs
    for _index_0 = 1, #_list_0 do
      local input = _list_0[_index_0]
      if args.syntax then
        for input_file in all_files(input) do
          local err
          ok, err = pcall(nomsu.parse, nomsu, Nomsu(input_file, io.open(input_file):read("*a")))
          if not ok then
            insert(parse_errs, err)
          elseif print_file then
            print_file:write("Parse succeeded: " .. tostring(input_file) .. "\n")
            print_file:flush()
          end
        end
      elseif args.format then
        for input_file in all_files(input) do
          local tree = nomsu:parse(io.open(input_file):read("*a"))
          local formatted = tostring(self:tree_to_nomsu(tree))
          if output_file then
            output_file:write(formatted, "\n")
            output_file:flush()
          end
          if print_file then
            print_file:write(formatted, "\n")
            print_file:flush()
          end
        end
      elseif input == STDIN then
        local file = io.input():read("*a")
        FILE_CACHE.stdin = file
        nomsu:run(Nomsu(Source('stdin', 1, #file), file), compile_fn)
      else
        nomsu:run_file(input, compile_fn)
      end
    end
    if #parse_errs > 0 then
      io.stderr:write(concat(parse_errs, "\n\n"))
      io.stderr:flush()
      os.exit(false, true)
    elseif args.syntax then
      os.exit(true, true)
    end
    if args.interactive then
      while true do
        io.write(colored.bright(colored.yellow(">> ")))
        local buff = ""
        while true do
          local line = io.read("*L")
          if line == "\n" or not line then
            if #buff > 0 then
              io.write("\027[1A\027[2K")
            end
            break
          end
          line = line:gsub("\t", "    ")
          buff = buff .. line
          io.write(colored.dim(colored.yellow(".. ")))
        end
        if #buff == 0 then
          break
        end
        local ret
        ok, ret = pcall(nomsu.run, nomsu, buff)
        if ok and ret ~= nil then
          print("= " .. repr(ret))
        elseif not ok then
          print_err_msg(ret)
        end
      end
    end
  end
  local err_hand
  err_hand = function(error_message)
    print_err_msg(error_message)
    return os.exit(false, true)
  end
  do
    local ldt
    ok, ldt = pcall(require, 'ldt')
    if ok then
      ldt.guard(run)
    else
      xpcall(run, err_hand)
    end
  end
end
return NomsuCompiler
