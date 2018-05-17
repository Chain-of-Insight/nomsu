local _pairs, _ipairs = pairs, ipairs
if jit then
  package.cpath = "./luajit_lpeg/?.so;" .. package.cpath
  bit32 = require('bit')
  pairs = function(x)
    do
      local mt = getmetatable(x)
      if mt then
        if mt.__pairs then
          return mt.__pairs(x)
        end
      end
    end
    return _pairs(x)
  end
  ipairs = function(x)
    do
      local mt = getmetatable(x)
      if mt then
        if mt.__ipairs then
          return mt.__ipairs(x)
        end
      end
    end
    return _ipairs(x)
  end
end
re = require('re')
lpeg = require('lpeg')
lpeg.setmaxstack(10000)
local P, R, V, S, Cg, C, Cp, B, Cmt
P, R, V, S, Cg, C, Cp, B, Cmt = lpeg.P, lpeg.R, lpeg.V, lpeg.S, lpeg.Cg, lpeg.C, lpeg.Cp, lpeg.B, lpeg.Cmt
local utils = require('utils')
local new_uuid = require('uuid')
local immutable = require('immutable')
Tuple = immutable(nil, {
  name = "Tuple"
})
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
local debug_getinfo = debug.getinfo
local Nomsu, Lua, Source
do
  local _obj_0 = require("code_obj")
  Nomsu, Lua, Source = _obj_0.Nomsu, _obj_0.Lua, _obj_0.Source
end
local STDIN, STDOUT, STDERR = "/dev/fd/0", "/dev/fd/1", "/dev/fd/2"
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
  if path:match("%.nom$") or path:match("%.lua$") or path:match("^/dev/fd/[012]$") then
    return iterate_single, path
  end
  path = path:gsub("\\", "\\\\"):gsub("`", ""):gsub('"', '\\"'):gsub("$", "")
  return io.popen("find -L \"" .. path .. "\" -type f -name \"*.nom\""):lines()
end
local line_counter = re.compile([[    lines <- {| line (%nl line)* |}
    line <- {} (!%nl .)*
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
local LUA_METADATA = { }
local lua_line_to_nomsu_line
lua_line_to_nomsu_line = function(lua_filename, lua_line_no)
  local metadata = assert(LUA_METADATA[lua_filename], "Failed to find nomsu metadata for: " .. tostring(lua_filename) .. ".")
  local lua_offset = LINE_STARTS[metadata.lua_file][lua_line_no]
  local best = metadata.nomsu_sources[1]
  for lua, nomsu in pairs(metadata.lua_to_nomsu) do
    if lua.start <= lua_offset and lua > best then
      best = lua
    end
  end
  return best:get_line_number()
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
      return string.sub(self, i, i)
    elseif type(i) == 'table' then
      return string.sub(self, i[1], i[2])
    end
  end
end
local Types = require("nomsu_tree")
local NOMSU_DEFS
do
  local _with_0 = { }
  _with_0.nl = P("\r") ^ -1 * P("\n")
  _with_0.ws = S(" \t")
  _with_0.tonumber = tonumber
  _with_0.print = function(src, pos, msg)
    print(msg, pos, repr(src:sub(math.max(0, pos - 16), math.max(0, pos - 1)) .. "|" .. src:sub(pos, pos + 16)))
    return true
  end
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
  _with_0.operator = _with_0.operator_char ^ 1
  _with_0.utf8_char = (R("\194\223") * R("\128\191") + R("\224\239") * R("\128\191") * R("\128\191") + R("\240\244") * R("\128\191") * R("\128\191") * R("\128\191"))
  _with_0.ident_char = R("az", "AZ", "09") + P("_") + _with_0.utf8_char
  _with_0.indent = P(function(self, start)
    local nodent = lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack]
    local indented = nodent .. "    "
    if self:sub(start, start + #indented - 1) == indented then
      insert(lpeg.userdata.indent_stack, indented)
      return start + #indented
    end
  end)
  _with_0.dedent = P(function(self, start)
    local nodent = lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack]
    local spaces = self:match("[ ]*", start)
    if #spaces <= #nodent - 4 then
      remove(lpeg.userdata.indent_stack)
      return start
    end
  end)
  _with_0.nodent = P(function(self, start)
    local nodent = lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack]
    if self:sub(start, start + #nodent - 1) == nodent then
      return start + #nodent
    end
  end)
  _with_0.error = function(src, end_pos, start_pos, err_msg)
    local seen_errors = lpeg.userdata.errors
    if seen_errors[start_pos] then
      return true
    end
    local err_pos = start_pos
    local text_loc = lpeg.userdata.source_code.source:sub(err_pos, err_pos)
    local line_no = text_loc:get_line_number()
    src = FILE_CACHE[text_loc.filename]
    local prev_line = line_no == 1 and "" or src:sub(LINE_STARTS[src][line_no - 1] or 1, LINE_STARTS[src][line_no] - 2)
    local err_line = src:sub(LINE_STARTS[src][line_no], (LINE_STARTS[src][line_no + 1] or 0) - 2)
    local next_line = src:sub(LINE_STARTS[src][line_no + 1] or -1, (LINE_STARTS[src][line_no + 2] or 0) - 2)
    local pointer = ("-"):rep(err_pos - LINE_STARTS[src][line_no]) .. "^"
    err_msg = (err_msg or "Parse error") .. " at " .. tostring(lpeg.userdata.source_code.source.filename) .. ":" .. tostring(line_no) .. ":\n"
    if #prev_line > 0 then
      err_msg = err_msg .. ("\n" .. prev_line)
    end
    err_msg = err_msg .. "\n" .. tostring(err_line) .. "\n" .. tostring(pointer)
    if #next_line > 0 then
      err_msg = err_msg .. ("\n" .. next_line)
    end
    seen_errors[start_pos] = err_msg
    return true
  end
  NOMSU_DEFS = _with_0
end
setmetatable(NOMSU_DEFS, {
  __index = function(self, key)
    local make_node
    make_node = function(src, ...)
      return Types[key](...)
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
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- (({} %3) -> %2)"
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    comment <- "--" [^%nl]*
    ]])
  local nomsu_peg = peg_tidier:match(FILE_CACHE["nomsu.peg"])
  NOMSU_PATTERN = re.compile(nomsu_peg, NOMSU_DEFS)
end
local NomsuCompiler
do
  local _class_0
  local stub_defs, stub_pattern, var_pattern, _nomsu_chunk_counter, _running_files, MAX_LINE, math_expression
  local _base_0 = {
    define_action = function(self, signature, fn, is_compile_action)
      if is_compile_action == nil then
        is_compile_action = false
      end
      assert(type(fn) == 'function', "Bad fn: " .. tostring(repr(fn)))
      if type(signature) == 'string' then
        signature = {
          signature
        }
      elseif type(signature) ~= 'table' then
        error("Invalid signature, expected list of strings, but got: " .. tostring(repr(signature)), 0)
      end
      local fn_info = debug_getinfo(fn, "u")
      assert(not fn_info.isvararg, "Vararg functions aren't supported. Sorry, use a list instead.")
      local fn_arg_positions
      do
        local _tbl_0 = { }
        for i = 1, fn_info.nparams do
          _tbl_0[debug.getlocal(fn, i)] = i
        end
        fn_arg_positions = _tbl_0
      end
      local arg_orders = { }
      for _index_0 = 1, #signature do
        local alias = signature[_index_0]
        local stub = assert(stub_pattern:match(alias))
        local stub_args = assert(var_pattern:match(alias));
        (is_compile_action and self.environment.COMPILE_ACTIONS or self.environment.ACTIONS)[stub] = fn
        do
          local _accum_0 = { }
          local _len_0 = 1
          for _index_1 = 1, #stub_args do
            local a = stub_args[_index_1]
            _accum_0[_len_0] = fn_arg_positions[Types.Var(a):as_lua_id()]
            _len_0 = _len_0 + 1
          end
          arg_orders[stub] = _accum_0
        end
      end
      self.environment.ARG_ORDERS[fn] = arg_orders
    end,
    define_compile_action = function(self, signature, fn)
      return self:define_action(signature, fn, true)
    end,
    parse = function(self, nomsu_code)
      if type(nomsu_code) == 'string' then
        _nomsu_chunk_counter = _nomsu_chunk_counter + 1
        local filename = "<nomsu chunk #" .. tostring(_nomsu_chunk_counter) .. ">.nom"
        FILE_CACHE[filename] = nomsu_code
        nomsu_code = Nomsu(filename, nomsu_code)
      end
      local userdata = {
        source_code = nomsu_code,
        indent_stack = {
          ""
        },
        errors = { }
      }
      local old_userdata
      old_userdata, lpeg.userdata = lpeg.userdata, userdata
      local tree = NOMSU_PATTERN:match(tostring(nomsu_code))
      lpeg.userdata = old_userdata
      assert(tree, "In file " .. tostring(colored.blue(filename)) .. " failed to parse:\n" .. tostring(colored.onyellow(colored.black(nomsu_code))))
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
        error(concat(errors, "\n\n"), 0)
      end
      return tree
    end,
    run = function(self, nomsu_code, compile_fn)
      if compile_fn == nil then
        compile_fn = nil
      end
      if #tostring(nomsu_code) == 0 then
        return nil
      end
      local tree = self:parse(nomsu_code)
      assert(tree, "Failed to parse: " .. tostring(nomsu_code))
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
          if filename:match("%.lua$") then
            local file = assert(FILE_CACHE[filename], "Could not find file: " .. tostring(filename))
            ret = self:run_lua(Lua(Source(filename), file))
          elseif filename:match("%.nom$") or filename:match("^/dev/fd/[012]$") then
            if not self.skip_precompiled then
              local lua_filename = filename:gsub("%.nom$", ".lua")
              local file = FILE_CACHE[lua_filename]
              if file then
                ret = self:run_lua(Lua(Source(filename), file))
                remove(_running_files)
                _continue_0 = true
                break
              end
            end
            local file = file or FILE_CACHE[filename]
            if not file then
              error("File does not exist: " .. tostring(filename), 0)
            end
            ret = self:run(Nomsu(Source(filename), file), compile_fn)
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
      local run_lua_fn, err = load(lua_string, filename, "t", self.environment)
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
      return run_lua_fn()
    end,
    tree_to_lua = function(self, tree)
      local _exp_0 = tree.type
      if "Action" == _exp_0 then
        local stub = tree:get_stub()
        local compile_action = self.environment.COMPILE_ACTIONS[stub]
        if compile_action then
          local args
          do
            local _accum_0 = { }
            local _len_0 = 1
            for _index_0 = 1, #tree do
              local arg = tree[_index_0]
              if arg.type ~= "Word" then
                _accum_0[_len_0] = arg
                _len_0 = _len_0 + 1
              end
            end
            args = _accum_0
          end
          do
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = self.environment.ARG_ORDERS[compile_action][stub]
            for _index_0 = 1, #_list_0 do
              local p = _list_0[_index_0]
              _accum_0[_len_0] = args[p - 1]
              _len_0 = _len_0 + 1
            end
            args = _accum_0
          end
          local ret = compile_action(tree, unpack(args))
          if not ret then
            error("Failed to produce any Lua")
          end
          return ret
        end
        local action = rawget(self.environment.ACTIONS, stub)
        local lua = Lua.Value()
        if not action and math_expression:match(stub) then
          for i, tok in ipairs(tree) do
            if tok.type == "Word" then
              lua:append(tok.value)
            else
              local tok_lua = self:tree_to_lua(tok)
              if not (tok_lua.is_value) then
                error("non-expression value inside math expression: " .. tostring(colored.yellow(repr(tok))))
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
            if tok.type == "Word" then
              _continue_0 = true
              break
            end
            local arg_lua = self:tree_to_lua(tok)
            if not (arg_lua.is_value) then
              error("Cannot use:\n" .. tostring(colored.yellow(repr(tok))) .. "\nas an argument to " .. tostring(stub) .. ", since it's not an expression, it produces: " .. tostring(repr(arg_lua)), 0)
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
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = self.environment.ARG_ORDERS[action][stub]
            for _index_0 = 1, #_list_0 do
              local p = _list_0[_index_0]
              _accum_0[_len_0] = args[p]
              _len_0 = _len_0 + 1
            end
            args = _accum_0
          end
        end
        lua:append("ACTIONS[", repr(stub), "](")
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
          if type(t) ~= 'userdata' then
            return repr(t)
          end
          if t.is_multi then
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
            return t.type .. "(" .. table.concat(bits, ", ") .. ")"
          else
            return t.type .. "(" .. make_tree(t.value) .. ")"
          end
        end
        return Lua.Value(nil, make_tree(tree.value))
      elseif "Block" == _exp_0 then
        local lua = Lua()
        for i, line in ipairs(tree) do
          local line_lua = self:tree_to_lua(line)
          if i > 1 then
            lua:append("\n")
          end
          lua:append(line_lua:as_statements())
        end
        return lua
      elseif "Text" == _exp_0 then
        local lua = Lua.Value()
        local string_buffer = ""
        for _index_0 = 1, #tree do
          local _continue_0 = false
          repeat
            local bit = tree[_index_0]
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
              error("Cannot use " .. tostring(colored.yellow(repr(bit))) .. " as a string interpolation value, since it's not an expression.", 0)
            end
            if #lua.bits > 0 then
              lua:append("..")
            end
            if bit.type ~= "Text" then
              bit_lua = Lua.Value(nil, "stringify(", bit_lua, ")")
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
        local lua = Lua.Value(nil, "{")
        local line_length = 0
        for i, item in ipairs(tree) do
          local item_lua = self:tree_to_lua(item)
          if not (item_lua.is_value) then
            error("Cannot use " .. tostring(colored.yellow(repr(item))) .. " as a list item, since it's not an expression.", 0)
          end
          lua:append(item_lua)
          local item_string = tostring(item_lua)
          local last_line = item_string:match("[^\n]*$")
          if item_string:match("\n") then
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
        local lua = Lua.Value(nil, "{")
        local line_length = 0
        for i, entry in ipairs(tree) do
          local entry_lua = self:tree_to_lua(entry)
          lua:append(entry_lua)
          local entry_lua_str = tostring(entry_lua)
          local last_line = entry_lua_str:match("\n([^\n]*)$")
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
          error("Cannot use " .. tostring(colored.yellow(repr(key))) .. " as a dict key, since it's not an expression.", 0)
        end
        local value_lua = value and self:tree_to_lua(value) or Lua.Value(nil, "true")
        if not (value_lua.is_value) then
          error("Cannot use " .. tostring(colored.yellow(repr(value))) .. " as a dict value, since it's not an expression.", 0)
        end
        local key_str = tostring(key_lua):match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
        if key_str then
          return Lua(nil, key_str, "=", value_lua)
        elseif tostring(key_lua):sub(1, 1) == "[" then
          return Lua(nil, "[ ", key_lua, "]=", value_lua)
        else
          return Lua(nil, "[", key_lua, "]=", value_lua)
        end
      elseif "IndexChain" == _exp_0 then
        local lua = self:tree_to_lua(tree[1])
        if not (lua.is_value) then
          error("Cannot index " .. tostring(colored.yellow(repr(tree[1]))) .. ", since it's not an expression.", 0)
        end
        local first_char = tostring(lua):sub(1, 1)
        if first_char == "{" or first_char == '"' or first_char == "[" then
          lua:parenthesize()
        end
        for i = 2, #tree do
          local key = tree[i]
          local key_lua = self:tree_to_lua(key)
          if not (key_lua.is_value) then
            error("Cannot use " .. tostring(colored.yellow(repr(key))) .. " as an index, since it's not an expression.", 0)
          end
          local key_lua_str = tostring(key_lua)
          do
            local lua_id = key_lua_str:match("^['\"]([a-zA-Z_][a-zA-Z0-9_]*)['\"]$")
            if lua_id then
              lua:append("." .. tostring(lua_id))
            elseif key_lua_str:sub(1, 1) == '[' then
              lua:append("[ ", key_lua, " ]")
            else
              lua:append("[", key_lua, "]")
            end
          end
        end
        return lua
      elseif "Number" == _exp_0 then
        return Lua.Value(nil, tostring(tree.value))
      elseif "Var" == _exp_0 then
        return Lua.Value(nil, tree:as_lua_id())
      elseif "Word" == _exp_0 then
        return error("Cannot convert a Word to lua")
      elseif "Comment" == _exp_0 then
        return Lua(nil, "--" .. tree.value:gsub("\n", "\n--") .. "\n")
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
          local nomsu = Nomsu()
          for i, bit in ipairs(tree) do
            if bit.type == "Word" then
              if i > 1 then
                nomsu:append(" ")
              end
              nomsu:append(bit.value)
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
          local nomsu = Nomsu()
          local next_space = ""
          local last_colon = nil
          for i, bit in ipairs(tree) do
            if bit.type == "Word" then
              nomsu:append(next_space, bit.value)
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
              if arg_nomsu and #arg_nomsu < MAX_LINE then
                if bit.type == "Action" then
                  if can_use_colon and i > 1 then
                    nomsu:append(next_space:match("[^ ]*"), ": ", arg_nomsu)
                    next_space = "\n.."
                    last_colon = i
                  else
                    nomsu:append(next_space, "(", arg_nomsu, ")")
                    next_space = " "
                  end
                else
                  nomsu:append(next_space, arg_nomsu)
                  next_space = " "
                end
              else
                arg_nomsu = self:tree_to_nomsu(bit, nil, true)
                if not (nomsu) then
                  return nil
                end
                if bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
                  if i == 1 then
                    arg_nomsu = Nomsu(nil, "(..)\n    ", arg_nomsu)
                  else
                    arg_nomsu = Nomsu(nil, "\n    ", arg_nomsu)
                  end
                end
                if last_colon == i - 1 and (bit.type == "Action" or bit.type == "Block") then
                  next_space = ""
                end
                nomsu:append(next_space, arg_nomsu)
                next_space = "\n.."
              end
              if next_space == " " and #(tostring(nomsu):match("[^\n]*$")) > MAX_LINE then
                next_space = "\n.."
              end
            end
          end
          return nomsu
        end
      elseif "EscapedNomsu" == _exp_0 then
        local nomsu = self:tree_to_nomsu(tree.value, true)
        if nomsu == nil and not inline then
          nomsu = self:tree_to_nomsu(tree.value)
          return nomsu and Nomsu(nil, "\\:\n    ", nomsu)
        end
        return nomsu and Nomsu(nil, "\\(", nomsu, ")")
      elseif "Block" == _exp_0 then
        if inline then
          local nomsu = Nomsu()
          for i, line in ipairs(self) do
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
        local nomsu = Nomsu()
        for i, line in ipairs(self) do
          line = assert(self:tree_to_nomsu(line, nil, true), "Could not convert line to nomsu")
          nomsu:append(line)
          if i < #self then
            nomsu:append("\n")
            if tostring(line):match("\n") then
              nomsu:append("\n")
            end
          end
        end
        return nomsu
      elseif "Text" == _exp_0 then
        if inline then
          local nomsu = Nomsu(nil, '"')
          for _index_0 = 1, #tree do
            local bit = tree[_index_0]
            if type(bit) == 'string' then
              nomsu:append((bit:gsub("\\", "\\\\"):gsub("\n", "\\n")))
            else
              local interp_nomsu = self:tree_to_nomsu(bit, true)
              if interp_nomsu then
                if bit.type ~= "Word" and bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
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
          local nomsu = Nomsu(nil, '".."\n    ')
          for i, bit in ipairs(self) do
            if type(bit) == 'string' then
              nomsu:append((bit:gsub("\\", "\\\\"):gsub("\n", "\n    ")))
            else
              local interp_nomsu = self:tree_to_nomsu(bit, true)
              if interp_nomsu then
                if bit.type ~= "Word" and bit.type ~= "List" and bit.type ~= "Dict" and bit.type ~= "Text" then
                  interp_nomsu:parenthesize()
                end
                nomsu:append("\\", interp_nomsu)
              else
                interp_nomsu = self:tree_to_nomsu(bit)
                if not (interp_nomsu) then
                  return nil
                end
                nomsu:append("\\\n        ", interp_nomsu)
                if i < #self then
                  nomsu:append("\n    ..")
                end
              end
            end
          end
          return nomsu
        end
      elseif "List" == _exp_0 then
        if inline then
          local nomsu = Nomsu(nil, "[")
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
          local nomsu = Nomsu(nil, "[..]")
          local line = Nomsu(nil, "\n    ")
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
                line = Nomsu(nil, "\n    ")
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
          local nomsu = Nomsu(nil, "{")
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
          local nomsu = Nomsu(nil, "{..}")
          local line = Nomsu(nil, "\n    ")
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
                line = Nomsu(nil, "\n    ")
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
          value_nomsu = Nomsu(nil, "")
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
        return Nomsu(nil, key_nomsu, ":", value_nomsu)
      elseif "IndexChain" == _exp_0 then
        local nomsu = Nomsu()
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
        return Nomsu(nil, tostring(tree.value))
      elseif "Var" == _exp_0 then
        return Nomsu(nil, "%", tree.value)
      elseif "Word" == _exp_0 then
        return Nomsu(nil, tree.value)
      elseif "Comment" == _exp_0 then
        if inline then
          return nil
        end
        return Nomsu(nil, "#", tree.value:gsub("\n", "\n    "))
      else
        return error("Unknown type: " .. tostring(tree.type))
      end
    end,
    tree_to_value = function(self, tree)
      if tree.type == 'Text' and #tree == 1 and type(tree[1]) == 'string' then
        return tree[1]
      end
      local lua = Lua(nil, "return ", self:tree_to_lua(tree), ";")
      return self:run_lua(lua)
    end,
    walk_tree = function(self, tree, depth)
      if depth == nil then
        depth = 0
      end
      coroutine.yield(tree, depth)
      if not (Types.is_node(tree)) then
        return 
      end
      if tree.is_multi then
        for _index_0 = 1, #tree do
          local v = tree[_index_0]
          self:walk_tree(v, depth + 1)
        end
      else
        return self:walk_tree(v, depth + 1)
      end
    end,
    initialize_core = function(self)
      local nomsu = self
      self:define_compile_action("immediately %block", function(self, _block)
        local lua = nomsu:tree_to_lua(_block):as_statements()
        lua:declare_locals()
        nomsu:run_lua(lua)
        return Lua(nil, "if IMMEDIATE then\n    ", lua, "\nend")
      end)
      local add_lua_string_bits
      add_lua_string_bits = function(lua, code)
        if code.type ~= "Text" then
          lua:append(", ", nomsu:tree_to_lua(code))
          return 
        end
        for _index_0 = 1, #code do
          local bit = code[_index_0]
          lua:append(", ")
          if type(bit) == "string" then
            lua:append(repr(bit))
          else
            local bit_lua = nomsu:tree_to_lua(bit)
            if not (bit_lua.is_value) then
              error("Cannot use " .. tostring(colored.yellow(repr(bit))) .. " as a string interpolation value, since it's not an expression.")
            end
            lua:append(bit_lua)
          end
        end
      end
      self:define_compile_action("Lua %code", function(self, _code)
        local lua = Lua.Value(nil, "Lua(nil")
        add_lua_string_bits(lua, _code)
        lua:append(")")
        return lua
      end)
      self:define_compile_action("Lua value %code", function(self, _code)
        local lua = Lua.Value(nil, "Lua.Value(nil")
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
              error("Cannot use " .. tostring(colored.yellow(repr(bit))) .. " as a string interpolation value, since it's not an expression.", 0)
            end
            lua:append(bit_lua)
          end
        end
        return lua
      end
      self:define_compile_action("lua> %code", function(self, _code)
        if _code.type ~= "Text" then
          return Lua(nil, "nomsu:run_lua(", nomsu:tree_to_lua(_code), ");")
        end
        return add_lua_bits(Lua(), _code)
      end)
      self:define_compile_action("=lua %code", function(self, _code)
        if _code.type ~= "Text" then
          return Lua.Value(nil, "nomsu:run_lua(", nomsu:tree_to_lua(_code), ":as_statements('return '))")
        end
        return add_lua_bits(Lua.Value(), _code)
      end)
      return self:define_compile_action("use %path", function(self, _path)
        local path = nomsu:tree_to_value(_path)
        nomsu:run_file(path)
        return Lua(nil, "nomsu:run_file(" .. tostring(repr(path)) .. ");")
      end)
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
      self.file_metadata = setmetatable({ }, {
        __mode = "k"
      })
      self.environment = {
        nomsu = self,
        repr = repr,
        stringify = stringify,
        utils = utils,
        lpeg = lpeg,
        re = re,
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
        pairs = pairs,
        load = load,
        ipairs = ipairs
      }
      if jit then
        self.environment.len = function(x)
          do
            local mt = getmetatable(x)
            if mt then
              if mt.__len then
                return mt.__len(x)
              end
            end
          end
          return #x
        end
      else
        self.environment.len = (function(x)
          return #x
        end)
      end
      self.environment.ipairs = function(x)
        if type(x) == 'function' then
          return coroutine.wrap(x)
        elseif type(x) == 'thread' then
          return coroutine.resume, x, nil
        else
          do
            local mt = getmetatable(x)
            if mt then
              if mt.__ipairs then
                return mt.__ipairs(x)
              end
            else
              return _ipairs(x)
            end
          end
        end
      end
      self.environment.pairs = function(x)
        if type(x) == 'function' then
          return coroutine.wrap(x)
        elseif type(x) == 'thread' then
          return coroutine.resume, x, nil
        else
          do
            local mt = getmetatable(x)
            if mt then
              if mt.__pairs then
                return mt.__pairs(x)
              end
            else
              return _pairs(x)
            end
          end
        end
      end
      for k, v in pairs(Types) do
        self.environment[k] = v
      end
      self.environment.Tuple = Tuple
      self.environment.Lua = Lua
      self.environment.Nomsu = Nomsu
      self.environment.Source = Source
      self.environment.ACTIONS = setmetatable({ }, {
        __index = function(self, key)
          return function(...)
            return error("Attempt to run undefined action: " .. tostring(key), 0)
          end
        end
      })
      self.environment.COMPILE_ACTIONS = { }
      self.environment.ARG_ORDERS = setmetatable({ }, {
        __mode = "k"
      })
      self.environment.LOADED = { }
      self.environment.Types = Types
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
  stub_defs = {
    space = (P(' ') + P('\n..')) ^ 0,
    word = (NOMSU_DEFS.ident_char ^ 1 + NOMSU_DEFS.operator),
    varname = (R('az', 'AZ', '09') + P('_') + NOMSU_DEFS.utf8_char) ^ 0
  }
  stub_pattern = re.compile([=[        {~ (%space->'') (('%' (%varname->'')) / %word)? ((%space->' ') (('%' (%varname->'')) / %word))* (%space->'') ~}
    ]=], stub_defs)
  var_pattern = re.compile("{| %space ((('%' {%varname}) / %word) %space)+ |}", stub_defs)
  _nomsu_chunk_counter = 0
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
      for k, v in pairs(nomsu.environment.ACTIONS) do
        if v == info.func then
          info.name = k
          break
        end
      end
      local _ = [=[            if metadata = nomsu.action_metadata[info.func]
                info.name = metadata.aliases[1]
                filename = if type(metadata.source) == 'string'
                    metadata.source\match("^[^[:]*")
                else metadata.source.filename
                info.short_src = filename
                info.source = FILE_CACHE[filename]
                ok, linedefined = pcall(lua_line_to_nomsu_line, info.short_src, info.linedefined)
                if ok then info.linedefined = linedefined
                ok, currentline = pcall(lua_line_to_nomsu_line, info.short_src, info.currentline)
                --if ok then info.currentline = currentline
                ]=]
    end
    return info
  end
  local print_err_msg
  print_err_msg = function(error_message, stack_offset)
    if stack_offset == nil then
      stack_offset = 2
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
    local _, line_table = to_lua(nomsu_source)
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
        local name = calling_fn.name
        if name == "run_lua_fn" then
          _continue_0 = true
          break
        end
        local line = nil
        _ = [=[            if metadata = nomsu.action_metadata[calling_fn.func]
                filename, start, stop = metadata.source\match("([^:]*):([0-9]*),([0-9]*)")
                if filename
                    file = FILE_CACHE[filename]
                    line_no = 1
                    for _ in file\sub(1,tonumber(start))\gmatch("\n") do line_no += 1
                    offending_statement = file\sub(tonumber(start),tonumber(stop))
                    if #offending_statement > 50
                        offending_statement = offending_statement\sub(1,50).."..."
                    offending_statement = colored.red(offending_statement)
                    line = colored.yellow(filename..":"..tostring(line_no).."\n    "..offending_statement)
                else
                    line = colored.yellow(metadata.source)
                name = colored.bright(colored.yellow(metadata.aliases[1]))
            else
                if calling_fn.istailcall and not name
                    name = "<tail call>"
                if calling_fn.short_src == "./nomsu.moon" and line_table
                    char = line_table[calling_fn.currentline]
                    line_num = 1
                    for _ in nomsu_source\sub(1,char)\gmatch("\n") do line_num += 1
                    line = colored.cyan("#{calling_fn.short_src}:#{line_num}")
                    name = colored.bright(colored.cyan(name or "???"))
                else
                    line = colored.blue("#{calling_fn.short_src}:#{calling_fn.currentline}")
                    name = colored.bright(colored.blue(name or "???"))
            _from = colored.dim colored.white "|"
            io.stderr\write(("%32s %s %s\n")\format(name, _from, line))
            ]=]
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
    local output_file
    if args.output_file == "-" then
      output_file = io.stdout
    elseif args.output_file then
      output_file = io.open(args.output_file, 'w')
    end
    local print_file
    if args.print_file == "-" then
      print_file = io.stdout
    elseif args.print_file then
      print_file = io.open(args.print_file, 'w')
    elseif output_file == io.stdout then
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
    if output_file then
      compile_fn = function(code)
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
        nomsu:run(io.input():read("*a"), compile_fn)
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
