local re = require('re')
local lpeg = require('lpeg')
local utils = require('utils')
local new_uuid = require('uuid')
local repr, stringify, min, max, equivalent, set, is_list, sum
repr, stringify, min, max, equivalent, set, is_list, sum = utils.repr, utils.stringify, utils.min, utils.max, utils.equivalent, utils.set, utils.is_list, utils.sum
local colors = setmetatable({ }, {
  __index = function()
    return ""
  end
})
local colored = setmetatable({ }, {
  __index = function(_, color)
    return (function(msg)
      return colors[color] .. (msg or '') .. colors.reset
    end)
  end
})
local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
lpeg.setmaxstack(10000)
local P, R, V, S, Cg, C, Cp, B, Cmt
P, R, V, S, Cg, C, Cp, B, Cmt = lpeg.P, lpeg.R, lpeg.V, lpeg.S, lpeg.Cg, lpeg.C, lpeg.Cp, lpeg.B, lpeg.Cmt
local NOMSU_DEFS
do
  local _with_0 = { }
  _with_0.nl = P("\n")
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
    local spaces = self:match("[ \t]*", start)
    if #spaces > lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack] then
      insert(lpeg.userdata.indent_stack, #spaces)
      return start + #spaces
    end
  end)
  _with_0.dedent = P(function(self, start)
    local spaces = self:match("[ \t]*", start)
    if #spaces < lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack] then
      remove(lpeg.userdata.indent_stack)
      return start
    end
  end)
  _with_0.nodent = P(function(self, start)
    local spaces = self:match("[ \t]*", start)
    if #spaces == lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack] then
      return start + #spaces
    end
  end)
  _with_0.gt_nodent = P(function(self, start)
    local spaces = self:match("[ \t]*", start)
    if #spaces >= lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack] + 4 then
      return start + lpeg.userdata.indent_stack[#lpeg.userdata.indent_stack] + 4
    end
  end)
  _with_0.error = function(src, pos, err_msg)
    if lpeg.userdata.source_code:sub(pos, pos) == "\n" then
      pos = pos + #lpeg.userdata.source_code:match("[ \t\n]*", pos)
    end
    local line_no = 1
    while (lpeg.userdata.line_starts[line_no + 1] or math.huge) < pos do
      line_no = line_no + 1
    end
    local prev_line
    if line_no > 1 then
      prev_line = lpeg.userdata.source_code:match("[^\n]*", lpeg.userdata.line_starts[line_no - 1])
    else
      prev_line = ""
    end
    local err_line = lpeg.userdata.source_code:match("[^\n]*", lpeg.userdata.line_starts[line_no])
    local next_line
    if line_no < #lpeg.userdata.line_starts then
      next_line = lpeg.userdata.source_code:match("[^\n]*", lpeg.userdata.line_starts[line_no + 1])
    else
      next_line = ""
    end
    local pointer = ("-"):rep(pos - lpeg.userdata.line_starts[line_no]) .. "^"
    err_msg = (err_msg or "Parse error") .. " in " .. tostring(lpeg.userdata.filename) .. " on line " .. tostring(line_no) .. ":\n"
    err_msg = err_msg .. "\n" .. tostring(prev_line) .. "\n" .. tostring(err_line) .. "\n" .. tostring(pointer) .. "\n" .. tostring(next_line) .. "\n"
    return error(err_msg)
  end
  _with_0.FunctionCall = function(start, value, stop)
    local stub = concat((function()
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = 1, #value do
        local t = value[_index_0]
        _accum_0[_len_0] = (t.type == "Word" and t.value or "%")
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(), " ")
    local src = lpeg.userdata.source_code:sub(start, stop - 1)
    return {
      start = start,
      stop = stop,
      type = "FunctionCall",
      src = src,
      get_line_no = lpeg.userdata.get_line_no,
      value = value,
      stub = stub
    }
  end
  NOMSU_DEFS = _with_0
end
setmetatable(NOMSU_DEFS, {
  __index = function(self, key)
    local make_node
    make_node = function(start, value, stop)
      return {
        start = start,
        stop = stop,
        value = value,
        src = lpeg.userdata.source_code:sub(start, stop - 1),
        get_line_no = lpeg.userdata.get_line_no,
        type = key
      }
    end
    self[key] = make_node
    return make_node
  end
})
local NOMSU
do
  local peg_tidier = re.compile([[    file <- {~ %nl* (def/comment) (%nl+ (def/comment))* %nl* ~}
    def <- anon_def / captured_def
    anon_def <- ({ident} (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- %2"
    captured_def <- ({ident} (" "*) "(" {ident} ")" (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- ({} %3 {}) -> %2"
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    comment <- "--" [^%nl]*
    ]])
  local nomsu_peg = peg_tidier:match(io.open("nomsu.peg"):read("*a"))
  NOMSU = re.compile(nomsu_peg, NOMSU_DEFS)
end
local NomsuCompiler
do
  local _class_0
  local _base_0 = {
    writeln = function(self, ...)
      self:write(...)
      return self:write("\n")
    end,
    errorln = function(self, ...)
      self:write_err(...)
      return self:write_err("\n")
    end,
    define_action = function(self, signature, line_no, fn, src, compile_time)
      if compile_time == nil then
        compile_time = false
      end
      if type(signature) == 'string' then
        signature = self:get_stubs({
          signature
        })
      elseif type(signature) == 'table' and type(signature[1]) == 'string' then
        signature = self:get_stubs(signature)
      end
      assert(type(fn) == 'function', "Bad fn: " .. tostring(repr(fn)))
      local aliases = { }
      self.__class.def_number = self.__class.def_number + 1
      local fn_info = debug.getinfo(fn, "u")
      local fn_arg_positions, arg_orders
      if not (fn_info.isvararg) then
        do
          local _tbl_0 = { }
          for i = 1, fn_info.nparams do
            _tbl_0[debug.getlocal(fn, i)] = i
          end
          fn_arg_positions = _tbl_0
        end
        arg_orders = { }
      end
      for sig_i = 1, #signature do
        local stub, arg_names = unpack(signature[sig_i])
        assert(stub, "NO STUB FOUND: " .. tostring(repr(signature)))
        if self.debug then
          self:writeln(tostring(colored.bright("DEFINING ACTION:")) .. " " .. tostring(colored.underscore(colored.magenta(repr(stub)))) .. " " .. tostring(colored.bright("WITH ARGS")) .. " " .. tostring(colored.dim(repr(arg_names))) .. " ON: " .. tostring(self.environment.ACTIONS))
        end
        self.environment.ACTIONS[stub] = fn
        if not (fn_info.isvararg) then
          local arg_positions
          do
            local _accum_0 = { }
            local _len_0 = 1
            for _index_0 = 1, #arg_names do
              local a = arg_names[_index_0]
              _accum_0[_len_0] = fn_arg_positions[self:var_to_lua_identifier(a)]
              _len_0 = _len_0 + 1
            end
            arg_positions = _accum_0
          end
          assert(#arg_positions == #arg_names, "Mismatch in args between lua function's " .. tostring(repr(fn_arg_positions)) .. " and stub's " .. tostring(repr(arg_names)))
          arg_orders[stub] = arg_positions
        end
      end
      self.action_metadata[fn] = {
        fn = fn,
        src = src,
        line_no = line_no,
        aliases = aliases,
        arg_orders = arg_orders,
        arg_positions = fn_arg_positions,
        def_number = self.__class.def_number
      }
    end,
    define_compile_action = function(self, signature, line_no, fn, src)
      self:define_action(signature, line_no, fn, src, true)
      self.action_metadata[fn].compile_time = true
      if self.debug then
        return self:writeln(tostring(colored.bright(colored.green("(it was compile time)"))))
      end
    end,
    serialize_defs = function(self, scope, after)
      if scope == nil then
        scope = nil
      end
      if after == nil then
        after = nil
      end
      error("Not currently functional.")
      after = after or (self.core_defs or 0)
      scope = scope or self.defs
      local defs_by_num = { }
      for stub, def in pairs(scope) do
        if def and stub:sub(1, 1) ~= "#" then
          defs_by_num[def.def_number] = def
        end
      end
      local keys
      do
        local _accum_0 = { }
        local _len_0 = 1
        for k, v in pairs(defs_by_num) do
          _accum_0[_len_0] = k
          _len_0 = _len_0 + 1
        end
        keys = _accum_0
      end
      table.sort(keys)
      local buff = { }
      local k_i = 1
      local _using = nil
      local _using_do = { }
      for k_i, i in ipairs(keys) do
        local _continue_0 = false
        repeat
          if i <= after then
            _continue_0 = true
            break
          end
          local def = defs_by_num[i]
          if def.defs == scope then
            if def.src then
              insert(buff, def.src)
            end
            _continue_0 = true
            break
          end
          if _using == def.defs then
            if def.src then
              insert(_using_do, def.src)
            end
          else
            _using = def.defs
            _using_do = {
              def.src
            }
          end
          if k_i == #keys or defs_by_num[keys[k_i + 1]].defs ~= _using then
            insert(buff, "using:\n    " .. tostring(self:indent(self:serialize_defs(_using))) .. "\n..do:\n    " .. tostring(self:indent(concat(_using_do, "\n"))))
          end
          _continue_0 = true
        until true
        if not _continue_0 then
          break
        end
      end
      for k, v in pairs(scope["#vars"] or { }) do
        insert(buff, "<%" .. tostring(k) .. "> = " .. tostring(self:value_to_nomsu(v)))
      end
      return concat(buff, "\n")
    end,
    dedent = function(self, code)
      if not (code:find("\n")) then
        return code
      end
      local spaces, indent_spaces = math.huge, math.huge
      for line in code:gmatch("\n([^\n]*)") do
        local _continue_0 = false
        repeat
          if line:match("^%s*#.*") then
            _continue_0 = true
            break
          else
            do
              local s = line:match("^(%s*)%.%..*")
              if s then
                spaces = math.min(spaces, #s)
              else
                do
                  s = line:match("^(%s*)%S.*")
                  if s then
                    indent_spaces = math.min(indent_spaces, #s)
                  end
                end
              end
            end
          end
          _continue_0 = true
        until true
        if not _continue_0 then
          break
        end
      end
      if spaces ~= math.huge and spaces < indent_spaces then
        return (code:gsub("\n" .. (" "):rep(spaces), "\n"))
      else
        return (code:gsub("\n" .. (" "):rep(indent_spaces), "\n    "))
      end
    end,
    indent = function(self, code, levels)
      if levels == nil then
        levels = 1
      end
      return code:gsub("\n", "\n" .. ("    "):rep(levels))
    end,
    parse = function(self, nomsu_code, filename)
      assert(type(filename) == "string", "Bad filename type: " .. tostring(type(filename)))
      if self.debug then
        self:writeln(tostring(colored.bright("PARSING:")) .. "\n" .. tostring(colored.yellow(nomsu_code)))
      end
      nomsu_code = nomsu_code:gsub("\r", "")
      local userdata
      do
        local _with_0 = {
          source_code = nomsu_code,
          filename = filename,
          indent_stack = {
            0
          }
        }
        _with_0.line_starts = re.compile("lines <- {| line ('\n' line)* |} line <- {} [^\n]*"):match(nomsu_code)
        _with_0.get_line_no = function(self)
          if not (self._line_no) then
            local line_no = 1
            while (_with_0.line_starts[line_no + 1] or math.huge) < self.start do
              line_no = line_no + 1
            end
            self._line_no = tostring(_with_0.filename) .. ":" .. tostring(line_no)
          end
          return self._line_no
        end
        userdata = _with_0
      end
      local old_userdata
      old_userdata, lpeg.userdata = lpeg.userdata, userdata
      local tree = NOMSU:match(nomsu_code)
      lpeg.userdata = old_userdata
      assert(tree, "In file " .. tostring(colored.blue(filename)) .. " failed to parse:\n" .. tostring(colored.onyellow(colored.black(nomsu_code))))
      if self.debug then
        self:writeln("PARSE TREE:")
        self:print_tree(tree, "    ")
      end
      return tree
    end,
    run = function(self, src, filename, max_operations, output_file)
      if max_operations == nil then
        max_operations = nil
      end
      if output_file == nil then
        output_file = nil
      end
      if src == "" then
        return nil, ""
      end
      if max_operations then
        local timeout
        timeout = function()
          debug.sethook()
          return error("Execution quota exceeded. Your code took too long.")
        end
        debug.sethook(timeout, "", max_operations)
      end
      local tree = self:parse(src, filename)
      assert(tree, "Failed to parse: " .. tostring(src))
      assert(tree.type == "File", "Attempt to run non-file: " .. tostring(tree.type))
      local lua = self:tree_to_lua(tree)
      local lua_code = lua.statements or (lua.expr .. ";")
      lua_code = "-- File: " .. tostring(filename) .. "\n" .. lua_code
      local ret = self:run_lua(lua_code)
      if max_operations then
        debug.sethook()
      end
      if output_file then
        output_file:write(lua_code)
      end
      return ret, lua_code
    end,
    run_file = function(self, filename)
      if filename:match(".*%.lua") then
        local file = io.open(filename)
        local contents = file:read("*a")
        file:close()
        return assert(load(contents, nil, nil, self.environment))()
      end
      if filename:match(".*%.nom") then
        if not self.skip_precompiled then
          local file = io.open(filename:gsub("%.nom", ".lua"), "r")
          if file then
            local lua_code = file:read("*a")
            file:close()
            return self:run_lua(lua_code)
          end
        end
        local file = file or io.open(filename)
        if not file then
          error("File does not exist: " .. tostring(filename))
        end
        local nomsu_code = file:read('*a')
        file:close()
        return self:run(nomsu_code, filename)
      else
        return error("Invalid filetype for " .. tostring(filename))
      end
    end,
    require_file = function(self, filename)
      local loaded = self.environment.LOADED
      if not loaded[filename] then
        loaded[filename] = self:run_file(filename) or true
      end
      return loaded[filename]
    end,
    run_lua = function(self, lua_code)
      local run_lua_fn, err = load(lua_code, nil, nil, self.environment)
      if self.debug then
        self:writeln(tostring(colored.bright("RUNNING LUA:")) .. "\n" .. tostring(colored.blue(colored.bright(lua_code))))
      end
      if not run_lua_fn then
        local n = 1
        local fn
        fn = function()
          n = n + 1
          return ("\n%-3d|"):format(n)
        end
        local code = "1  |" .. lua_code:gsub("\n", fn)
        error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(colored.onblack(code)))) .. "\n\n" .. tostring(err))
      end
      return run_lua_fn()
    end,
    tree_to_value = function(self, tree, filename)
      local code = "return " .. tostring(self:tree_to_lua(tree).expr) .. ";"
      if self.debug then
        self:writeln(tostring(colored.bright("RUNNING LUA TO GET VALUE:")) .. "\n" .. tostring(colored.blue(colored.bright(code))))
      end
      local lua_thunk, err = load(code, nil, nil, self.environment)
      if not lua_thunk then
        error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(colored.onblack(code)))) .. "\n\n" .. tostring(colored.red(err)))
      end
      return lua_thunk()
    end,
    tree_to_nomsu = function(self, tree, indentation, max_line, expr_type)
      if indentation == nil then
        indentation = ""
      end
      if max_line == nil then
        max_line = 80
      end
      if expr_type == nil then
        expr_type = nil
      end
      assert(tree, "No tree provided to tree_to_nomsu.")
      assert(tree.type, "Invalid tree: " .. tostring(repr(tree)))
      local join_lines
      join_lines = function(lines)
        for _index_0 = 1, #lines do
          local line = lines[_index_0]
          if #indentation + #line > max_line then
            return nil
          end
        end
        return concat(lines, "\n" .. indentation)
      end
      local is_operator
      is_operator = function(tok)
        return tok and tok.type == "Word" and NOMSU_DEFS.operator:match(tok.value)
      end
      local inline_expression, noeol_expression, expression
      inline_expression = function(tok)
        local _exp_0 = tok.type
        if "Block" == _exp_0 then
          if #tok.value > 1 then
            return nil
          end
          local nomsu = inline_expression(tok.value)
          return nomsu and "(: " .. tostring(nomsu) .. ")"
        elseif "FunctionCall" == _exp_0 then
          local buff = ""
          for i, bit in ipairs(tok.value) do
            if bit.type == "Word" then
              if i == 1 or (is_operator(bit) and is_operator(tok.value[i - 1])) then
                buff = buff .. bit.value
              else
                buff = buff .. (" " .. bit.value)
              end
            else
              local nomsu = inline_expression(bit)
              if not (nomsu) then
                return nil
              end
              if not (i == 1 or bit.type == "Block") then
                buff = buff .. " "
              end
              buff = buff .. (function()
                if bit.type == "FunctionCall" then
                  return "(" .. nomsu .. ")"
                else
                  return nomsu
                end
              end)()
            end
          end
          return buff
        elseif "List" == _exp_0 then
          local bits = { }
          local _list_0 = tok.value
          for _index_0 = 1, #_list_0 do
            local bit = _list_0[_index_0]
            local nomsu = inline_expression(bit)
            if not (nomsu) then
              return nil
            end
            insert(bits, nomsu)
          end
          return "[" .. concat(bits, ", ") .. "]"
        elseif "Dict" == _exp_0 then
          local bits = { }
          local _list_0 = tok.value
          for _index_0 = 1, #_list_0 do
            local bit = _list_0[_index_0]
            local key_nomsu
            if bit.dict_key.type == "Word" then
              key_nomsu = bit.dict_key.value
            else
              key_nomsu = inline_expression(bit.dict_key)
            end
            if not (key_nomsu) then
              return nil
            end
            if bit.dict_key.type == "FunctionCall" then
              key_nomsu = "(" .. key_nomsu .. ")"
            end
            local value_nomsu = inline_expression(bit.dict_value)
            if not (value_nomsu) then
              return nil
            end
            insert(bits, key_nomsu .. "=" .. value_nomsu)
          end
          return "{" .. concat(bits, ", ") .. "}"
        elseif "Text" == _exp_0 then
          local buff = '"'
          local _list_0 = tok.value
          for _index_0 = 1, #_list_0 do
            local bit = _list_0[_index_0]
            if type(bit) == 'string' then
              if bit:find("\n") then
                return nil
              end
              buff = buff .. bit:gsub("\\", "\\\\"):gsub("\n", "\\n")
            else
              local nomsu = inline_expression(bit)
              if not (nomsu) then
                return nil
              end
              buff = buff .. (function()
                if bit.type == "Var" or bit.type == "List" or bit.type == "Dict" then
                  return "\\" .. nomsu
                else
                  return "\\(" .. nomsu .. ")"
                end
              end)()
            end
            if #buff > max_line then
              return nil
            end
          end
          return buff .. '"'
        elseif "Nomsu" == _exp_0 then
          local nomsu = inline_expression(tok.value)
          if not nomsu then
            return nil
          end
          return "\\(" .. nomsu .. ")"
        elseif "Number" == _exp_0 then
          return tostring(tok.value)
        elseif "Var" == _exp_0 then
          return "%" .. tok.value
        else
          return nil
        end
      end
      noeol_expression = function(tok)
        local nomsu = inline_expression(tok)
        if nomsu and #nomsu < max_line then
          return nomsu
        end
        local _exp_0 = tok.type
        if "Block" == _exp_0 then
          local buff = ":"
          local _list_0 = tok.value
          for _index_0 = 1, #_list_0 do
            local line = _list_0[_index_0]
            nomsu = expression(line)
            if not (nomsu) then
              return nil
            end
            buff = buff .. ("\n    " .. self:indent(nomsu))
          end
          return buff
        elseif "FunctionCall" == _exp_0 then
          nomsu = expression(tok)
          if not (nomsu) then
            return nil
          end
          return "(..)\n    " .. self:indent(nomsu)
        elseif "List" == _exp_0 then
          local buff = "[..]"
          local line = "\n    "
          local _list_0 = tok.value
          for _index_0 = 1, #_list_0 do
            local bit = _list_0[_index_0]
            nomsu = inline_expression(bit)
            if line ~= "\n    " and #line + #", " + #nomsu > max_line then
              buff = buff .. line
              line = "\n    "
            end
            local sep = line == "\n    " and "" or ", "
            if nomsu then
              line = line .. (sep .. nomsu)
              if #line >= max_line then
                buff = buff .. line
                line = "\n    "
              end
            else
              line = line .. (sep .. expression(bit))
              buff = buff .. line
              line = "\n    "
            end
          end
          if line ~= "\n    " then
            buff = buff .. line
          end
          return buff
        elseif "Dict" == _exp_0 then
          local buff = "{..}"
          local line = "\n    "
          local _list_0 = tok.value
          for _index_0 = 1, #_list_0 do
            local bit = _list_0[_index_0]
            local key_nomsu = inline_expression(bit.dict_key)
            if not (key_nomsu) then
              return nil
            end
            if bit.dict_key.type == "FunctionCall" then
              key_nomsu = "(" .. key_nomsu .. ")"
            end
            local value_nomsu = inline_expression(bit.dict_value)
            if value_nomsu and #key_nomsu + #value_nomsu < max_line then
              line = line .. (key_nomsu .. "=" .. value_nomsu .. ",")
              if #line >= max_line then
                buff = buff .. line
                line = "\n    "
              end
            else
              line = line .. (key_nomsu .. "=" .. expression(bit.dict_value))
              buff = buff .. line
              line = "\n    "
            end
          end
          if line ~= "\n    " then
            buff = buff .. line
          end
          return buff
        elseif "Text" == _exp_0 then
          local buff = '".."\n    '
          local _list_0 = tok.value
          for _index_0 = 1, #_list_0 do
            local bit = _list_0[_index_0]
            if type(bit) == 'string' then
              buff = buff .. bit:gsub("\\", "\\\\"):gsub("\n", "\n    ")
            else
              nomsu = inline_expression(bit)
              if not (nomsu) then
                return nil
              end
              buff = buff .. (function()
                if bit.type == "Var" or bit.type == "List" or bit.type == "Dict" then
                  return "\\" .. nomsu
                else
                  return "\\(" .. nomsu .. ")"
                end
              end)()
            end
          end
          return buff
        elseif "Nomsu" == _exp_0 then
          nomsu = expression(tok.value)
          if not nomsu then
            return nil
          end
          return "\\(..)\n    " .. self:indent(nomsu)
        elseif "Comment" == _exp_0 then
          if tok.value:find("\n") then
            return "#.." .. tok.value:gsub("\n", "\n    ")
          else
            return "#" .. tok.value
          end
        else
          return inline_expression(tok)
        end
      end
      expression = function(tok)
        local nomsu = inline_expression(tok)
        if nomsu and #nomsu < max_line then
          return nomsu
        end
        local _exp_0 = tok.type
        if "Block" == _exp_0 then
          if #tok.value == 1 then
            if tok.value[1].type == "FunctionCall" then
              nomsu = inline_expression(tok.value[1])
            else
              nomsu = noeol_expression(tok.value[1])
            end
            if nomsu and #(nomsu:match("[^\n]*")) < max_line then
              return ": " .. nomsu
            end
          end
          return noeol_expression(tok)
        elseif "FunctionCall" == _exp_0 then
          local buff = ""
          for i, bit in ipairs(tok.value) do
            if bit.type == "Word" then
              if i == 1 or (is_operator(bit) and is_operator(tok.value[i - 1])) or buff:sub(-2, -1) == ".." then
                buff = buff .. bit.value
              else
                buff = buff .. (" " .. bit.value)
              end
            else
              nomsu = inline_expression(bit)
              if nomsu and #nomsu < max_line then
                if bit.type == "FunctionCall" then
                  nomsu = "(" .. nomsu .. ")"
                end
              else
                nomsu = expression(bit)
                if not (nomsu) then
                  return nil
                end
                if bit.type == "FunctionCall" then
                  nomsu = "(..)\n    " .. self:indent(nomsu)
                end
                if i < #tok.value then
                  nomsu = nomsu .. "\n.."
                end
              end
              if not (i == 1 or bit.type == "Block") then
                buff = buff .. " "
              end
              buff = buff .. nomsu
            end
          end
          return buff
        elseif "File" == _exp_0 then
          local lines = { }
          local _list_0 = tree.value
          for _index_0 = 1, #_list_0 do
            local line = _list_0[_index_0]
            nomsu = expression(line)
            assert(nomsu, "Failed to produce output for:\n" .. tostring(colored.yellow(line.src)))
            insert(lines, nomsu)
          end
          return concat(lines, "\n")
        elseif "Comment" == _exp_0 then
          if tok.value:find("\n") then
            return "#.." .. tok.value:gsub("\n", "\n    ")
          else
            return "#" .. tok.value
          end
        else
          return noeol_expression(tok)
        end
      end
      return expression(tree)
    end,
    value_to_nomsu = function(self, value)
      local _exp_0 = type(value)
      if "nil" == _exp_0 then
        return "(nil)"
      elseif "bool" == _exp_0 then
        return value and "(yes)" or "(no)"
      elseif "number" == _exp_0 then
        return repr(value)
      elseif "table" == _exp_0 then
        if is_list(value) then
          return "[" .. tostring(concat((function()
            local _accum_0 = { }
            local _len_0 = 1
            for _index_0 = 1, #value do
              local v = value[_index_0]
              _accum_0[_len_0] = self:value_to_nomsu(v)
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)(), ", ")) .. "]"
        else
          return "{" .. tostring(concat((function()
            local _accum_0 = { }
            local _len_0 = 1
            for k, v in pairs(value) do
              _accum_0[_len_0] = tostring(self:value_to_nomsu(k)) .. "=" .. tostring(self:value_to_nomsu(v))
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)(), ", ")) .. "}"
        end
      elseif "string" == _exp_0 then
        if value == "\n" then
          return "'\\n'"
        elseif not value:find([["]]) and not value:find("\n") and not value:find("\\") then
          return "\"" .. value .. "\""
        else
          return '".."\n    ' .. (self:indent(value))
        end
      else
        return error("Unsupported value_to_nomsu type: " .. tostring(type(value)))
      end
    end,
    tree_to_lua = function(self, tree)
      assert(tree, "No tree provided.")
      if not tree.type then
        error("Invalid tree: " .. tostring(repr(tree)))
      end
      local _exp_0 = tree.type
      if "File" == _exp_0 then
        if #tree.value == 1 then
          return self:tree_to_lua(tree.value[1])
        end
        local lua_bits = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local line = _list_0[_index_0]
          local lua = self:tree_to_lua(line)
          if not lua then
            error("No lua produced by " .. tostring(repr(line)))
          end
          if lua.statements then
            insert(lua_bits, lua.statements)
          end
          if lua.expr then
            insert(lua_bits, tostring(lua.expr) .. ";")
          end
        end
        return {
          statements = concat(lua_bits, "\n")
        }
      elseif "Comment" == _exp_0 then
        return {
          statements = "--" .. tree.value:gsub("\n", "\n--")
        }
      elseif "Nomsu" == _exp_0 then
        return {
          expr = "nomsu:parse(" .. tostring(repr(tree.value.src)) .. ", " .. tostring(repr(tree:get_line_no())) .. ").value[1]"
        }
      elseif "Block" == _exp_0 then
        local lua_bits = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local arg = _list_0[_index_0]
          local lua = self:tree_to_lua(arg)
          if #tree.value == 1 and lua.expr and not lua.statements then
            return {
              expr = lua.expr
            }
          end
          if lua.statements then
            insert(lua_bits, lua.statements)
          end
          if lua.expr then
            insert(lua_bits, tostring(lua.expr) .. ";")
          end
        end
        return {
          statements = concat(lua_bits, "\n")
        }
      elseif "FunctionCall" == _exp_0 then
        insert(self.compilestack, tree)
        local fn = rawget(self.environment.ACTIONS, tree.stub)
        local metadata = self.environment.ACTION_METADATA[fn]
        if metadata and metadata.compile_time then
          local args
          do
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = tree.value
            for _index_0 = 1, #_list_0 do
              local arg = _list_0[_index_0]
              if arg.type ~= "Word" then
                _accum_0[_len_0] = arg
                _len_0 = _len_0 + 1
              end
            end
            args = _accum_0
          end
          if metadata and metadata.arg_orders then
            local new_args
            do
              local _accum_0 = { }
              local _len_0 = 1
              local _list_0 = metadata.arg_orders[tree.stub]
              for _index_0 = 1, #_list_0 do
                local p = _list_0[_index_0]
                _accum_0[_len_0] = args[p]
                _len_0 = _len_0 + 1
              end
              new_args = _accum_0
            end
            args = new_args
          end
          if self.debug then
            self:write(tostring(colored.bright("RUNNING MACRO")) .. " " .. tostring(colored.underscore(colored.magenta(tree.stub))) .. " ")
            self:writeln(tostring(colored.bright("WITH ARGS:")) .. " " .. tostring(colored.dim(repr((function()
              local _accum_0 = { }
              local _len_0 = 1
              for _index_0 = 1, #args do
                local a = args[_index_0]
                _accum_0[_len_0] = (repr(a)):sub(1, 50)
                _len_0 = _len_0 + 1
              end
              return _accum_0
            end)()))))
          end
          local lua = fn(unpack(args))
          remove(self.compilestack)
          return lua
        elseif not metadata and self.__class.math_patt:match(tree.stub) then
          local bits = { }
          local _list_0 = tree.value
          for _index_0 = 1, #_list_0 do
            local tok = _list_0[_index_0]
            if tok.type == "Word" then
              insert(bits, tok.value)
            else
              local lua = self:tree_to_lua(tok)
              assert(lua.statements == nil, "non-expression value inside math expression")
              insert(bits, lua.expr)
            end
          end
          remove(self.compilestack)
          return {
            expr = "(" .. tostring(concat(bits, " ")) .. ")"
          }
        end
        local args = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local _continue_0 = false
          repeat
            local tok = _list_0[_index_0]
            if tok.type == "Word" then
              _continue_0 = true
              break
            end
            local lua = self:tree_to_lua(tok)
            assert(lua.expr, "Cannot use " .. tostring(tok.src) .. " as an argument, since it's not an expression, it produces: " .. tostring(repr(lua)))
            insert(args, lua.expr)
            _continue_0 = true
          until true
          if not _continue_0 then
            break
          end
        end
        if metadata and metadata.arg_orders then
          local new_args
          do
            local _accum_0 = { }
            local _len_0 = 1
            local _list_1 = metadata.arg_orders[tree.stub]
            for _index_0 = 1, #_list_1 do
              local p = _list_1[_index_0]
              _accum_0[_len_0] = args[p]
              _len_0 = _len_0 + 1
            end
            new_args = _accum_0
          end
          args = new_args
        end
        remove(self.compilestack)
        return {
          expr = self.__class:comma_separated_items("ACTIONS[" .. tostring(repr(tree.stub)) .. "](", args, ")")
        }
      elseif "Text" == _exp_0 then
        local concat_parts = { }
        local string_buffer = ""
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local _continue_0 = false
          repeat
            local bit = _list_0[_index_0]
            if type(bit) == "string" then
              string_buffer = string_buffer .. bit
              _continue_0 = true
              break
            end
            if string_buffer ~= "" then
              insert(concat_parts, repr(string_buffer))
              string_buffer = ""
            end
            local lua = self:tree_to_lua(bit)
            if self.debug then
              self:writeln((colored.bright("INTERP:")))
              self:print_tree(bit)
              self:writeln(tostring(colored.bright("EXPR:")) .. " " .. tostring(lua.expr) .. ", " .. tostring(colored.bright("STATEMENT:")) .. " " .. tostring(lua.statements))
            end
            if lua.statements then
              error("Cannot use [[" .. tostring(bit.src) .. "]] as a string interpolation value, since it's not an expression.")
            end
            insert(concat_parts, "stringify(" .. tostring(lua.expr) .. ")")
            _continue_0 = true
          until true
          if not _continue_0 then
            break
          end
        end
        if string_buffer ~= "" then
          insert(concat_parts, repr(string_buffer))
        end
        if #concat_parts == 0 then
          return {
            expr = "''"
          }
        elseif #concat_parts == 1 then
          return {
            expr = concat_parts[1]
          }
        else
          return {
            expr = "(" .. tostring(concat(concat_parts, "..")) .. ")"
          }
        end
      elseif "List" == _exp_0 then
        local items = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local item = _list_0[_index_0]
          local lua = self:tree_to_lua(item)
          if lua.statements then
            error("Cannot use [[" .. tostring(item.src) .. "]] as a list item, since it's not an expression.")
          end
          insert(items, lua.expr)
        end
        return {
          expr = self.__class:comma_separated_items("{", items, "}")
        }
      elseif "Dict" == _exp_0 then
        local items = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local entry = _list_0[_index_0]
          local key_lua
          if entry.dict_key.type == "Word" then
            key_lua = {
              expr = repr(entry.dict_key.value)
            }
          else
            key_lua = self:tree_to_lua(entry.dict_key)
          end
          if key_lua.statements then
            error("Cannot use [[" .. tostring(entry.dict_key.src) .. "]] as a dict key, since it's not an expression.")
          end
          local value_lua = self:tree_to_lua(entry.dict_value)
          if value_lua.statements then
            error("Cannot use [[" .. tostring(entry.dict_value.src) .. "]] as a dict value, since it's not an expression.")
          end
          local key_str = key_lua.expr:match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
          if key_str then
            insert(items, tostring(key_str) .. "=" .. tostring(value_lua.expr))
          elseif key_lua.expr:sub(1, 1) == "[" then
            insert(items, "[ " .. tostring(key_lua.expr) .. "]=" .. tostring(value_lua.expr))
          else
            insert(items, "[" .. tostring(key_lua.expr) .. "]=" .. tostring(value_lua.expr))
          end
        end
        return {
          expr = self.__class:comma_separated_items("{", items, "}")
        }
      elseif "Number" == _exp_0 then
        return {
          expr = repr(tree.value)
        }
      elseif "Var" == _exp_0 then
        return {
          expr = self:var_to_lua_identifier(tree.value)
        }
      else
        return error("Unknown/unimplemented thingy: " .. tostring(tree.type))
      end
    end,
    walk_tree = function(self, tree, depth)
      if depth == nil then
        depth = 0
      end
      coroutine.yield(tree, depth)
      if type(tree) ~= 'table' or not tree.type then
        return 
      end
      local _exp_0 = tree.type
      if "List" == _exp_0 or "File" == _exp_0 or "Block" == _exp_0 or "FunctionCall" == _exp_0 or "Text" == _exp_0 then
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local v = _list_0[_index_0]
          self:walk_tree(v, depth + 1)
        end
      elseif "Dict" == _exp_0 then
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local e = _list_0[_index_0]
          self:walk_tree(e.dict_key, depth + 1)
          self:walk_tree(e.dict_value, depth + 1)
        end
      else
        self:walk_tree(tree.value, depth + 1)
      end
      return nil
    end,
    print_tree = function(self, tree)
      self:write(colors.bright .. colors.green)
      for node, depth in coroutine.wrap(function()
        return self:walk_tree(tree)
      end) do
        if type(node) ~= 'table' or not node.type then
          self:writeln(("    "):rep(depth) .. repr(node))
        else
          self:writeln(tostring(("    "):rep(depth)) .. tostring(node.type) .. ":")
        end
      end
      return self:write(colors.reset)
    end,
    tree_to_str = function(self, tree)
      local bits = { }
      for node, depth in coroutine.wrap(function()
        return self:walk_tree(tree)
      end) do
        if type(node) ~= 'table' or not node.type then
          insert(bits, (("    "):rep(depth) .. repr(node)))
        else
          insert(bits, (tostring(("    "):rep(depth)) .. tostring(node.type) .. ":"))
        end
      end
      return concat(bits, "\n")
    end,
    tree_with_replaced_vars = function(self, tree, replacements)
      if type(tree) ~= 'table' then
        return tree
      end
      local _exp_0 = tree.type
      if "Var" == _exp_0 then
        if replacements[tree.value] ~= nil then
          tree = replacements[tree.value]
        end
      elseif "File" == _exp_0 or "Nomsu" == _exp_0 or "Block" == _exp_0 or "List" == _exp_0 or "FunctionCall" == _exp_0 or "Text" == _exp_0 then
        local new_value = self:tree_with_replaced_vars(tree.value, replacements)
        if new_value ~= tree.value then
          do
            local _tbl_0 = { }
            for k, v in pairs(tree) do
              _tbl_0[k] = v
            end
            tree = _tbl_0
          end
          tree.value = new_value
        end
      elseif "Dict" == _exp_0 then
        local dirty = false
        replacements = { }
        for i, e in ipairs(tree.value) do
          local new_key = self:tree_with_replaced_vars(e.dict_key, replacements)
          local new_value = self:tree_with_replaced_vars(e.dict_value, replacements)
          dirty = dirty or (new_key ~= e.dict_key or new_value ~= e.dict_value)
          replacements[i] = {
            dict_key = new_key,
            dict_value = new_value
          }
        end
        if dirty then
          do
            local _tbl_0 = { }
            for k, v in pairs(tree) do
              _tbl_0[k] = v
            end
            tree = _tbl_0
          end
          tree.value = replacements
        end
      elseif nil == _exp_0 then
        local new_values = { }
        local any_different = false
        for k, v in pairs(tree) do
          new_values[k] = self:tree_with_replaced_vars(v, replacements)
          any_different = any_different or (new_values[k] ~= tree[k])
        end
        if any_different then
          tree = new_values
        end
      end
      return tree
    end,
    get_stub = function(self, x)
      if not x then
        error("Nothing to get stub from")
      end
      if type(x) == 'string' then
        local spec = concat(self.__class.stub_patt:match(x), " ")
        local arg_names = { }
        local stub = spec:gsub("%%(%S*)", function(arg)
          insert(arg_names, arg)
          return "%"
        end)
        return stub, arg_names
      end
      if type(x) ~= 'table' then
        error("Invalid type for getting stub: " .. tostring(type(x)) .. " for:\n" .. tostring(repr(x)))
      end
      local _exp_0 = x.type
      if "Text" == _exp_0 then
        return self:get_stub(x.value)
      elseif "FunctionCall" == _exp_0 then
        return self:get_stub(x.src)
      else
        return error("Unsupported get stub type: " .. tostring(x.type) .. " for " .. tostring(repr(x)))
      end
    end,
    get_stubs = function(self, x)
      if type(x) ~= 'table' then
        return {
          {
            self:get_stub(x)
          }
        }
      end
      local _exp_0 = x.type
      if nil == _exp_0 then
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #x do
          local i = x[_index_0]
          _accum_0[_len_0] = {
            self:get_stub(i)
          }
          _len_0 = _len_0 + 1
        end
        return _accum_0
      elseif "List" == _exp_0 then
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = x.value
        for _index_0 = 1, #_list_0 do
          local i = _list_0[_index_0]
          _accum_0[_len_0] = {
            self:get_stub(i)
          }
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end
      return {
        {
          self:get_stub(x)
        }
      }
    end,
    var_to_lua_identifier = function(self, var)
      if type(var) == 'table' and var.type == "Var" then
        var = var.value
      end
      return "_" .. (var:gsub("%W", function(verboten)
        if verboten == "_" then
          return "__"
        else
          return ("_%x"):format(verboten:byte())
        end
      end))
    end,
    source_code = function(self, level)
      if level == nil then
        level = 0
      end
      return self:dedent(self.compilestack[#self.compilestack - level].src)
    end,
    initialize_core = function(self)
      local nomsu = self
      local nomsu_string_as_lua
      nomsu_string_as_lua = function(code)
        local concat_parts = { }
        local _list_0 = code.value
        for _index_0 = 1, #_list_0 do
          local bit = _list_0[_index_0]
          if type(bit) == "string" then
            insert(concat_parts, bit)
          else
            local lua = nomsu:tree_to_lua(bit)
            if lua.statements then
              error("Cannot use [[" .. tostring(bit.src) .. "]] as a string interpolation value, since it's not an expression.")
            end
            insert(concat_parts, lua.expr)
          end
        end
        return concat(concat_parts)
      end
      self:define_compile_action("immediately %block", "nomsu.moon", function(_block)
        local lua = nomsu:tree_to_lua(_block)
        local lua_code = lua.statements or (lua.expr .. ";")
        lua_code = "-- Immediately:\n" .. lua_code
        nomsu:run_lua(lua_code)
        return {
          statements = lua_code
        }
      end)
      self:define_compile_action("lua> %code", "nomsu.moon", function(_code)
        local lua = nomsu_string_as_lua(_code)
        return {
          statements = lua
        }
      end)
      self:define_compile_action("=lua %code", "nomsu.moon", function(_code)
        local lua = nomsu_string_as_lua(_code)
        return {
          expr = lua
        }
      end)
      self:define_compile_action("__line_no__", "nomsu.moon", function()
        return {
          expr = repr(nomsu.compilestack[#nomsu.compilestack]:get_line_no())
        }
      end)
      self:define_compile_action("__src__ %level", "nomsu.moon", function(_level)
        return {
          expr = repr(nomsu:source_code(nomsu:tree_to_value(_level)))
        }
      end)
      self:define_action("run file %filename", "nomsu.moon", function(_filename)
        return nomus:run_file(_filename)
      end)
      return self:define_compile_action("use %filename", "nomsu.moon", function(_filename)
        local filename = nomsu:tree_to_value(_filename)
        nomsu:require_file(filename)
        return {
          statements = "nomsu:require_file(" .. tostring(repr(filename)) .. ");"
        }
      end)
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self)
      self.write = function(self, ...)
        return io.write(...)
      end
      self.write_err = function(self, ...)
        return io.stderr:write(...)
      end
      self.ids = setmetatable({ }, {
        __mode = "k",
        __index = function(self, key)
          local id = new_uuid()
          self[key] = id
          return id
        end
      })
      self.compilestack = { }
      self.debug = false
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
      self.environment.ACTIONS = setmetatable({ }, {
        __index = function(self, key)
          return error("Attempt to run undefined action: " .. tostring(key), 0)
        end
      })
      self.action_metadata = setmetatable({ }, {
        __mode = "k"
      })
      self.environment.ACTION_METADATA = self.action_metadata
      self.environment.LOADED = { }
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
  self.def_number = 0
  self.math_patt = re.compile([[ "%" (" " [*/^+-] " %")+ ]])
  self.unescape_string = function(self, str)
    return Cs(((P("\\\\") / "\\") + (P("\\\"") / '"') + NOMSU_DEFS.escaped_char + P(1)) ^ 0):match(str)
  end
  self.comma_separated_items = function(self, open, items, close)
    local bits = {
      open
    }
    local so_far = 0
    for i, item in ipairs(items) do
      if i < #items then
        item = item .. ", "
      end
      insert(bits, item)
      so_far = so_far + #item
      if so_far >= 80 then
        insert(bits, "\n")
        so_far = 0
      end
    end
    insert(bits, close)
    return concat(bits)
  end
  self.stub_patt = re.compile("{|(' '+ / '\n..' / {'%' %id*} / {%id+} / {%op})*|}", {
    id = NOMSU_DEFS.ident_char,
    op = NOMSU_DEFS.operator
  })
  NomsuCompiler = _class_0
end
if arg then
  colors = require('consolecolors')
  local parser = re.compile([[        args <- {| {:flags: flags? :} ({:input: input :} ";" ("-o;"{:output: output :} ";")?)? (";")? |} !.
        flags <- (({| ({flag} ";")* |}) -> set)
        flag <- "-c" / "-i" / "-p" / "-O" / "--help" / "-h" / "-v"
        input <- "-" / [^;]+
        output <- "-" / [^;]+
    ]], {
    set = set
  })
  local args = concat(arg, ";") .. ";"
  args = parser:match(args) or { }
  if not args or not args.flags or args.flags["--help"] or args.flags["-h"] then
    print("Usage: lua nomsu.lua [-c] [-i] [-p] [-O] [--help] [input [-o output]]")
    os.exit()
  end
  local nomsu = NomsuCompiler()
  local run
  run = function()
    if args.flags["-v"] then
      nomsu.debug = true
    end
    nomsu.skip_precompiled = not args.flags["-O"]
    if args.input then
      if args.flags["-c"] and not args.output then
        args.output = args.input:gsub("%.nom", ".lua")
      end
      local compiled_output = nil
      if args.flags["-p"] then
        local _write = nomsu.write
        nomsu.write = function() end
        compiled_output = io.output()
      elseif args.output then
        compiled_output = io.open(args.output, 'w')
      end
      if args.input:match(".*%.lua") then
        local retval = dofile(args.input)(nomsu, { })
      else
        local input
        if args.input == '-' then
          input = io.read('*a')
        else
          input = io.open(args.input):read("*a")
        end
        local retval, code = nomsu:run(input, args.input)
        if args.output then
          compiled_output:write(code)
        end
      end
      if args.flags["-p"] then
        nomsu.write = _write
      end
    end
    if args.flags["-i"] then
      nomsu:run('use "lib/core.nom"', "stdin")
      while true do
        local buff = ""
        while true do
          io.write(">> ")
          local line = io.read("*L")
          if line == "\n" or not line then
            break
          end
          buff = buff .. line
        end
        if #buff == 0 then
          break
        end
        local ok, ret = pcall(function()
          return nomsu:run(buff, "stdin")
        end)
        if ok and ret ~= nil then
          print("= " .. repr(ret))
        end
      end
    end
  end
  local err_hand
  err_hand = function(error_message)
    print(tostring(colored.red("ERROR:")) .. " " .. tostring(colored.bright(colored.yellow(colored.onred((error_message or ""))))))
    print("stack traceback:")
    local to_lua
    to_lua = require("moonscript.base").to_lua
    local nomsu_file = io.open("nomsu.moon")
    local nomsu_source = nomsu_file:read("*a")
    local _, line_table = to_lua(nomsu_source)
    nomsu_file:close()
    local level = 2
    while true do
      local _continue_0 = false
      repeat
        local calling_fn = debug.getinfo(level)
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
        do
          local metadata = nomsu.action_metadata[calling_fn.func]
          if metadata then
            line = colored.yellow(metadata.line_no)
            name = colored.bright(colored.yellow(metadata.aliases[1]))
          else
            if calling_fn.istailcall and not name then
              name = "<tail call>"
            end
            if calling_fn.short_src == "./nomsu.moon" then
              local char = line_table[calling_fn.currentline]
              local line_num = 1
              for _ in nomsu_source:sub(1, char):gmatch("\n") do
                line_num = line_num + 1
              end
              line = colored.cyan(tostring(calling_fn.short_src) .. ":" .. tostring(line_num))
              name = colored.bright(colored.cyan(name or "???"))
            else
              line = colored.blue(tostring(calling_fn.short_src) .. ":" .. tostring(calling_fn.currentline))
              name = colored.bright(colored.blue(name or "???"))
            end
          end
        end
        local _from = colored.dim(colored.white("|"))
        print(("%32s %s %s"):format(name, _from, line))
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
    return os.exit(false, true)
  end
  xpcall(run, err_hand)
end
return NomsuCompiler
