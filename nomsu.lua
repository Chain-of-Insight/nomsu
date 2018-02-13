local lfs = require('lfs')
local re = require('re')
local lpeg = require('lpeg')
local utils = require('utils')
local new_uuid = require('uuid')
local immutable = require('immutable')
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
local _tuples = { }
local Tuple
Tuple = function(t)
  if not _tuples[#t] then
    _tuples[#t] = immutable(#t)
  end
  return _tuples[#t]:from_table(t)
end
local cached
cached = function(fn)
  local cache = setmetatable({ }, {
    __mode = "k"
  })
  return function(self, arg)
    if not (cache[arg]) then
      cache[arg] = fn(self, arg)
    end
    return cache[arg]
  end
end
do
  local STRING_METATABLE = getmetatable("")
  STRING_METATABLE.__add = function(self, other)
    return self .. stringify(other)
  end
  STRING_METATABLE.__index = function(self, i)
    if type(i) == 'number' then
      return string.sub(self, i, i)
    elseif type(i) == 'table' then
      return string.sub(self, i[1], i[2])
    else
      return string[i]
    end
  end
end
lpeg.setmaxstack(10000)
local P, R, V, S, Cg, C, Cp, B, Cmt
P, R, V, S, Cg, C, Cp, B, Cmt = lpeg.P, lpeg.R, lpeg.V, lpeg.S, lpeg.Cg, lpeg.C, lpeg.Cp, lpeg.B, lpeg.Cmt
local Types = { }
local _list_0 = {
  "File",
  "Nomsu",
  "Block",
  "List",
  "FunctionCall",
  "Text",
  "Dict",
  "Number",
  "Word",
  "Var",
  "Comment"
}
for _index_0 = 1, #_list_0 do
  local t = _list_0[_index_0]
  Types[t] = immutable({
    "id",
    "value"
  }, {
    type = t,
    name = t
  })
end
Types.DictEntry = immutable({
  "key",
  "value"
}, {
  name = "DictEntry"
})
Types.is_node = function(n)
  return type(n) == 'userdata' and n.type
end
local NOMSU_DEFS
do
  local _with_0 = { }
  _with_0.Tuple = Tuple
  _with_0.DictEntry = function(k, v)
    return Types.DictEntry(k, v)
  end
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
  _with_0.error = function(src, pos, err_msg)
    if lpeg.userdata.source_code:sub(pos, pos):match("[\r\n]") then
      pos = pos + #lpeg.userdata.source_code:match("[ \t\n\r]*", pos)
    end
    local line_no = 1
    while (lpeg.userdata.line_starts[line_no + 1] or math.huge) < pos do
      line_no = line_no + 1
    end
    local prev_line
    if line_no > 1 then
      prev_line = lpeg.userdata.source_code:match("[^\r\n]*", lpeg.userdata.line_starts[line_no - 1])
    else
      prev_line = ""
    end
    local err_line = lpeg.userdata.source_code:match("[^\r\n]*", lpeg.userdata.line_starts[line_no])
    local next_line
    if line_no < #lpeg.userdata.line_starts then
      next_line = lpeg.userdata.source_code:match("[^\r\n]*", lpeg.userdata.line_starts[line_no + 1])
    else
      next_line = ""
    end
    local pointer = ("-"):rep(pos - lpeg.userdata.line_starts[line_no]) .. "^"
    err_msg = (err_msg or "Parse error") .. " in " .. tostring(lpeg.userdata.filename) .. " on line " .. tostring(line_no) .. ":\n"
    err_msg = err_msg .. "\n" .. tostring(prev_line) .. "\n" .. tostring(err_line) .. "\n" .. tostring(pointer) .. "\n" .. tostring(next_line) .. "\n"
    return error(err_msg)
  end
  NOMSU_DEFS = _with_0
end
local node_id = 0
setmetatable(NOMSU_DEFS, {
  __index = function(self, key)
    local make_node
    make_node = function(start, value, stop)
      node_id = node_id + 1
      if type(value) == 'table' then
        error(value)
      end
      local node = Types[key](node_id, value)
      lpeg.userdata.tree_metadata[node] = {
        start = start,
        stop = stop,
        filename = lpeg.userdata.filename,
        source_code = lpeg.userdata.source_code
      }
      return node
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
  local line_counter, stub_defs, stub_pattern, var_pattern
  local _base_0 = {
    define_action = function(self, signature, source, fn)
      if self.debug then
        print(tostring(colored.bright("DEFINING ACTION:")) .. " " .. tostring(colored.green(repr(signature))))
      end
      if type(fn) ~= 'function' then
        error('function', "Bad fn: " .. tostring(repr(fn)))
      end
      if type(signature) == 'string' then
        signature = {
          signature
        }
      elseif type(signature) ~= 'table' or signature.type ~= nil then
        error("Invalid signature, expected list of strings, but got: " .. tostring(repr(signature)), 0)
      end
      local stubs = self:get_stubs_from_signature(signature)
      local stub_args = self:get_args_from_signature(signature)
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
      for sig_i = 1, #stubs do
        local stub, args = stubs[sig_i], stub_args[sig_i]
        if self.debug then
          print(tostring(colored.bright("ALIAS:")) .. " " .. tostring(colored.underscore(colored.magenta(repr(stub)))) .. " " .. tostring(colored.bright("WITH ARGS")) .. " " .. tostring(colored.dim(repr(args))) .. " ON: " .. tostring(self.environment.ACTIONS))
        end
        self.environment.ACTIONS[stub] = fn
        if not (fn_info.isvararg) then
          local arg_positions
          do
            local _accum_0 = { }
            local _len_0 = 1
            for _index_0 = 1, #args do
              local a = args[_index_0]
              _accum_0[_len_0] = fn_arg_positions[a]
              _len_0 = _len_0 + 1
            end
            arg_positions = _accum_0
          end
          if #arg_positions ~= #args then
            error("Mismatch in args between lua function's " .. tostring(repr(fn_arg_positions)) .. " and stub's " .. tostring(repr(args)) .. " for " .. tostring(repr(stub)), 0)
          end
          arg_orders[stub] = arg_positions
        end
      end
      self.action_metadata[fn] = {
        fn = fn,
        source = source,
        aliases = stubs,
        arg_orders = arg_orders,
        arg_positions = fn_arg_positions,
        def_number = self.__class.def_number
      }
    end,
    define_compile_action = function(self, signature, source, fn, src)
      self:define_action(signature, source, fn)
      self.action_metadata[fn].compile_time = true
    end,
    serialize_defs = function(self, scope, after)
      if scope == nil then
        scope = nil
      end
      if after == nil then
        after = nil
      end
      return error("Not currently functional.", 0)
    end,
    dedent = function(self, code)
      if not (code:find("\n")) then
        return code
      end
      local spaces, indent_spaces = math.huge, math.huge
      for line in code:gmatch("\n([^\n]*)") do
        local _continue_0 = false
        repeat
          if line:match("^%s*#.*") or line:match("^%s*$") then
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
      elseif indent_spaces ~= math.huge then
        return (code:gsub("\n" .. (" "):rep(indent_spaces), "\n    "))
      else
        return code
      end
    end,
    indent = function(self, code, levels)
      if levels == nil then
        levels = 1
      end
      return code:gsub("\n", "\n" .. ("    "):rep(levels))
    end,
    get_line_number = cached(function(self, tree)
      local metadata = self.tree_metadata[tree]
      if not (metadata) then
        error("Failed to find metatdata for tree: " .. tostring(tree), 0)
      end
      if not (self.file_metadata[metadata.filename]) then
        error("Failed to find file metatdata for file: " .. tostring(metadata.filename), 0)
      end
      local line_starts = self.file_metadata[metadata.filename].line_starts
      local first_line = 1
      while first_line < #line_starts and line_starts[first_line + 1] < metadata.start do
        first_line = first_line + 1
      end
      local last_line = first_line
      while last_line < #line_starts and line_starts[last_line + 1] < metadata.stop do
        last_line = last_line + 1
      end
      return tostring(metadata.filename) .. ":" .. tostring(first_line)
    end),
    get_source_code = function(self, tree)
      local metadata = self.tree_metadata[tree]
      if not (metadata) then
        return self:tree_to_nomsu(tree)
      end
      return self:dedent(metadata.source_code:sub(metadata.start, metadata.stop - 1))
    end,
    parse = function(self, nomsu_code, filename)
      assert(type(filename) == "string", "Bad filename type: " .. tostring(type(filename)))
      if self.debug then
        print(tostring(colored.bright("PARSING:")) .. "\n" .. tostring(colored.yellow(nomsu_code)))
      end
      if not (self.file_metadata[filename]) then
        self.file_metadata[filename] = {
          source_code = nomsu_code,
          filename = filename,
          line_starts = line_counter:match(nomsu_code)
        }
      end
      local userdata = {
        source_code = nomsu_code,
        filename = filename,
        indent_stack = {
          ""
        },
        tree_metadata = self.tree_metadata,
        line_starts = self.file_metadata[filename].line_starts
      }
      local old_userdata
      old_userdata, lpeg.userdata = lpeg.userdata, userdata
      local tree = NOMSU:match(nomsu_code)
      lpeg.userdata = old_userdata
      assert(tree, "In file " .. tostring(colored.blue(filename)) .. " failed to parse:\n" .. tostring(colored.onyellow(colored.black(nomsu_code))))
      if self.debug then
        print("PARSE TREE:")
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
          return error("Execution quota exceeded. Your code took too long.", 0)
        end
        debug.sethook(timeout, "", max_operations)
      end
      local tree = self:parse(src, filename)
      assert(tree, "Failed to parse: " .. tostring(src))
      assert(tree.type == "File", "Attempt to run non-file: " .. tostring(tree.type))
      local lua = self:tree_to_lua(tree)
      local lua_code = lua.statements or (lua.expr .. ";")
      if lua_code.locals and #lua_code.locals > 0 then
        lua_code = "local " .. concat(lua_code.locals, ", ") .. ";\n" .. lua_code
      end
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
      local file_attributes = assert(lfs.attributes(filename), "File not found: " .. tostring(filename))
      if file_attributes.mode == "directory" then
        for short_filename in lfs.dir(filename) do
          local full_filename = filename .. '/' .. short_filename
          local attr = lfs.attributes(full_filename)
          if attr.mode ~= "directory" and short_filename:match(".*%.nom") then
            self:run_file(full_filename)
          end
        end
        return 
      end
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
          error("File does not exist: " .. tostring(filename), 0)
        end
        local nomsu_code = file:read('*a')
        file:close()
        return self:run(nomsu_code, filename)
      else
        return error("Invalid filetype for " .. tostring(filename), 0)
      end
    end,
    use_file = function(self, filename)
      local loaded = self.environment.LOADED
      if not loaded[filename] then
        for i, f in ipairs(self.use_stack) do
          if f == filename then
            local loop
            do
              local _accum_0 = { }
              local _len_0 = 1
              for j = i, #self.use_stack do
                _accum_0[_len_0] = self.use_stack[j]
                _len_0 = _len_0 + 1
              end
              loop = _accum_0
            end
            insert(loop, filename)
            error("Circular import, this loops forever: " .. tostring(concat(loop, " -> ")))
          end
        end
        insert(self.use_stack, filename)
        loaded[filename] = self:run_file(filename) or true
      end
      return loaded[filename]
    end,
    run_lua = function(self, lua_code)
      local run_lua_fn, err = load(lua_code, nil, nil, self.environment)
      if self.debug then
        print(tostring(colored.bright("RUNNING LUA:")) .. "\n" .. tostring(colored.blue(colored.bright(lua_code))))
      end
      if not run_lua_fn then
        local n = 1
        local fn
        fn = function()
          n = n + 1
          return ("\n%-3d|"):format(n)
        end
        local code = "1  |" .. lua_code:gsub("\n", fn)
        error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(colored.onblack(code)))) .. "\n\n" .. tostring(err), 0)
      end
      return run_lua_fn()
    end,
    tree_to_value = function(self, tree, filename)
      if tree.type == 'Text' and #tree.value == 1 and type(tree.value[1]) == 'string' then
        return tree.value[1]
      end
      local code = "return " .. tostring(self:tree_to_lua(tree).expr) .. ";"
      if self.debug then
        print(tostring(colored.bright("RUNNING LUA TO GET VALUE:")) .. "\n" .. tostring(colored.blue(colored.bright(code))))
      end
      local lua_thunk, err = load(code, nil, nil, self.environment)
      if not lua_thunk then
        error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(colored.onblack(code)))) .. "\n\n" .. tostring(colored.red(err)), 0)
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
      assert(Types.is_node(tree), "Invalid tree: " .. tostring(repr(tree)))
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
          local _list_1 = tok.value
          for _index_0 = 1, #_list_1 do
            local bit = _list_1[_index_0]
            local nomsu = inline_expression(bit)
            if not (nomsu) then
              return nil
            end
            insert(bits, nomsu)
          end
          return "[" .. concat(bits, ", ") .. "]"
        elseif "Dict" == _exp_0 then
          local bits = { }
          local _list_1 = tok.value
          for _index_0 = 1, #_list_1 do
            local bit = _list_1[_index_0]
            local key_nomsu
            if bit.key.type == "Word" then
              key_nomsu = bit.key.value
            else
              key_nomsu = inline_expression(bit.key)
            end
            if not (key_nomsu) then
              return nil
            end
            if bit.key.type == "FunctionCall" then
              key_nomsu = "(" .. key_nomsu .. ")"
            end
            local value_nomsu = inline_expression(bit.value)
            if not (value_nomsu) then
              return nil
            end
            insert(bits, key_nomsu .. "=" .. value_nomsu)
          end
          return "{" .. concat(bits, ", ") .. "}"
        elseif "Text" == _exp_0 then
          local buff = '"'
          local _list_1 = tok.value
          for _index_0 = 1, #_list_1 do
            local bit = _list_1[_index_0]
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
          local _list_1 = tok.value
          for _index_0 = 1, #_list_1 do
            local line = _list_1[_index_0]
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
          local _list_1 = tok.value
          for _index_0 = 1, #_list_1 do
            local bit = _list_1[_index_0]
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
          local _list_1 = tok.value
          for _index_0 = 1, #_list_1 do
            local bit = _list_1[_index_0]
            local key_nomsu = inline_expression(bit.key)
            if not (key_nomsu) then
              return nil
            end
            if bit.key.type == "FunctionCall" then
              key_nomsu = "(" .. key_nomsu .. ")"
            end
            local value_nomsu = inline_expression(bit.value)
            if value_nomsu and #key_nomsu + #value_nomsu < max_line then
              line = line .. (key_nomsu .. "=" .. value_nomsu .. ",")
              if #line >= max_line then
                buff = buff .. line
                line = "\n    "
              end
            else
              line = line .. (key_nomsu .. "=" .. expression(bit.value))
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
          local _list_1 = tok.value
          for _index_0 = 1, #_list_1 do
            local bit = _list_1[_index_0]
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
          local _list_1 = tree.value
          for _index_0 = 1, #_list_1 do
            local line = _list_1[_index_0]
            nomsu = expression(line)
            if not (nomsu) then
              local src = self:get_source_code(line)
              error("Failed to produce output for:\n" .. tostring(colored.yellow(src)), 0)
            end
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
              _accum_0[_len_0] = tostring(self:value_to_nomsu(k)) .. ":" .. tostring(self:value_to_nomsu(v))
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
        return error("Unsupported value_to_nomsu type: " .. tostring(type(value)), 0)
      end
    end,
    tree_to_lua = function(self, tree)
      assert(tree, "No tree provided.")
      if not Types.is_node(tree) then
        error("Invalid tree: " .. tostring(repr(tree)), 0)
      end
      local _exp_0 = tree.type
      if "File" == _exp_0 then
        if #tree.value == 1 then
          return self:tree_to_lua(tree.value[1])
        end
        local declared_locals = { }
        local lua_bits = { }
        local line_no = 1
        local _list_1 = tree.value
        for _index_0 = 1, #_list_1 do
          local line = _list_1[_index_0]
          local lua = self:tree_to_lua(line)
          if not lua then
            error("No lua produced by " .. tostring(repr(line)), 0)
          end
          if lua.locals then
            local new_locals
            do
              local _accum_0 = { }
              local _len_0 = 1
              local _list_2 = lua.locals
              for _index_1 = 1, #_list_2 do
                local l = _list_2[_index_1]
                if not declared_locals[l] then
                  _accum_0[_len_0] = l
                  _len_0 = _len_0 + 1
                end
              end
              new_locals = _accum_0
            end
            if #new_locals > 0 then
              insert(lua_bits, "local " .. tostring(concat(new_locals, ", ")) .. ";")
              for _index_1 = 1, #new_locals do
                local l = new_locals[_index_1]
                declared_locals[l] = true
              end
            end
          end
          if lua.statements then
            insert(lua_bits, lua.statements)
          elseif lua.expr then
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
          expr = "nomsu:parse(" .. tostring(repr(self:get_source_code(tree.value))) .. ", " .. tostring(repr(self:get_line_number(tree.value))) .. ").value[1]"
        }
      elseif "Block" == _exp_0 then
        local lua_bits = { }
        local locals = { }
        local _list_1 = tree.value
        for _index_0 = 1, #_list_1 do
          local arg = _list_1[_index_0]
          local lua = self:tree_to_lua(arg)
          if #tree.value == 1 and lua.expr and not lua.statements then
            return {
              expr = lua.expr,
              locals = lua.locals
            }
          end
          if lua.locals then
            local _list_2 = lua.locals
            for _index_1 = 1, #_list_2 do
              local l = _list_2[_index_1]
              table.insert(locals, l)
            end
          end
          if lua.statements then
            insert(lua_bits, lua.statements)
          elseif lua.expr then
            insert(lua_bits, tostring(lua.expr) .. ";")
          end
        end
        utils.deduplicate(locals)
        return {
          statements = concat(lua_bits, "\n"),
          locals = (#locals > 0 and locals or nil)
        }
      elseif "FunctionCall" == _exp_0 then
        insert(self.compilestack, tree)
        local stub = self:tree_to_stub(tree)
        local ok, fn = pcall(function()
          return self.environment.ACTIONS[stub]
        end)
        if not ok then
          fn = nil
        end
        local metadata = self.action_metadata[fn]
        if metadata and metadata.compile_time then
          local args
          do
            local _accum_0 = { }
            local _len_0 = 1
            local _list_1 = tree.value
            for _index_0 = 1, #_list_1 do
              local arg = _list_1[_index_0]
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
              local _list_1 = metadata.arg_orders[stub]
              for _index_0 = 1, #_list_1 do
                local p = _list_1[_index_0]
                _accum_0[_len_0] = args[p]
                _len_0 = _len_0 + 1
              end
              new_args = _accum_0
            end
            args = new_args
          end
          if self.debug then
            print(tostring(colored.bright("RUNNING MACRO")) .. " " .. tostring(colored.underscore(colored.magenta(stub))) .. " ")
            print(tostring(colored.bright("WITH ARGS:")) .. " " .. tostring(colored.dim(concat((function()
              local _accum_0 = { }
              local _len_0 = 1
              for _index_0 = 1, #args do
                local a = args[_index_0]
                _accum_0[_len_0] = (repr(a)):sub(1, 100)
                _len_0 = _len_0 + 1
              end
              return _accum_0
            end)(), ", "))))
          end
          local lua = fn(unpack(args))
          remove(self.compilestack)
          return lua
        elseif not metadata and self.__class.math_patt:match(stub) then
          local bits = { }
          local _list_1 = tree.value
          for _index_0 = 1, #_list_1 do
            local tok = _list_1[_index_0]
            if tok.type == "Word" then
              insert(bits, tok.value)
            else
              local lua = self:tree_to_lua(tok)
              if not (lua.expr) then
                local src = self:get_source_code(tok)
                error("non-expression value inside math expression: " .. tostring(colored.yellow(src)))
              end
              insert(bits, lua.expr)
            end
          end
          remove(self.compilestack)
          return {
            expr = "(" .. tostring(concat(bits, " ")) .. ")"
          }
        end
        local args = { }
        local _list_1 = tree.value
        for _index_0 = 1, #_list_1 do
          local _continue_0 = false
          repeat
            local tok = _list_1[_index_0]
            if tok.type == "Word" then
              _continue_0 = true
              break
            end
            local lua = self:tree_to_lua(tok)
            if not (lua.expr) then
              local line = self:get_line_number(tok)
              local src = self:get_source_code(tok)
              error(tostring(line) .. ": Cannot use:\n" .. tostring(colored.yellow(src)) .. "\nas an argument to " .. tostring(stub) .. ", since it's not an expression, it produces: " .. tostring(repr(lua)), 0)
            end
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
            local _list_2 = metadata.arg_orders[stub]
            for _index_0 = 1, #_list_2 do
              local p = _list_2[_index_0]
              _accum_0[_len_0] = args[p]
              _len_0 = _len_0 + 1
            end
            new_args = _accum_0
          end
          args = new_args
        end
        remove(self.compilestack)
        return {
          expr = self.__class:comma_separated_items("ACTIONS[" .. tostring(repr(stub)) .. "](", args, ")")
        }
      elseif "Text" == _exp_0 then
        local concat_parts = { }
        local string_buffer = ""
        local _list_1 = tree.value
        for _index_0 = 1, #_list_1 do
          local _continue_0 = false
          repeat
            local bit = _list_1[_index_0]
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
              print(colored.bright("INTERP:"))
              self:print_tree(bit)
              print(tostring(colored.bright("EXPR:")) .. " " .. tostring(lua.expr) .. ", " .. tostring(colored.bright("STATEMENT:")) .. " " .. tostring(lua.statements))
            end
            if not (lua.expr) then
              local line = self:get_line_number(bit)
              local src = self:get_source_code(bit)
              error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(bit)) .. " as a string interpolation value, since it's not an expression.", 0)
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
        local _list_1 = tree.value
        for _index_0 = 1, #_list_1 do
          local item = _list_1[_index_0]
          local lua = self:tree_to_lua(item)
          if not (lua.expr) then
            local line = self:get_line_number(item)
            local src = self:get_source_code(item)
            error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as a list item, since it's not an expression.", 0)
          end
          insert(items, lua.expr)
        end
        return {
          expr = self.__class:comma_separated_items("{", items, "}")
        }
      elseif "Dict" == _exp_0 then
        local items = { }
        local _list_1 = tree.value
        for _index_0 = 1, #_list_1 do
          local entry = _list_1[_index_0]
          local key_lua
          if entry.key.type == "Word" then
            key_lua = {
              expr = repr(entry.key.value)
            }
          else
            key_lua = self:tree_to_lua(entry.key)
          end
          if not (key_lua.expr) then
            local line = self:get_line_number(entry.key)
            local src = self:get_source_code(entry.key)
            error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as a dict key, since it's not an expression.", 0)
          end
          local value_lua = self:tree_to_lua(entry.value)
          if not (value_lua.expr) then
            local line = self:get_line_number(entry.value)
            local src = self:get_source_code(entry.value)
            error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as a dict value, since it's not an expression.", 0)
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
        return error("Unknown/unimplemented thingy: " .. tostring(tree.type), 0)
      end
    end,
    walk_tree = function(self, tree, depth)
      if depth == nil then
        depth = 0
      end
      coroutine.yield(tree, depth)
      if not (Types.is_node(tree)) then
        return 
      end
      local _exp_0 = tree.type
      if "List" == _exp_0 or "File" == _exp_0 or "Block" == _exp_0 or "FunctionCall" == _exp_0 or "Text" == _exp_0 then
        local _list_1 = tree.value
        for _index_0 = 1, #_list_1 do
          local v = _list_1[_index_0]
          self:walk_tree(v, depth + 1)
        end
      elseif "Dict" == _exp_0 then
        local _list_1 = tree.value
        for _index_0 = 1, #_list_1 do
          local e = _list_1[_index_0]
          self:walk_tree(e.key, depth + 1)
          self:walk_tree(e.value, depth + 1)
        end
      else
        self:walk_tree(tree.value, depth + 1)
      end
      return nil
    end,
    print_tree = function(self, tree)
      io.write(colors.bright .. colors.green)
      for node, depth in coroutine.wrap(function()
        return self:walk_tree(tree)
      end) do
        if Types.is_node(node) then
          print(tostring(("    "):rep(depth)) .. tostring(node.type) .. ":")
        else
          print(("    "):rep(depth) .. repr(node))
        end
      end
      return io.write(colors.reset)
    end,
    tree_to_str = function(self, tree)
      local bits = { }
      for node, depth in coroutine.wrap(function()
        return self:walk_tree(tree)
      end) do
        if Types.is_node(node) then
          insert(bits, (tostring(("    "):rep(depth)) .. tostring(node.type) .. ":"))
        else
          insert(bits, (("    "):rep(depth) .. repr(node)))
        end
      end
      return concat(bits, "\n")
    end,
    tree_map = function(self, tree, fn)
      if not (Types.is_node(tree)) then
        return tree
      end
      local replacement = fn(tree)
      if replacement ~= nil then
        return replacement
      end
      local _exp_0 = tree.type
      if "File" == _exp_0 or "Nomsu" == _exp_0 or "Block" == _exp_0 or "List" == _exp_0 or "FunctionCall" == _exp_0 or "Text" == _exp_0 then
        local new_values, is_changed = { }, false
        for i, old_value in ipairs(tree.value) do
          local new_value = type(old_value) ~= "string" and self:tree_map(old_value, fn) or nil
          if new_value ~= nil and new_value ~= old_value then
            is_changed = true
            new_values[i] = new_value
          else
            new_values[i] = old_value
          end
        end
        if is_changed then
          local new_tree = getmetatable(tree)(tree.id, Tuple(new_values))
          self.tree_metadata[new_tree] = self.tree_metadata[tree]
          return new_tree
        end
      elseif "Dict" == _exp_0 then
        local new_values, is_changed = { }, false
        for i, e in ipairs(tree.value) do
          local new_key = self:tree_map(e.key, fn)
          local new_value = self:tree_map(e.value, fn)
          if (new_key ~= nil and new_key ~= e.key) or (new_value ~= nil and new_value ~= e.value) then
            is_changed = true
            new_values[i] = DictEntry(new_key, new_value)
          else
            new_values[i] = e
          end
        end
        if is_changed then
          local new_tree = getmetatable(tree)(tree.id, Tuple(new_values))
          self.tree_metadata[new_tree] = self.tree_metadata[tree]
          return new_tree
        end
      elseif nil == _exp_0 then
        error("Invalid tree: " .. tostring(repr(tree)))
      end
      return tree
    end,
    tree_with_replaced_vars = function(self, tree, replacements)
      return self:tree_map(tree, function(t)
        if t.type == "Var" then
          local id = self:var_to_lua_identifier(t.value)
          if replacements[id] ~= nil then
            return replacements[id]
          end
        end
      end)
    end,
    tree_to_stub = cached(function(self, tree)
      if tree.type ~= "FunctionCall" then
        error("Tried to get stub from non-functioncall tree: " .. tostring(tree.type), 0)
      end
      return concat((function()
        local _accum_0 = { }
        local _len_0 = 1
        local _list_1 = tree.value
        for _index_0 = 1, #_list_1 do
          local t = _list_1[_index_0]
          _accum_0[_len_0] = (t.type == "Word" and t.value or "%")
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), " ")
    end),
    tree_to_named_stub = cached(function(self, tree)
      if tree.type ~= "FunctionCall" then
        error("Tried to get stub from non-functioncall tree: " .. tostring(tree.type), 0)
      end
      return concat((function()
        local _accum_0 = { }
        local _len_0 = 1
        local _list_1 = tree.value
        for _index_0 = 1, #_list_1 do
          local t = _list_1[_index_0]
          _accum_0[_len_0] = (t.type == "Word" and t.value or "%" .. tostring(t.value))
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), " ")
    end),
    get_stubs_from_signature = function(self, signature)
      if type(signature) ~= 'table' or signature.type then
        error("Invalid signature: " .. tostring(repr(signature)), 0)
      end
      local stubs = { }
      for i, alias in ipairs(signature) do
        if type(alias) ~= 'string' then
          error("Expected entries in signature to be strings, not " .. tostring(type(alias)) .. "s like: " .. tostring(repr(alias)) .. "\nsignature: " .. tostring(repr(signature)), 0)
        end
        stubs[i] = stub_pattern:match(alias)
        if not (stubs[i]) then
          error("Failed to match stub pattern on alias: " .. tostring(repr(alias)))
        end
      end
      return stubs
    end,
    get_args_from_signature = function(self, signature)
      if type(signature) ~= 'table' or signature.type then
        error("Invalid signature: " .. tostring(repr(signature)), 0)
      end
      local stub_args = { }
      for i, alias in ipairs(signature) do
        if type(alias) ~= 'string' then
          error("Invalid type for signature: " .. tostring(type(alias)) .. " for:\n" .. tostring(repr(alias)), 0)
        end
        local args = var_pattern:match(alias)
        if not (args) then
          error("Failed to match arg pattern on alias: " .. tostring(repr(alias)), 0)
        end
        for j = 1, #args do
          args[j] = self:var_to_lua_identifier(args[j])
        end
        stub_args[i] = args
      end
      return stub_args
    end,
    var_to_lua_identifier = function(self, var)
      if Types.Var:is_instance(var) then
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
    initialize_core = function(self)
      local get_line_no
      get_line_no = function()
        return "nomsu.moon:" .. tostring(debug.getinfo(2).currentline)
      end
      local nomsu = self
      local nomsu_string_as_lua
      nomsu_string_as_lua = function(code)
        local concat_parts = { }
        local _list_1 = code.value
        for _index_0 = 1, #_list_1 do
          local bit = _list_1[_index_0]
          if type(bit) == "string" then
            insert(concat_parts, bit)
          else
            local lua = nomsu:tree_to_lua(bit)
            if not (lua.expr) then
              local line = self:get_line_number(bit)
              local src = self:get_source_code(bit)
              error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as a string interpolation value, since it's not an expression.", 0)
            end
            insert(concat_parts, lua.expr)
          end
        end
        return concat(concat_parts)
      end
      self:define_compile_action("immediately %block", get_line_no(), function(_block)
        local lua = nomsu:tree_to_lua(_block)
        local lua_code = lua.statements or (lua.expr .. ";")
        if lua.locals and #lua.locals > 0 then
          lua_code = "local " .. tostring(concat(lua.locals, ", ")) .. ";\n" .. tostring(lua_code)
        end
        nomsu:run_lua(lua_code)
        return {
          statements = "if IMMEDIATE then\n" .. tostring(lua_code) .. "\nend",
          locals = lua.locals
        }
      end)
      self:define_compile_action("lua> %code", get_line_no(), function(_code)
        if _code.type == "Text" then
          local lua = nomsu_string_as_lua(_code)
          return {
            statements = lua
          }
        else
          return {
            statements = "nomsu:run_lua(" .. tostring(nomsu:tree_to_lua(_code).expr) .. ");"
          }
        end
      end)
      self:define_compile_action("=lua %code", get_line_no(), function(_code)
        local lua = nomsu_string_as_lua(_code)
        return {
          expr = lua
        }
      end)
      self:define_compile_action("!! code location !!", get_line_no(), function()
        local tree = nomsu.compilestack[#nomsu.compilestack - 1]
        local metadata = self.tree_metadata[tree]
        return {
          expr = repr(tostring(metadata.filename) .. ":" .. tostring(metadata.start) .. "," .. tostring(metadata.stop))
        }
      end)
      self:define_action("run file %filename", get_line_no(), function(_filename)
        return nomsu:run_file(_filename)
      end)
      return self:define_compile_action("use %filename", get_line_no(), function(_filename)
        local filename = nomsu:tree_to_value(_filename)
        nomsu:use_file(filename)
        return {
          expr = "nomsu:use_file(" .. tostring(repr(filename)) .. ")"
        }
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
      self.use_stack = { }
      self.compilestack = { }
      self.file_metadata = setmetatable({ }, {
        __mode = "k"
      })
      self.tree_metadata = setmetatable({ }, {
        __mode = "k"
      })
      self.action_metadata = setmetatable({ }, {
        __mode = "k"
      })
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
  line_counter = re.compile([[        lines <- {| line (%nl line)* |}
        line <- {} (!%nl .)*
    ]], {
    nl = NOMSU_DEFS.nl
  })
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
  stub_defs = {
    space = (P(' ') + P('\n..')) ^ 0,
    word = (NOMSU_DEFS.ident_char ^ 1 + NOMSU_DEFS.operator ^ 1),
    varname = (R('az', 'AZ', '09') + P('_') + NOMSU_DEFS.utf8_char) ^ 0
  }
  stub_pattern = re.compile("{~ (%space->'') (('%' (%varname->'')) / %word)? ((%space->' ') (('%' (%varname->'')) / %word))* (%space->'') ~}", stub_defs)
  var_pattern = re.compile("{| %space ((('%' {%varname}) / %word) %space)+ |}", stub_defs)
  NomsuCompiler = _class_0
end
if arg and debug.getinfo(2).func ~= require then
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
        nomsu.environment.print = function() end
        compiled_output = io.output()
      elseif args.output then
        compiled_output = io.open(args.output, 'w')
      end
      if args.input:match(".*%.lua") then
        local retval = dofile(args.input)(nomsu, { })
      else
        local retval, code
        if args.input == '-' then
          retval, code = nomsu:run(io.read('*a'), 'stdin')
        else
          retval, code = nomsu:run_file(args.input)
        end
        if compiled_output then
          compiled_output:write("local IMMEDIATE = true;\n")
          compiled_output:write(code)
        end
      end
      if args.flags["-p"] then
        nomsu.environment.print = print
      end
    end
    if args.flags["-i"] then
      nomsu:run('use "core"', "stdin")
      while true do
        io.write(colored.bright(colored.yellow(">> ")))
        local buff = ""
        while true do
          local line = io.read("*L")
          if line == "\n" or not line then
            break
          end
          line = line:gsub("\t", "    ")
          buff = buff .. line
          io.write(colored.dim(colored.yellow(".. ")))
        end
        if #buff == 0 then
          break
        end
        local ok, ret = pcall(function()
          return nomsu:run(buff, "stdin")
        end)
        if ok and ret ~= nil then
          print("= " .. repr(ret))
        elseif not ok then
          print(colored.bright(colored.red(ret)))
        end
      end
    end
  end
  local err_hand
  err_hand = function(error_message)
    print(tostring(colored.red("ERROR:")) .. " " .. tostring(colored.bright(colored.yellow(colored.onred((error_message or ""))))))
    print("stack traceback:")
    local ok, to_lua = pcall(function()
      return require('moonscript.base').to_lua
    end)
    if not ok then
      to_lua = function()
        return nil
      end
    end
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
            local filename, start, stop = metadata.source:match("([^:]*):([0-9]*),([0-9]*)")
            if filename then
              local file = io.open(filename):read("*a")
              local line_no = 1
              for _ in file:sub(1, tonumber(start)):gmatch("\n") do
                line_no = line_no + 1
              end
              local offending_statement = file:sub(tonumber(start), tonumber(stop))
              if #offending_statement > 50 then
                offending_statement = offending_statement:sub(1, 50) .. "..."
              end
              offending_statement = colored.red(offending_statement)
              line = colored.yellow(filename .. ":" .. tostring(line_no) .. "\n    " .. offending_statement)
            else
              line = colored.yellow(metadata.source)
            end
            name = colored.bright(colored.yellow(metadata.aliases[1]))
          else
            if calling_fn.istailcall and not name then
              name = "<tail call>"
            end
            if calling_fn.short_src == "./nomsu.moon" and line_table then
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
