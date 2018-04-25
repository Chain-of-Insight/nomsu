local lfs = require('lfs')
local re = require('re')
local lpeg = require('lpeg')
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
  local _obj_0 = require("lua_obj")
  Nomsu, Lua, Source = _obj_0.Nomsu, _obj_0.Lua, _obj_0.Source
end
FILE_CACHE = setmetatable({ }, {
  __index = function(self, filename)
    local file = io.open(filename)
    if not (file) then
      return nil
    end
    local contents = file:read("a"):sub(1, -2)
    file:close()
    self[filename] = contents
    return contents
  end
})
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
    if type(i) == 'number' then
      return string.sub(self, i, i)
    elseif type(i) == 'table' then
      return string.sub(self, i[1], i[2])
    else
      return string[i]
    end
  end
end
local Types = require("nomsu_tree")
local NOMSU_DEFS
do
  local _with_0 = { }
  _with_0.Tuple = function(values)
    return Tuple(table.unpack(values))
  end
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
    if src:sub(pos, pos):match("[\r\n]") then
      pos = pos + #src:match("[ \t\n\r]*", pos)
    end
    local line_no = 1
    local text_loc = lpeg.userdata.source_code.source:sub(pos, pos)
    line_no = text_loc:get_line_number()
    src = FILE_CACHE[text_loc.filename]
    local prev_line = src:sub(LINE_STARTS[src][line_no - 1] or 1, LINE_STARTS[src][line_no] - 1)
    local err_line = src:sub(LINE_STARTS[src][line_no], (LINE_STARTS[src][line_no + 1] or 0) - 1)
    local next_line = src:sub(LINE_STARTS[src][line_no + 1] or -1, (LINE_STARTS[src][line_no + 2] or 0) - 1)
    local pointer = ("-"):rep(pos - LINE_STARTS[src][line_no]) .. "^"
    err_msg = (err_msg or "Parse error") .. " in " .. tostring(lpeg.userdata.source_code.source.filename) .. " on line " .. tostring(line_no) .. ":\n"
    err_msg = err_msg .. "\n" .. tostring(prev_line) .. "\n" .. tostring(err_line) .. "\n" .. tostring(pointer) .. "\n" .. tostring(next_line) .. "\n"
    return error(err_msg)
  end
  NOMSU_DEFS = _with_0
end
setmetatable(NOMSU_DEFS, {
  __index = function(self, key)
    local make_node
    make_node = function(start, value, stop)
      if type(value) == 'table' then
        error("Not a tuple: " .. tostring(repr(value)))
      end
      local source = lpeg.userdata.source_code.source
      start = start + (source.start - 1)
      stop = stop + (source.start - 1)
      source = Source(source.filename, start, stop - 1)
      local node = Types[key](value, source)
      return node
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
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- ({} %3 {}) -> %2"
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    comment <- "--" [^%nl]*
    ]])
  local nomsu_peg = peg_tidier:match(FILE_CACHE["nomsu.peg"])
  NOMSU_PATTERN = re.compile(nomsu_peg, NOMSU_DEFS)
end
local NomsuCompiler
do
  local _class_0
  local _nomsu_chunk_counter, stub_defs, stub_pattern, var_pattern
  local _base_0 = {
    define_action = function(self, signature, source, fn)
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
      local fn_info = debug_getinfo(fn, "u")
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
        }
      }
      local old_userdata
      old_userdata, lpeg.userdata = lpeg.userdata, userdata
      local tree = NOMSU_PATTERN:match(tostring(nomsu_code))
      lpeg.userdata = old_userdata
      assert(tree, "In file " .. tostring(colored.blue(filename)) .. " failed to parse:\n" .. tostring(colored.onyellow(colored.black(nomsu_code))))
      return tree
    end,
    run = function(self, nomsu_code, compile_fn)
      if compile_fn == nil then
        compile_fn = nil
      end
      if #nomsu_code == 0 then
        return nil
      end
      local tree = self:parse(nomsu_code)
      assert(tree, "Failed to parse: " .. tostring(nomsu_code))
      assert(tree.type == "File", "Attempt to run non-file: " .. tostring(tree.type))
      local lua = tree:as_lua(self)
      lua:convert_to_statements()
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
      local file_attributes = assert(lfs.attributes(filename), "File not found: " .. tostring(filename))
      if file_attributes.mode == "directory" then
        for short_filename in lfs.dir(filename) do
          local full_filename = filename .. '/' .. short_filename
          local attr = lfs.attributes(full_filename)
          if attr.mode ~= "directory" and short_filename:match(".*%.nom") then
            self:run_file(full_filename, compile_fn)
          end
        end
        return 
      end
      if filename:match(".*%.lua") then
        local file = assert(FILE_CACHE[filename], "Could not find file: " .. tostring(filename))
        return self:run_lua(Lua(Source(filename), file))
      end
      if filename:match(".*%.nom") then
        if not self.skip_precompiled then
          local lua_filename = filename:gsub("%.nom$", ".lua")
          local file = FILE_CACHE[lua_filename]
          if file then
            return self:run_lua(Lua(Source(filename), file))
          end
        end
        local file = file or FILE_CACHE[filename]
        if not file then
          error("File does not exist: " .. tostring(filename), 0)
        end
        return self:run(Nomsu(Source(filename), file), compile_fn)
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
    run_lua = function(self, lua)
      assert(type(lua) ~= 'string', "Attempt to run lua string instead of Lua (object)")
      local lua_string = tostring(lua)
      if rawget(FILE_CACHE, lua.source.filename) == nil then
        FILE_CACHE[lua.source.filename] = lua_string
      end
      if rawget(FILE_CACHE, lua.source) == nil then
        FILE_CACHE[lua.source] = lua_string
      end
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
    tree_to_value = function(self, tree)
      if tree.type == 'Text' and #tree.value == 1 and type(tree.value[1]) == 'string' then
        return tree.value[1]
      end
      local lua = Lua(tree.source, "return ", tree:as_lua(self), ";")
      return self:run_lua(lua)
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
    walk_tree = function(self, tree, depth)
      if depth == nil then
        depth = 0
      end
      coroutine.yield(tree, depth)
      if not (Types.is_node(tree)) then
        return 
      end
      local _exp_0 = tree.type
      if "List" == _exp_0 or "File" == _exp_0 or "Block" == _exp_0 or "Action" == _exp_0 or "Text" == _exp_0 or "IndexChain" == _exp_0 then
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local v = _list_0[_index_0]
          self:walk_tree(v, depth + 1)
        end
      elseif "Dict" == _exp_0 then
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local e = _list_0[_index_0]
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
      if "File" == _exp_0 or "Nomsu" == _exp_0 or "Block" == _exp_0 or "List" == _exp_0 or "Action" == _exp_0 or "Text" == _exp_0 or "IndexChain" == _exp_0 then
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
          return tree:with_value(Tuple(table.unpack(new_values)))
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
          return tree:with_value(Tuple(table.unpack(new_values)))
        end
      elseif nil == _exp_0 then
        error("Invalid tree: " .. tostring(repr(tree)))
      end
      return tree
    end,
    tree_with_replaced_vars = function(self, tree, replacements)
      return self:tree_map(tree, function(t)
        if t.type == "Var" then
          local id = tostring(t:as_lua(self))
          if replacements[id] ~= nil then
            return replacements[id]
          end
        end
      end)
    end,
    tree_to_stub = function(self, tree)
      if tree.type ~= "Action" then
        error("Tried to get stub from non-functioncall tree: " .. tostring(tree.type), 0)
      end
      return concat((function()
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local t = _list_0[_index_0]
          _accum_0[_len_0] = (t.type == "Word" and t.value or "%")
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), " ")
    end,
    tree_to_named_stub = function(self, tree)
      if tree.type ~= "Action" then
        error("Tried to get stub from non-functioncall tree: " .. tostring(tree.type), 0)
      end
      return concat((function()
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local t = _list_0[_index_0]
          _accum_0[_len_0] = (t.type == "Word" and t.value or "%" .. tostring(t.value))
          _len_0 = _len_0 + 1
        end
        return _accum_0
      end)(), " ")
    end,
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
        return "nomsu.moon:" .. tostring(debug_getinfo(2).currentline)
      end
      local nomsu = self
      self:define_compile_action("immediately %block", get_line_no(), function(self, _block)
        local lua = _block:as_lua(nomsu)
        lua:convert_to_statements()
        lua:declare_locals()
        nomsu:run_lua(lua)
        return Lua(self.source, "if IMMEDIATE then\n    ", lua, "\nend")
      end)
      local add_lua_bits
      add_lua_bits = function(lua, code)
        if code.type ~= "Text" then
          lua:append(", ", code:as_lua(nomsu))
          return 
        end
        local _list_0 = code.value
        for _index_0 = 1, #_list_0 do
          local bit = _list_0[_index_0]
          lua:append(", ")
          if type(bit) == "string" then
            lua:append(repr(bit))
          else
            local bit_lua = bit:as_lua(nomsu)
            if not (bit_lua.is_value) then
              local line, src = bit.source:get_line(), bit.source:get_text()
              error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as a string interpolation value, since it's not an expression.")
            end
            lua:append(bit_lua)
          end
        end
      end
      self:define_compile_action("Lua %code", get_line_no(), function(self, _code)
        local lua = Lua.Value(self.source, "Lua(", tostring(_code.source))
        add_lua_bits(lua, _code)
        lua:append(")")
        return lua
      end)
      self:define_compile_action("Lua %source %code", get_line_no(), function(self, _source, _code)
        local lua = Lua.Value(self.source, "Lua(", _source:as_lua(nomsu))
        add_lua_bits(lua, _code)
        lua:append(")")
        return lua
      end)
      self:define_compile_action("Lua value %code", get_line_no(), function(self, _code)
        local lua = Lua.Value(self.source, "Lua.Value(", tostring(_code.source))
        add_lua_bits(lua, _code)
        lua:append(")")
        return lua
      end)
      self:define_compile_action("Lua value %source %code", get_line_no(), function(self, _source, _code)
        local lua = Lua.Value(self.source, "Lua.Value(", _source:as_lua(nomsu))
        add_lua_bits(lua, _code)
        lua:append(")")
        return lua
      end)
      self:define_compile_action("lua> %code", get_line_no(), function(self, _code)
        if _code.type ~= "Text" then
          return Lua.Value(self.source, "nomsu:run_lua(Lua(", repr(_code.source), ", ", repr(tostring(_code:as_lua(nomsu))), "))")
        end
        local lua = Lua(_code.source)
        local _list_0 = _code.value
        for _index_0 = 1, #_list_0 do
          local bit = _list_0[_index_0]
          if type(bit) == "string" then
            lua:append(bit)
          else
            local bit_lua = bit:as_lua(nomsu)
            if not (bit_lua.is_value) then
              local line, src = bit.source:get_line(), bit.source:get_text()
              error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as a string interpolation value, since it's not an expression.", 0)
            end
            lua:append(bit_lua)
          end
        end
        return lua
      end)
      self:define_compile_action("=lua %code", get_line_no(), function(self, _code)
        if _code.type ~= "Text" then
          return Lua.Value(self.source, "nomsu:run_lua(Lua(", repr(_code.source), ", ", repr(tostring(_code:as_lua(nomsu))), "))")
        end
        local lua = Lua.Value(self.source)
        local _list_0 = _code.value
        for _index_0 = 1, #_list_0 do
          local bit = _list_0[_index_0]
          if type(bit) == "string" then
            lua:append(bit)
          else
            local bit_lua = bit:as_lua(nomsu)
            if not (lua.is_value) then
              local line, src = bit.source:get_line(), bit.source:get_text()
              error(tostring(line) .. ": Cannot use " .. tostring(colored.yellow(src)) .. " as a string interpolation value, since it's not an expression.", 0)
            end
            lua:append(bit_lua)
          end
        end
        return lua
      end)
      self:define_compile_action("!! code location !!", get_line_no(), function(self)
        return Lua.Value(self.source, repr(tostring(self.source)))
      end)
      self:define_action("run file %filename", get_line_no(), function(_filename)
        return nomsu:run_file(_filename)
      end)
      return self:define_compile_action("use %filename", get_line_no(), function(self, _filename)
        local filename = nomsu:tree_to_value(_filename)
        nomsu:use_file(filename)
        return Lua.Value(self.source, "nomsu:use_file(" .. tostring(repr(filename)) .. ")")
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
      self.file_metadata = setmetatable({ }, {
        __mode = "k"
      })
      self.action_metadata = setmetatable({ }, {
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
      for k, v in pairs(Types) do
        self.environment[k] = v
      end
      self.environment.Tuple = Tuple
      self.environment.Lua = Lua
      self.environment.Nomsu = Nomsu
      self.environment.Source = Source
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
  _nomsu_chunk_counter = 0
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
if arg and debug_getinfo(2).func ~= require then
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
        local metadata = nomsu.action_metadata[info.func]
        if metadata then
          info.name = metadata.aliases[1]
          local _ = [=[                filename = if type(metadata.source) == 'string'
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
      end
    end
    return info
  end
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
      local compile_fn = nil
      if args.flags["-p"] then
        nomsu.environment.print = function() end
        compile_fn = function(code)
          return io.output():write("local IMMEDIATE = true;\n" .. tostring(code))
        end
      elseif args.output then
        compile_fn = function(code)
          return io.open(args.output, 'w'):write("local IMMEDIATE = true;\n" .. tostring(code))
        end
      end
      if args.input:match(".*%.lua") then
        dofile(args.input)(nomsu, { })
      else
        if args.input == "-" then
          nomsu:run(io.read('a'), compile_fn)
        else
          nomsu:run_file(args.input, compile_fn)
        end
      end
      if args.flags["-p"] then
        nomsu.environment.print = print
      end
    end
    if args.flags["-i"] then
      nomsu:run('use "core"')
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
        local ret
        ok, ret = pcall(nomsu.run, nomsu, buff)
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
    local level = 2
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
        do
          local metadata = nomsu.action_metadata[calling_fn.func]
          if metadata then
            local filename, start, stop = metadata.source:match("([^:]*):([0-9]*),([0-9]*)")
            if filename then
              local file = FILE_CACHE[filename]
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
