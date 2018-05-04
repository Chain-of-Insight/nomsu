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
    local contents = file:read("a")
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
  return io.popen("find \"" .. path .. "\" -type f -name \"*.nom\""):lines()
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
  local stub_defs, stub_pattern, var_pattern, _nomsu_chunk_counter
  local _base_0 = {
    define_action = function(self, signature, fn, is_macro)
      if is_macro == nil then
        is_macro = false
      end
      assert(type(fn) == 'function', "Bad fn: " .. tostring(repr(fn)))
      if type(signature) == 'string' then
        signature = {
          signature
        }
      elseif type(signature) ~= 'table' then
        error("Invalid signature, expected list of strings, but got: " .. tostring(repr(signature)), 0)
      end
      local stubs
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #signature do
          local alias = signature[_index_0]
          _accum_0[_len_0] = assert(stub_pattern:match(alias))
          _len_0 = _len_0 + 1
        end
        stubs = _accum_0
      end
      local stub_args
      do
        local _accum_0 = { }
        local _len_0 = 1
        for _index_0 = 1, #signature do
          local alias = signature[_index_0]
          _accum_0[_len_0] = assert(var_pattern:match(alias))
          _len_0 = _len_0 + 1
        end
        stub_args = _accum_0
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
        stub_args = assert(var_pattern:match(alias));
        (is_macro and self.environment.MACROS or self.environment.ACTIONS)[stub] = fn
        do
          local _accum_0 = { }
          local _len_0 = 1
          for _index_1 = 1, #stub_args do
            local a = stub_args[_index_1]
            _accum_0[_len_0] = fn_arg_positions[self:var_to_lua_identifier(a)]
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
      local ret = nil
      for filename in all_files(filename) do
        local _continue_0 = false
        repeat
          if filename:match("%.lua$") then
            local file = assert(FILE_CACHE[filename], "Could not find file: " .. tostring(filename))
            ret = self:run_lua(Lua(Source(filename), file))
          elseif filename:match("%.nom$") or filename:match("^/dev/fd/[012]$") then
            if not self.skip_precompiled then
              local lua_filename = filename:gsub("%.nom$", ".lua")
              local file = FILE_CACHE[lua_filename]
              if file then
                ret = self:run_lua(Lua(Source(filename), file))
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
          _continue_0 = true
        until true
        if not _continue_0 then
          break
        end
      end
      return ret
    end,
    use_file = function(self, filename)
      local loaded = self.environment.LOADED
      if not loaded[filename] then
        local ret = nil
        for filename in all_files(filename) do
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
            ret = loaded[filename]
            remove(self.use_stack)
          end
        end
        loaded[filename] = ret
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
      return tree:map(fn)
    end,
    tree_with_replaced_vars = function(self, tree, replacements)
      return tree:map(function(t)
        if t.type == "Var" then
          local id = tostring(t:as_lua(self))
          if replacements[id] ~= nil then
            return replacements[id]
          end
        end
      end)
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
      local nomsu = self
      self:define_compile_action("immediately %block", function(self, _block)
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
      self:define_compile_action("Lua %code", function(self, _code)
        local lua = Lua.Value(self.source, "Lua(", tostring(_code.source))
        add_lua_bits(lua, _code)
        lua:append(")")
        return lua
      end)
      self:define_compile_action("Lua %source %code", function(self, _source, _code)
        local lua = Lua.Value(self.source, "Lua(", _source:as_lua(nomsu))
        add_lua_bits(lua, _code)
        lua:append(")")
        return lua
      end)
      self:define_compile_action("Lua value %code", function(self, _code)
        local lua = Lua.Value(self.source, "Lua.Value(", tostring(_code.source))
        add_lua_bits(lua, _code)
        lua:append(")")
        return lua
      end)
      self:define_compile_action("Lua value %source %code", function(self, _source, _code)
        local lua = Lua.Value(self.source, "Lua.Value(", _source:as_lua(nomsu))
        add_lua_bits(lua, _code)
        lua:append(")")
        return lua
      end)
      self:define_compile_action("lua> %code", function(self, _code)
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
      self:define_compile_action("=lua %code", function(self, _code)
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
      self:define_compile_action("!! code location !!", function(self)
        return Lua.Value(self.source, repr(tostring(self.source)))
      end)
      self:define_action("run file %filename", function(_filename)
        return nomsu:run_file(_filename)
      end)
      return self:define_compile_action("use %path", function(self, _path)
        local path = nomsu:tree_to_value(_path)
        nomsu:use_file(path)
        return Lua(self.source, "nomsu:use_file(" .. tostring(repr(path)) .. ");")
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
          return function(...)
            return error("Attempt to run undefined action: " .. tostring(key), 0)
          end
        end
      })
      self.environment.MACROS = { }
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
    word = (NOMSU_DEFS.ident_char ^ 1 + NOMSU_DEFS.operator ^ 1),
    varname = (R('az', 'AZ', '09') + P('_') + NOMSU_DEFS.utf8_char) ^ 0
  }
  stub_pattern = re.compile([=[        {~ (%space->'') (('%' (%varname->'')) / %word)? ((%space->' ') (('%' (%varname->'')) / %word))* (%space->'') ~}
    ]=], stub_defs)
  var_pattern = re.compile("{| %space ((('%' {%varname}) / %word) %space)+ |}", stub_defs)
  _nomsu_chunk_counter = 0
  NomsuCompiler = _class_0
end
if arg and debug_getinfo(2).func ~= require then
  colors = require('consolecolors')
  local parser = re.compile([[        args <- {| (flag ";")* {:inputs: {| ({file} ";")* |} :} {:nomsu_args: {| ("--;" ({[^;]*} ";")*)? |} :} ";"? |} !.
        flag <-
            {:interactive: ("-i" -> true) :}
          / {:verbose: ("-v" -> true) :}
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
    -v Verbose mode.
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
        io.stderr:write(("%32s %s %s\n"):format(name, _from, line))
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
    if args.verbose then
      nomsu.debug = true
    end
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
          local formatted = tostring(tree:as_nomsu())
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
    local ldt = require('ldt')
    if ldt then
      ldt.guard(run)
    else
      xpcall(run, err_hand)
    end
  end
end
return NomsuCompiler
