local re = require('re')
local lpeg = require('lpeg')
local utils = require('utils')
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
if _VERSION == "Lua 5.1" then
  local xp = xpcall
  local xpcall
  xpcall = function(f, errhandler, ...)
    local args = {
      n = select("#", ...),
      ...
    }
    return xp(function(...)
      return f(unpack(args, 1, args.n))
    end), errhandler
  end
end
lpeg.setmaxstack(10000)
local P, R, V, S, Cg, C, Cp, B, Cmt
P, R, V, S, Cg, C, Cp, B, Cmt = lpeg.P, lpeg.R, lpeg.V, lpeg.S, lpeg.Cg, lpeg.C, lpeg.Cp, lpeg.B, lpeg.Cmt
local STRING_ESCAPES = {
  n = "\n",
  t = "\t",
  b = "\b",
  a = "\a",
  v = "\v",
  f = "\f",
  r = "\r"
}
local DIGIT, HEX = R('09'), R('09', 'af', 'AF')
local ESCAPE_CHAR = (P("\\") * S("xX") * C(HEX * HEX)) / function(self)
  return string.char(tonumber(self, 16))
end
ESCAPE_CHAR = ESCAPE_CHAR + ((P("\\") * C(DIGIT * (DIGIT ^ -2))) / function(self)
  return string.char(tonumber(self))
end)
ESCAPE_CHAR = ESCAPE_CHAR + ((P("\\") * C(S("ntbavfr"))) / STRING_ESCAPES)
local OPERATOR_CHAR = S("'~`!@$^&*-+=|<>?/")
local UTF8_CHAR = (R("\194\223") * R("\128\191") + R("\224\239") * R("\128\191") * R("\128\191") + R("\240\244") * R("\128\191") * R("\128\191") * R("\128\191"))
local IDENT_CHAR = R("az", "AZ", "09") + P("_") + UTF8_CHAR
local parse
do
  local ctx = { }
  local indent_patt = P(function(self, start)
    local spaces = self:match("[ \t]*", start)
    if #spaces > ctx.indent_stack[#ctx.indent_stack] then
      insert(ctx.indent_stack, #spaces)
      return start + #spaces
    end
  end)
  local dedent_patt = P(function(self, start)
    local spaces = self:match("[ \t]*", start)
    if #spaces < ctx.indent_stack[#ctx.indent_stack] then
      remove(ctx.indent_stack)
      return start
    end
  end)
  local nodent_patt = P(function(self, start)
    local spaces = self:match("[ \t]*", start)
    if #spaces == ctx.indent_stack[#ctx.indent_stack] then
      return start + #spaces
    end
  end)
  local gt_nodent_patt = P(function(self, start)
    local spaces = self:match("[ \t]*", start)
    if #spaces >= ctx.indent_stack[#ctx.indent_stack] + 4 then
      return start + ctx.indent_stack[#ctx.indent_stack] + 4
    end
  end)
  local defs = {
    nl = P("\n"),
    ws = S(" \t"),
    tonumber = tonumber,
    operator = OPERATOR_CHAR,
    print = function(src, pos, msg)
      return print(msg, pos, repr(src:sub(math.max(0, pos - 16), math.max(0, pos - 1)) .. "|" .. src:sub(pos, pos + 16))) or true
    end,
    utf8_char = (R("\194\223") * R("\128\191") + R("\224\239") * R("\128\191") * R("\128\191") + R("\240\244") * R("\128\191") * R("\128\191") * R("\128\191")),
    indented = indent_patt,
    nodented = nodent_patt,
    dedented = dedent_patt,
    gt_nodented = gt_nodent_patt,
    escape_char = ESCAPE_CHAR,
    error = function(src, pos, err_msg)
      if ctx.source_code:sub(pos, pos) == "\n" then
        pos = pos + #ctx.source_code:match("[ \t\n]*", pos)
      end
      local line_no = 1
      while (ctx.line_starts[line_no + 1] or math.huge) < pos do
        line_no = line_no + 1
      end
      local prev_line = line_no > 1 and ctx.source_code:match("[^\n]*", ctx.line_starts[line_no - 1]) or ""
      local err_line = ctx.source_code:match("[^\n]*", ctx.line_starts[line_no])
      local next_line = line_no < #ctx.line_starts and ctx.source_code:match("[^\n]*", ctx.line_starts[line_no + 1]) or ""
      local pointer = ("-"):rep(pos - ctx.line_starts[line_no]) .. "^"
      err_msg = (err_msg or "Parse error") .. " in " .. tostring(ctx.filename) .. " on line " .. tostring(line_no) .. ":\n"
      err_msg = err_msg .. "\n" .. tostring(prev_line) .. "\n" .. tostring(err_line) .. "\n" .. tostring(pointer) .. "\n" .. tostring(next_line) .. "\n"
      return error(err_msg)
    end,
    FunctionCall = function(start, value, stop)
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
      local line_no = 1
      while (ctx.line_starts[line_no + 1] or math.huge) < start do
        line_no = line_no + 1
      end
      local src = ctx.source_code:sub(start, stop - 1)
      return {
        type = "FunctionCall",
        src = src,
        line_no = tostring(ctx.filename) .. ":" .. tostring(line_no),
        value = value,
        stub = stub
      }
    end
  }
  setmetatable(defs, {
    __index = function(self, key)
      local make_node
      make_node = function(start, value, stop)
        return {
          start = start,
          stop = stop,
          value = value,
          src = ctx.source_code:sub(start, stop - 1),
          type = key
        }
      end
      self[key] = make_node
      return make_node
    end
  })
  local peg_tidier = re.compile([[    file <- {~ %nl* (def/comment) (%nl+ (def/comment))* %nl* ~}
    def <- anon_def / captured_def
    anon_def <- ({ident} (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- %2"
    captured_def <- ({ident} (" "*) "(" {ident} ")" (" "*) ":"
        {((%nl " "+ [^%nl]*)+) / ([^%nl]*)}) -> "%1 <- ({} %3 {}) -> %2"
    ident <- [a-zA-Z_][a-zA-Z0-9_]*
    comment <- "--" [^%nl]*
    ]])
  local nomsu = peg_tidier:match(io.open("nomsu.peg"):read("*a"))
  nomsu = re.compile(nomsu, defs)
  parse = function(source_code, filename)
    local old_ctx = ctx
    ctx = {
      source_code = source_code,
      filename = filename,
      indent_stack = {
        0
      }
    }
    ctx.line_starts = re.compile("lines <- {| line ('\n' line)* |} line <- {} [^\n]*"):match(source_code)
    local tree = nomsu:match(source_code)
    ctx = old_ctx
    return tree
  end
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
    def = function(self, signature, fn, src, is_macro)
      if is_macro == nil then
        is_macro = false
      end
      if type(signature) == 'string' then
        signature = self:get_stubs({
          signature
        })
      elseif type(signature) == 'table' and type(signature[1]) == 'string' then
        signature = self:get_stubs(signature)
      end
      self:assert(type(fn) == 'function', "Bad fn: " .. tostring(repr(fn)))
      local canonical_args = nil
      local canonical_escaped_args = nil
      local aliases = { }
      self.__class.def_number = self.__class.def_number + 1
      local def = {
        fn = fn,
        src = src,
        is_macro = is_macro,
        aliases = { },
        def_number = self.__class.def_number,
        defs = self.defs
      }
      local where_defs_go = (getmetatable(self.defs) or { }).__newindex or self.defs
      for sig_i = 1, #signature do
        local stub, arg_names, escaped_args = unpack(signature[sig_i])
        self:assert(stub, "NO STUB FOUND: " .. tostring(repr(signature)))
        if self.debug then
          self:writeln(tostring(colored.bright("DEFINING RULE:")) .. " " .. tostring(colored.underscore(colored.magenta(repr(stub)))) .. " " .. tostring(colored.bright("WITH ARGS")) .. " " .. tostring(colored.dim(repr(arg_names))))
        end
        for i = 1, #arg_names - 1 do
          for j = i + 1, #arg_names do
            if arg_names[i] == arg_names[j] then
              self:error("Duplicate argument in function " .. tostring(stub) .. ": '" .. tostring(arg_names[i]) .. "'")
            end
          end
        end
        if canonical_args then
          self:assert(equivalent(set(arg_names), canonical_args), "Mismatched args")
        else
          canonical_args = set(arg_names)
        end
        if canonical_escaped_args then
          self:assert(equivalent(escaped_args, canonical_escaped_args), "Mismatched escaped args")
        else
          canonical_escaped_args = escaped_args
          def.escaped_args = escaped_args
        end
        insert(def.aliases, stub)
        local stub_def = setmetatable({
          stub = stub,
          arg_names = arg_names,
          escaped_args = escaped_args
        }, {
          __index = def
        })
        rawset(where_defs_go, stub, stub_def)
      end
    end,
    defmacro = function(self, signature, fn, src)
      return self:def(signature, fn, src, true)
    end,
    scoped = function(self, thunk)
      local old_defs = self.defs
      local new_defs = {
        ["#vars"] = setmetatable({ }, {
          __index = self.defs["#vars"]
        }),
        ["#loaded_files"] = setmetatable({ }, {
          __index = self.defs["#loaded_files"]
        })
      }
      self.defs = setmetatable(new_defs, {
        __index = old_defs
      })
      local ok, ret1, ret2 = pcall(thunk, self)
      self.defs = old_defs
      if not ok then
        self:error(ret1)
      end
      return ret1, ret2
    end,
    serialize_defs = function(self, scope, after)
      if scope == nil then
        scope = nil
      end
      if after == nil then
        after = nil
      end
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
    call = function(self, stub, line_no, ...)
      local def = self.defs[stub]
      if def and def.is_macro and self.callstack[#self.callstack] ~= "#macro" then
        self:error("Attempt to call macro at runtime: " .. tostring(stub) .. "\nThis can be caused by using a macro in a function that is defined before the macro.")
      end
      insert(self.callstack, {
        stub,
        line_no
      })
      if not (def) then
        self:error("Attempt to call undefined function: " .. tostring(stub))
      end
      if not (def.is_macro) then
        self:assert_permission(stub)
      end
      local fn, arg_names
      fn, arg_names = def.fn, def.arg_names
      local args
      do
        local _tbl_0 = { }
        for i, name in ipairs(arg_names) do
          _tbl_0[name] = select(i, ...)
        end
        args = _tbl_0
      end
      if self.debug then
        self:write(tostring(colored.bright("CALLING")) .. " " .. tostring(colored.magenta(colored.underscore(stub))) .. " ")
        self:writeln(tostring(colored.bright("WITH ARGS:")))
        for name, value in pairs(args) do
          self:writeln("  " .. tostring(colored.bright("* " .. tostring(name))) .. " = " .. tostring(colored.dim(repr(value))))
        end
      end
      local old_defs
      old_defs, self.defs = self.defs, def.defs
      local rets = {
        fn(self, args)
      }
      self.defs = old_defs
      remove(self.callstack)
      return unpack(rets)
    end,
    run_macro = function(self, tree)
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
      if self.debug then
        self:write(tostring(colored.bright("RUNNING MACRO")) .. " " .. tostring(colored.underscore(colored.magenta(tree.stub))) .. " ")
        self:writeln(tostring(colored.bright("WITH ARGS:")) .. " " .. tostring(colored.dim(repr(args))))
      end
      insert(self.callstack, "#macro")
      local ret = self:call(tree.stub, tree.line_no, unpack(args))
      remove(self.callstack)
      return ret
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
    assert_permission = function(self, stub)
      local fn_def = self.defs[stub]
      if not (fn_def) then
        self:error("Undefined function: " .. tostring(fn_name))
      end
      local whiteset = fn_def.whiteset
      if whiteset == nil then
        return true
      end
      local _list_0 = self.callstack
      for _index_0 = 1, #_list_0 do
        local caller = _list_0[_index_0]
        if caller ~= "#macro" and whiteset[caller[1]] then
          return true
        end
      end
      return self:error("You do not have the authority to call: " .. tostring(stub))
    end,
    check_permission = function(self, fn_def)
      if getmetatable(fn_def) ~= functiondef_mt then
        local fn_name = fn_def
        fn_def = self.defs[fn_name]
        if fn_def == nil then
          self:error("Undefined function: " .. tostring(fn_name))
        end
      end
      local whiteset = fn_def.whiteset
      if whiteset == nil then
        return true
      end
      local _list_0 = self.callstack
      for _index_0 = 1, #_list_0 do
        local caller = _list_0[_index_0]
        if caller ~= "#macro" and whiteset[caller[1]] then
          return true
        end
      end
      return false
    end,
    parse = function(self, str, filename)
      if self.debug then
        self:writeln(tostring(colored.bright("PARSING:")) .. "\n" .. tostring(colored.yellow(str)))
      end
      str = str:gsub("\r", "")
      local tree = parse(str, filename)
      self:assert(tree, "In file " .. tostring(colored.blue(filename)) .. " failed to parse:\n" .. tostring(colored.onyellow(colored.black(str))))
      if self.debug then
        self:writeln("PARSE TREE:")
        self:print_tree(tree, "    ")
      end
      return tree
    end,
    run = function(self, src, filename, vars, max_operations, output_file)
      if vars == nil then
        vars = { }
      end
      if max_operations == nil then
        max_operations = nil
      end
      if output_file == nil then
        output_file = nil
      end
      if src == "" then
        return nil, "", vars
      end
      if max_operations then
        local timeout
        timeout = function()
          debug.sethook()
          return self:error("Execution quota exceeded. Your code took too long.")
        end
        debug.sethook(timeout, "", max_operations)
      end
      local tree = self:parse(src, filename)
      self:assert(tree, "Failed to parse: " .. tostring(src))
      self:assert(tree.type == "File", "Attempt to run non-file: " .. tostring(tree.type))
      local lua = self:tree_to_lua(tree, filename)
      local lua_code = lua.statements or (lua.expr .. ";")
      lua_code = "-- File: " .. tostring(filename) .. "\n" .. lua_code
      local ret = self:run_lua(lua_code, vars)
      if max_operations then
        debug.sethook()
      end
      if output_file then
        output_file:write(lua_code)
      end
      return ret, lua_code, vars
    end,
    run_file = function(self, filename, vars)
      if vars == nil then
        vars = { }
      end
      if filename:match(".*%.lua") then
        return dofile(filename)(self, vars)
      end
      if filename:match(".*%.nom") then
        if not self.skip_precompiled then
          local file = io.open(filename:gsub("%.nom", ".lua"), "r")
          if file then
            local lua_code = file:read("*a")
            file:close()
            return self:run_lua(lua_code, vars)
          end
        end
        local file = file or io.open(filename)
        if not file then
          self:error("File does not exist: " .. tostring(filename))
        end
        local nomsu_code = file:read('*a')
        file:close()
        return self:run(nomsu_code, filename)
      else
        return self:error("Invalid filetype for " .. tostring(filename))
      end
    end,
    require_file = function(self, filename, vars)
      if vars == nil then
        vars = { }
      end
      local loaded = self.defs["#loaded_files"]
      if not loaded[filename] then
        loaded[filename] = self:run_file(filename, vars) or true
      end
      return loaded[filename]
    end,
    run_lua = function(self, lua_code, vars)
      if vars == nil then
        vars = { }
      end
      local load_lua_fn, err = load(([[return function(nomsu, vars)
    %s
end]]):format(lua_code))
      if not load_lua_fn then
        local n = 1
        local fn
        fn = function()
          n = n + 1
          return ("\n%-3d|"):format(n)
        end
        local code = "1  |" .. lua_code:gsub("\n", fn)
        self:error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(colored.onblack(code)))) .. "\n\n" .. tostring(err))
      end
      local run_lua_fn = load_lua_fn()
      local ok, ret = pcall(run_lua_fn, self, vars)
      if not ok then
        self:errorln(debug.traceback())
        self:error(ret)
      end
      return ret
    end,
    tree_to_value = function(self, tree, vars, filename)
      local code = "return (function(nomsu, vars)\nreturn " .. tostring(self:tree_to_lua(tree, filename).expr) .. ";\nend);"
      code = "-- Tree to value: " .. tostring(filename) .. "\n" .. code
      if self.debug then
        self:writeln(tostring(colored.bright("RUNNING LUA TO GET VALUE:")) .. "\n" .. tostring(colored.blue(colored.bright(code))))
      end
      local lua_thunk, err = load(code)
      if not lua_thunk then
        self:error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(colored.onblack(code)))) .. "\n\n" .. tostring(colored.red(err)))
      end
      return (lua_thunk())(self, vars or { })
    end,
    tree_to_nomsu = function(self, tree, force_inline)
      if force_inline == nil then
        force_inline = false
      end
      self:assert(tree, "No tree provided.")
      if not tree.type then
        self:errorln(debug.traceback())
        self:error("Invalid tree: " .. tostring(repr(tree)))
      end
      local _exp_0 = tree.type
      if "File" == _exp_0 then
        return concat((function()
          local _accum_0 = { }
          local _len_0 = 1
          local _list_0 = tree.value
          for _index_0 = 1, #_list_0 do
            local v = _list_0[_index_0]
            _accum_0[_len_0] = self:tree_to_nomsu(v, force_inline)
            _len_0 = _len_0 + 1
          end
          return _accum_0
        end)(), "\n"), false
      elseif "Nomsu" == _exp_0 then
        local inside, inline = self:tree_to_nomsu(tree.value, force_inline)
        return "\\" .. tostring(inside), inline
      elseif "Block" == _exp_0 then
        if force_inline then
          return "(:" .. tostring(concat((function()
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = tree.value
            for _index_0 = 1, #_list_0 do
              local v = _list_0[_index_0]
              _accum_0[_len_0] = self:tree_to_nomsu(v, true)
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)(), "; ")) .. ")", true
        else
          return ":" .. self:indent("\n" .. concat((function()
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = tree.value
            for _index_0 = 1, #_list_0 do
              local v = _list_0[_index_0]
              _accum_0[_len_0] = self:tree_to_nomsu(v)
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)(), "\n")), false
        end
      elseif "FunctionCall" == _exp_0 then
        local buff = ""
        local sep = ""
        local inline = true
        local line_len = 0
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local arg = _list_0[_index_0]
          local nomsu, arg_inline = self:tree_to_nomsu(arg, force_inline)
          if sep == " " and line_len + #nomsu > 80 then
            sep = "\n.."
          end
          if not (sep == " " and not arg_inline and nomsu:sub(1, 1) == ":") then
            buff = buff .. sep
          end
          if arg_inline then
            sep = " "
            line_len = line_len + (1 + #nomsu)
          else
            line_len = 0
            inline = false
            sep = "\n.."
          end
          if arg.type == 'FunctionCall' then
            if arg_inline then
              buff = buff .. "(" .. tostring(nomsu) .. ")"
            else
              buff = buff .. "(..)\n    " .. tostring(self:indent(nomsu))
            end
          else
            buff = buff .. nomsu
          end
        end
        return buff, inline
      elseif "String" == _exp_0 then
        local buff = "\""
        local longbuff = "\"..\"\n    |"
        local inline = true
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local bit = _list_0[_index_0]
          if type(bit) == "string" then
            bit = bit:gsub("\\", "\\\\")
            buff = buff .. bit:gsub("\n", "\\n"):gsub("\"", "\\\"")
            longbuff = longbuff .. bit:gsub("\n", "\n    |")
          else
            local inside, bit_inline = self:tree_to_nomsu(bit, force_inline)
            inline = inline and bit_inline
            buff = buff .. "\\(" .. tostring(inside) .. ")"
            longbuff = longbuff .. "\\(" .. tostring(inside) .. ")"
          end
        end
        buff = buff .. "\""
        if force_inline or (inline and #buff <= 90) then
          return buff, true
        else
          return longbuff, false
        end
      elseif "List" == _exp_0 then
        local buff = "["
        local longbuff = "[..]\n    "
        local longsep = ""
        local longline = 0
        local inline = true
        for i, bit in ipairs(tree.value) do
          local nomsu, bit_inline = self:tree_to_nomsu(bit, force_inline)
          inline = inline and bit_inline
          if inline then
            if i > 1 then
              buff = buff .. ", "
            end
            buff = buff .. nomsu
          end
          longbuff = longbuff .. (longsep .. nomsu)
          longline = longline + #nomsu
          if bit_inline and longline <= 90 then
            longsep = ", "
          else
            longsep = "\n    "
          end
        end
        buff = buff .. "]"
        if force_inline or (inline and #buff <= 90) then
          return buff, true
        else
          return longbuff, false
        end
      elseif "Dict" == _exp_0 then
        return self:error("Sorry, not yet implemented.")
      elseif "Number" == _exp_0 then
        return repr(tree.value), true
      elseif "Var" == _exp_0 then
        return "%" .. tostring(tree.value), true
      elseif "Word" == _exp_0 then
        return tree.value, true
      else
        return self:error("Unknown/unimplemented thingy: " .. tostring(tree.type))
      end
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
    tree_to_lua = function(self, tree, filename)
      self:assert(tree, "No tree provided.")
      if not tree.type then
        self:errorln(debug.traceback())
        self:error("Invalid tree: " .. tostring(repr(tree)))
      end
      local _exp_0 = tree.type
      if "File" == _exp_0 then
        if #tree.value == 1 then
          return self:tree_to_lua(tree.value[1], filename)
        end
        local lua_bits = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local line = _list_0[_index_0]
          local lua = self:tree_to_lua(line, filename)
          if not lua then
            self:error("No lua produced by " .. tostring(repr(line)))
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
      elseif "Nomsu" == _exp_0 then
        return {
          expr = "nomsu:parse(" .. tostring(repr(tree.value.src)) .. ", " .. tostring(repr(tree.line_no)) .. ").value[1]"
        }
      elseif "Block" == _exp_0 then
        local lua_bits = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local arg = _list_0[_index_0]
          local lua = self:tree_to_lua(arg, filename)
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
        local def = self.defs[tree.stub]
        if def and def.is_macro then
          local lua = self:run_macro(tree)
          remove(self.compilestack)
          return lua
        elseif not def and self.__class.math_patt:match(tree.stub) then
          local bits = { }
          local _list_0 = tree.value
          for _index_0 = 1, #_list_0 do
            local tok = _list_0[_index_0]
            if tok.type == "Word" then
              insert(bits, tok.value)
            else
              local lua = self:tree_to_lua(tok, filename)
              self:assert(lua.statements == nil, "non-expression value inside math expression")
              insert(bits, lua.expr)
            end
          end
          remove(self.compilestack)
          return {
            expr = "(" .. tostring(concat(bits, " ")) .. ")"
          }
        end
        local args = {
          repr(tree.stub),
          repr(tree.line_no)
        }
        local arg_names, escaped_args
        if def then
          arg_names, escaped_args = def.arg_names, def.escaped_args
        else
          arg_names, escaped_args = (function()
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = tree.value
            for _index_0 = 1, #_list_0 do
              local w = _list_0[_index_0]
              if w.type == "Word" then
                _accum_0[_len_0] = w.value
                _len_0 = _len_0 + 1
              end
            end
            return _accum_0
          end)(), { }
        end
        local arg_num = 1
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local _continue_0 = false
          repeat
            local arg = _list_0[_index_0]
            if arg.type == 'Word' then
              _continue_0 = true
              break
            end
            if escaped_args[arg_names[arg_num]] then
              insert(args, "nomsu:parse(" .. tostring(repr(arg.src)) .. ", " .. tostring(repr(tree.line_no)) .. ").value[1]")
            else
              local lua = self:tree_to_lua(arg, filename)
              if lua.statements then
                self:error("Cannot use [[" .. tostring(arg.src) .. "]] as a function argument to " .. tostring(tree.stub) .. ", since it's not an expression.")
              end
              insert(args, lua.expr)
            end
            arg_num = arg_num + 1
            _continue_0 = true
          until true
          if not _continue_0 then
            break
          end
        end
        remove(self.compilestack)
        return {
          expr = self.__class:comma_separated_items("nomsu:call(", args, ")")
        }
      elseif "String" == _exp_0 then
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
            local lua = self:tree_to_lua(bit, filename)
            if self.debug then
              self:writeln((colored.bright("INTERP:")))
              self:print_tree(bit)
              self:writeln(tostring(colored.bright("EXPR:")) .. " " .. tostring(lua.expr) .. ", " .. tostring(colored.bright("STATEMENT:")) .. " " .. tostring(lua.statements))
            end
            if lua.statements then
              self:error("Cannot use [[" .. tostring(bit.src) .. "]] as a string interpolation value, since it's not an expression.")
            end
            insert(concat_parts, "nomsu:stringify(" .. tostring(lua.expr) .. ")")
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
          local lua = self:tree_to_lua(item, filename)
          if lua.statements then
            self:error("Cannot use [[" .. tostring(item.src) .. "]] as a list item, since it's not an expression.")
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
            key_lua = self:tree_to_lua(entry.dict_key, filename)
          end
          if key_lua.statements then
            self:error("Cannot use [[" .. tostring(entry.dict_key.src) .. "]] as a dict key, since it's not an expression.")
          end
          local value_lua = self:tree_to_lua(entry.dict_value, filename)
          if value_lua.statements then
            self:error("Cannot use [[" .. tostring(entry.dict_value.src) .. "]] as a dict value, since it's not an expression.")
          end
          local key_str = key_lua.expr:match([=[["']([a-zA-Z_][a-zA-Z0-9_]*)['"]]=])
          if key_str then
            insert(items, tostring(key_str) .. "=" .. tostring(value_lua.expr))
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
        if tree.value:match("^[a-zA-Z_][a-zA-Z0-9_]*$") then
          return {
            expr = "vars." .. tostring(tree.value)
          }
        else
          return {
            expr = "vars[" .. tostring(repr(tree.value)) .. "]"
          }
        end
      else
        return self:error("Unknown/unimplemented thingy: " .. tostring(tree.type))
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
      if "List" == _exp_0 or "File" == _exp_0 or "Block" == _exp_0 or "FunctionCall" == _exp_0 or "String" == _exp_0 then
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
    replaced_vars = function(self, tree, vars)
      if type(tree) ~= 'table' then
        return tree
      end
      local _exp_0 = tree.type
      if "Var" == _exp_0 then
        if vars[tree.value] ~= nil then
          tree = vars[tree.value]
        end
      elseif "File" == _exp_0 or "Nomsu" == _exp_0 or "Block" == _exp_0 or "List" == _exp_0 or "FunctionCall" == _exp_0 or "String" == _exp_0 then
        local new_value = self:replaced_vars(tree.value, vars)
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
        local replacements = { }
        for i, e in ipairs(tree.value) do
          local new_key = self:replaced_vars(e.dict_key, vars)
          local new_value = self:replaced_vars(e.dict_value, vars)
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
          new_values[k] = self:replaced_vars(v, vars)
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
        self:error("Nothing to get stub from")
      end
      if type(x) == 'string' then
        local spec = concat(self.__class.stub_patt:match(x), " ")
        local stub = spec:gsub("%%%S+", "%%"):gsub("\\", "")
        local arg_names
        do
          local _accum_0 = { }
          local _len_0 = 1
          for arg in spec:gmatch("%%(%S*)") do
            _accum_0[_len_0] = arg
            _len_0 = _len_0 + 1
          end
          arg_names = _accum_0
        end
        local escaped_args
        do
          local _tbl_0 = { }
          for arg in spec:gmatch("\\%%(%S*)") do
            _tbl_0[arg] = true
          end
          escaped_args = _tbl_0
        end
        return stub, arg_names, escaped_args
      end
      if type(x) ~= 'table' then
        self:error("Invalid type for getting stub: " .. tostring(type(x)) .. " for:\n" .. tostring(repr(x)))
      end
      local _exp_0 = x.type
      if "String" == _exp_0 then
        return self:get_stub(x.value)
      elseif "FunctionCall" == _exp_0 then
        return self:get_stub(x.src)
      else
        return self:error("Unsupported get stub type: " .. tostring(x.type) .. " for " .. tostring(repr(x)))
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
      return (var:gsub("%W", function(verboten)
        if verboten == "_" then
          return "__"
        else
          return ("_%x"):format(verboten:byte())
        end
      end))
    end,
    assert = function(self, condition, msg)
      if msg == nil then
        msg = ''
      end
      if not condition then
        return self:error(msg)
      end
    end,
    error = function(self, msg)
      local error_msg = colored.red("ERROR!")
      if msg then
        error_msg = error_msg .. ("\n" .. (colored.bright(colored.yellow(colored.onred(msg)))))
      end
      error_msg = error_msg .. "\nCallstack:"
      local maxlen = max((function()
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = self.callstack
        for _index_0 = 1, #_list_0 do
          local c = _list_0[_index_0]
          if c ~= "#macro" then
            _accum_0[_len_0] = #c[2]
            _len_0 = _len_0 + 1
          end
        end
        return _accum_0
      end)())
      for i = #self.callstack, 1, -1 do
        if self.callstack[i] ~= "#macro" then
          local line_no = self.callstack[i][2]
          if line_no then
            local nums
            do
              local _accum_0 = { }
              local _len_0 = 1
              for n in line_no:gmatch(":([0-9]+)") do
                _accum_0[_len_0] = tonumber(n)
                _len_0 = _len_0 + 1
              end
              nums = _accum_0
            end
            line_no = line_no:gsub(":.*$", ":" .. tostring(sum(nums) - #nums + 1))
          end
          error_msg = error_msg .. "\n    " .. tostring(("%-" .. tostring(maxlen) .. "s"):format(line_no)) .. "| " .. tostring(self.callstack[i][1])
        end
      end
      error_msg = error_msg .. "\n    <top level>"
      self.callstack = { }
      return error(error_msg, 3)
    end,
    typecheck = function(self, vars, varname, desired_type)
      local x = vars[varname]
      local x_type = type(x)
      if x_type == desired_type then
        return x
      end
      if x_type == 'table' then
        x_type = x.type or x_type
        if x_type == desired_type then
          return x
        end
      end
      return self:error("Invalid type for %" .. tostring(varname) .. ". Expected " .. tostring(desired_type) .. ", but got " .. tostring(x_type) .. ":\n" .. tostring(repr(x)))
    end,
    source_code = function(self, level)
      if level == nil then
        level = 0
      end
      return self:dedent(self.compilestack[#self.compilestack - level].src)
    end,
    initialize_core = function(self)
      local nomsu_string_as_lua
      nomsu_string_as_lua = function(self, code)
        local concat_parts = { }
        local _list_0 = code.value
        for _index_0 = 1, #_list_0 do
          local bit = _list_0[_index_0]
          if type(bit) == "string" then
            insert(concat_parts, bit)
          else
            local lua = self:tree_to_lua(bit, filename)
            if lua.statements then
              self:error("Cannot use [[" .. tostring(bit.src) .. "]] as a string interpolation value, since it's not an expression.")
            end
            insert(concat_parts, lua.expr)
          end
        end
        return concat(concat_parts)
      end
      self:defmacro("do %block", function(self, vars)
        local make_line
        make_line = function(lua)
          return lua.expr and (lua.expr .. ";") or lua.statements
        end
        if vars.block.type == "Block" then
          return self:tree_to_lua(vars.block)
        else
          return {
            expr = tostring(self:tree_to_lua(vars.block)) .. "(nomsu, vars)"
          }
        end
      end)
      self:defmacro("immediately %block", function(self, vars)
        local lua = self:tree_to_lua(vars.block)
        local lua_code = lua.statements or (lua.expr .. ";")
        lua_code = "-- Immediately:\n" .. lua_code
        self:run_lua(lua_code, vars)
        return {
          statements = lua_code
        }
      end)
      self:defmacro("lua> %code", function(self, vars)
        local lua = nomsu_string_as_lua(self, vars.code)
        return {
          statements = lua
        }
      end)
      self:defmacro("=lua %code", function(self, vars)
        local lua = nomsu_string_as_lua(self, vars.code)
        return {
          expr = lua
        }
      end)
      self:defmacro("__src__ %level", function(self, vars)
        return {
          expr = repr(self:source_code(self:tree_to_value(vars.level)))
        }
      end)
      self:def("run file %filename", function(self, vars)
        return self:run_file(vars.filename, vars)
      end)
      return self:defmacro("require %filename", function(self, vars)
        local filename = self:tree_to_value(vars.filename)
        self:require_file(filename, vars)
        return {
          statements = "nomsu:require_file(" .. tostring(repr(filename)) .. ");"
        }
      end)
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, parent)
      self.write = function(self, ...)
        return io.write(...)
      end
      self.write_err = function(self, ...)
        return io.stderr:write(...)
      end
      self.defs = {
        ["#vars"] = { },
        ["#loaded_files"] = { }
      }
      if parent then
        setmetatable(self.defs, {
          __index = parent.defs
        })
        setmetatable(self.defs["#vars"], {
          __index = parent["#vars"]
        })
        setmetatable(self.defs["#loaded_files"], {
          __index = parent["#loaded_files"]
        })
      end
      self.callstack = { }
      self.compilestack = { }
      self.debug = false
      self.utils = utils
      self.repr = function(self, ...)
        return repr(...)
      end
      self.stringify = function(self, ...)
        return stringify(...)
      end
      if not parent then
        return self:initialize_core()
      end
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
    return Cs(((P("\\\\") / "\\") + (P("\\\"") / '"') + ESCAPE_CHAR + P(1)) ^ 0):match(str)
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
  self.stub_patt = re.compile("{|(' '+ / '\n..' / {'\\'? '%' %id*} / {%id+} / {%op})*|}", {
    id = IDENT_CHAR,
    op = OPERATOR_CHAR
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
  local c = NomsuCompiler()
  if args.flags["-v"] then
    c.debug = true
  end
  c.skip_precompiled = not args.flags["-O"]
  if args.input then
    if args.flags["-c"] and not args.output then
      args.output = args.input:gsub("%.nom", ".lua")
    end
    local compiled_output = nil
    if args.flags["-p"] then
      local _write = c.write
      c.write = function() end
      compiled_output = io.output()
    elseif args.output then
      compiled_output = io.open(args.output, 'w')
    end
    if args.input:match(".*%.lua") then
      local retval = dofile(args.input)(c, { })
    else
      local input
      if args.input == '-' then
        input = io.read('*a')
      else
        input = io.open(args.input):read("*a")
      end
      local vars = { }
      local retval, code = c:run(input, args.input, vars)
      if args.output then
        compiled_output:write(code)
      end
    end
    if args.flags["-p"] then
      c.write = _write
    end
  end
  if args.flags["-i"] then
    local vars = { }
    c:run('require "lib/core.nom"', "stdin")
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
        return c:run(buff, "stdin", vars)
      end)
      if ok and ret ~= nil then
        print("= " .. repr(ret))
      end
    end
  end
end
return NomsuCompiler
