local re = require('re')
local lpeg = require('lpeg')
local utils = require('utils2')
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
      return colors[color] .. msg .. colors.reset
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
local indent_stack = {
  0
}
local indent_patt = P(function(self, start)
  local spaces = self:match("[ \t]*", start)
  if #spaces > indent_stack[#indent_stack] then
    insert(indent_stack, #spaces)
    return start + #spaces
  end
end)
local dedent_patt = P(function(self, start)
  local spaces = self:match("[ \t]*", start)
  if #spaces < indent_stack[#indent_stack] then
    remove(indent_stack)
    return start
  end
end)
local nodent_patt = P(function(self, start)
  local spaces = self:match("[ \t]*", start)
  if #spaces == indent_stack[#indent_stack] then
    return start + #spaces
  end
end)
local gt_nodent_patt = P(function(self, start)
  local spaces = self:match("[ \t]*", start)
  if #spaces >= indent_stack[#indent_stack] + 4 then
    return start + indent_stack[#indent_stack] + 4
  end
end)
local nomsu = [=[    file <- ({{| shebang?
        (ignored_line %nl)*
        statements (nodent statements)*
        (%nl ignored_line)* %nl?
        (({.+} ("" -> "Parse error")) => error)? |} }) -> File

    shebang <- "#!" [^%nl]* %nl

    inline_statements <- inline_statement (semicolon inline_statement)*
    noeol_statements <- (inline_statement semicolon)* noeol_statement
    statements <- (inline_statement semicolon)* statement

    statement <- functioncall / expression
    noeol_statement <- noeol_functioncall / noeol_expression
    inline_statement <- inline_functioncall / inline_expression

    inline_thunk <- ({ {| "{" %ws? inline_statements %ws? "}" |} }) -> Thunk
    eol_thunk <- ({ {| ":" %ws? noeol_statements eol |} }) -> Thunk
    indented_thunk <- ({ {| (":" / "{..}") indent
                statements (nodent statements)*
            (dedent / (({.+} ("" -> "Error while parsing thunk")) => error))
        |} }) -> Thunk

    inline_nomsu <- ({("\" inline_expression) }) -> Nomsu
    eol_nomsu <- ({("\" noeol_expression) }) -> Nomsu
    indented_nomsu <- ({("\" expression) }) -> Nomsu

    inline_expression <- number / variable / inline_string / inline_list / inline_nomsu
        / inline_thunk / ("(" %ws? inline_statement %ws? ")")
    noeol_expression <- indented_string / indented_nomsu / indented_list / indented_thunk
        / ("(..)" indent
            statement
        (dedent / (({.+} ("" -> "Error while parsing indented expression"))))
        ) / inline_expression
    expression <- eol_thunk / eol_nomsu / noeol_expression

    -- Function calls need at least one word in them
    inline_functioncall <- ({(''=>line_no) {|
            (inline_expression %ws?)* word (%ws? (inline_expression / word))*
        |} }) -> FunctionCall
    noeol_functioncall <- ({(''=>line_no) {|
            (noeol_expression %ws?)* word (%ws? (noeol_expression / word))*
        |} }) -> FunctionCall
    functioncall <- ({(''=>line_no) {|
            (expression (dotdot / %ws?))* word ((dotdot / %ws?) (expression / word))*
        |} }) -> FunctionCall

    word <- ({ { %operator / (!number %plain_word) } }) -> Word
    
    inline_string <- ({ '"' {|
        ({~ (("\\" -> "\") / ('\"' -> '"') / ("\n" -> "
") / (!string_interpolation [^%nl"]))+ ~}
        / string_interpolation)* |} '"' }) -> String

    indented_string <- ({ '".."' %ws? line_comment? %nl %gt_nodented? {|
        ({~ (("\\" -> "\") / (%nl+ {~ %gt_nodented -> "" ~}) / [^%nl\]) ~} / string_interpolation)*
    |} ((!.) / (&(%nl+ !%gt_nodented)) / (({.+} ("" -> "Error while parsing String")) => error))
        }) -> String

    string_interpolation <- "\" ((noeol_expression dotdot?) / dotdot)

    number <- ({ (("-"? (([0-9]+ "." [0-9]+) / ("." [0-9]+) / ([0-9]+)))-> tonumber) }) -> Number

    -- Variables can be nameless (i.e. just %) and can't contain operators like apostrophe
    -- which is a hack to allow %'s to parse as "%" and "' s" separately
    variable <- ({ ("%" { %plain_word? }) }) -> Var

    inline_list <- ({ {|
         ("[" %ws? ((inline_list_item comma)* inline_list_item comma?)? %ws? "]")
      |} }) -> List
    indented_list <- ({ {|
         ("[..]" indent
                list_line (nodent list_line)*
          (dedent / (({.+} ("" -> "Error while parsing list")) => error)))
      |} }) -> List
    list_line <- (inline_list_item comma)* ((inline_list_item %ws? ",") / (functioncall / expression))
    inline_list_item <- inline_functioncall / inline_expression

    block_comment <- "#.." [^%nl]* (%nl (%ws? &%nl))* %nl %indented [^%nl]+ (%nl ((%ws? (!. / &%nl)) / (!%dedented [^%nl]+)))*
    line_comment  <- "#" [^%nl]*

    eol <- %ws? line_comment? (!. / &%nl)
    ignored_line <- (%nodented (block_comment / line_comment)) / (%ws? (!. / &%nl))
    indent <- eol (%nl ignored_line)* %nl %indented ((block_comment/line_comment) (%nl ignored_line)* nodent)?
    nodent <- eol (%nl ignored_line)* %nl %nodented
    dedent <- eol (%nl ignored_line)* (((!.) &%dedented) / (&(%nl %dedented)))
    comma <- %ws? "," %ws?
    semicolon <- %ws? ";" %ws?
    dotdot <- nodent ".." %ws?
]=]
local CURRENT_FILE = nil
local whitespace = S(" \t") ^ 1
local operator = S("'~`!@$^&*-+=|<>?/") ^ 1
local utf8_continuation = R("\128\191")
local utf8_char = (R("\194\223") * utf8_continuation + R("\224\239") * utf8_continuation * utf8_continuation + R("\240\244") * utf8_continuation * utf8_continuation * utf8_continuation)
local plain_word = (R('az', 'AZ', '09') + S("_") + utf8_char) ^ 1
local defs = {
  ws = whitespace,
  nl = P("\n"),
  tonumber = tonumber,
  operator = operator,
  plain_word = plain_word,
  indented = indent_patt,
  nodented = nodent_patt,
  dedented = dedent_patt,
  gt_nodented = gt_nodent_patt,
  line_no = function(src, pos)
    local line_no = 1
    for _ in src:sub(1, pos):gmatch("\n") do
      line_no = line_no + 1
    end
    return pos, tostring(CURRENT_FILE) .. ":" .. tostring(line_no)
  end,
  FunctionCall = function(src, line_no, value, errors)
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
    return {
      type = "FunctionCall",
      src = src,
      line_no = line_no,
      value = value,
      errors = errors,
      stub = stub
    }
  end,
  error = function(src, pos, errors, err_msg)
    local line_no = 1
    for _ in src:sub(1, -#errors):gmatch("\n") do
      line_no = line_no + 1
    end
    local err_pos = #src - #errors + 1
    if errors:sub(1, 1) == "\n" then
      err_pos = err_pos + #errors:match("[ \t]*", 2)
    end
    local start_of_err_line = err_pos
    while src:sub(start_of_err_line, start_of_err_line) ~= "\n" and start_of_err_line > 1 do
      start_of_err_line = start_of_err_line - 1
    end
    local start_of_prev_line = start_of_err_line - 1
    while src:sub(start_of_prev_line, start_of_prev_line) ~= "\n" and start_of_prev_line > 1 do
      start_of_prev_line = start_of_prev_line - 1
    end
    local prev_line, err_line, next_line
    prev_line, err_line, next_line = src:match("([^\n]*)\n([^\n]*)\n([^\n]*)", start_of_prev_line + 1)
    local pointer = ("-"):rep(err_pos - start_of_err_line + 0) .. "^"
    return error("\n" .. tostring(err_msg or "Parse error") .. " in " .. tostring(CURRENT_FILE) .. " on line " .. tostring(line_no) .. ":\n\n" .. tostring(prev_line) .. "\n" .. tostring(err_line) .. "\n" .. tostring(pointer) .. "\n" .. tostring(next_line) .. "\n")
  end
}
setmetatable(defs, {
  __index = function(t, key)
    do
      local _with_0
      _with_0 = function(src, value, errors)
        return {
          type = key,
          src = src,
          value = value,
          errors = errors
        }
      end
      t[key] = _with_0
      local _ = nil
      return _with_0
    end
  end
})
nomsu = re.compile(nomsu, defs)
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
    def = function(self, signature, thunk, src, is_macro)
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
      if self.debug then
        self:write(colored.magenta("Defined rule " .. tostring(repr(signature))))
      end
      assert(type(thunk) == 'function', "Bad thunk: " .. tostring(repr(thunk)))
      local canonical_args = nil
      local canonical_escaped_args = nil
      local aliases = { }
      self.__class.def_number = self.__class.def_number + 1
      local def = {
        thunk = thunk,
        src = src,
        is_macro = is_macro,
        aliases = { },
        def_number = self.__class.def_number,
        defs = self.defs
      }
      local where_defs_go = (getmetatable(self.defs) or { }).__newindex or self.defs
      for _index_0 = 1, #signature do
        local _des_0 = signature[_index_0]
        local stub, arg_names, escaped_args
        stub, arg_names, escaped_args = _des_0[1], _des_0[2], _des_0[3]
        assert(stub, "NO STUB FOUND: " .. tostring(repr(signature)))
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
          assert(equivalent(set(arg_names), canonical_args), "Mismatched args")
        else
          canonical_args = set(arg_names)
        end
        if canonical_escaped_args then
          assert(equivalent(escaped_args, canonical_escaped_args), "Mismatched escaped args")
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
    defmacro = function(self, signature, thunk, src)
      return self:def(signature, thunk, src, true)
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
      local thunk, arg_names
      thunk, arg_names = def.thunk, def.arg_names
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
        self:writeln(tostring(colored.bright("WITH ARGS:")) .. " " .. tostring(colored.dim(repr(args))))
      end
      local old_defs
      old_defs, self.defs = self.defs, def.defs
      local rets = {
        thunk(self, args)
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
      local expr, statement = self:call(tree.stub, tree.line_no, unpack(args))
      remove(self.callstack)
      return expr, statement
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
    indent = function(self, code)
      return (code:gsub("\n", "\n    "))
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
        self:writeln(tostring(colored.bright("PARSING:")) .. "\n" .. tostring(str))
      end
      str = str:gsub("\r", "")
      local old_file = CURRENT_FILE
      local old_indent_stack
      old_indent_stack, indent_stack = indent_stack, {
        0
      }
      CURRENT_FILE = filename
      local tree = nomsu:match(str)
      indent_stack = old_indent_stack
      CURRENT_FILE = old_file
      assert(tree, "Failed to parse: " .. tostring(str))
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
      assert(tree, "Tree failed to compile: " .. tostring(src))
      assert(tree.type == "File", "Attempt to run non-file: " .. tostring(tree.type))
      local buffer = { }
      local return_value = nil
      local _list_0 = tree.value
      for _index_0 = 1, #_list_0 do
        local statement = _list_0[_index_0]
        if self.debug then
          self:writeln(tostring(colored.bright("RUNNING NOMSU:")) .. "\n" .. tostring(colored.bright(colored.yellow(statement.src))))
          self:writeln(colored.bright("PARSED TO TREE:"))
          self:print_tree(statement)
        end
        local ok, expr, statements = pcall(self.tree_to_lua, self, statement, filename)
        if not ok then
          self:errorln(tostring(colored.red("Error occurred in statement:")) .. "\n" .. tostring(colored.bright(colored.yellow(statement.src))))
          error(expr)
        end
        local code_for_statement = ([[return (function(nomsu, vars)
%s
return %s;
end);]]):format(statements or "", expr or "ret")
        if output_file then
          if statements and #statements > 0 then
            output_file:write("lua> \"..\"\n    " .. tostring(self:indent(statements:gsub("\\", "\\\\"))) .. "\n")
          end
          if expr and #expr > 0 then
            output_file:write("=lua \"..\"\n    " .. tostring(self:indent(expr:gsub("\\", "\\\\"))) .. "\n")
          end
        end
        if self.debug then
          self:writeln(tostring(colored.bright("RUNNING LUA:")) .. "\n" .. tostring(colored.blue(colored.bright(code_for_statement))))
        end
        local lua_thunk, err = load(code_for_statement)
        if not lua_thunk then
          local n = 1
          local fn
          fn = function()
            n = n + 1
            return ("\n%-3d|"):format(n)
          end
          local code = "1  |" .. code_for_statement:gsub("\n", fn)
          error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(colored.onblack(code)))) .. "\n\n" .. tostring(err) .. "\n\nProduced by statement:\n" .. tostring(colored.bright(colored.yellow(statement.src))))
        end
        local run_statement = lua_thunk()
        local ret
        ok, ret = pcall(run_statement, self, vars)
        if expr then
          return_value = ret
        end
        if not ok then
          self:errorln(tostring(colored.red("Error occurred in statement:")) .. "\n" .. tostring(colored.yellow(statement.src)))
          self:errorln(debug.traceback())
          error(ret)
        end
        if statements then
          insert(buffer, statements)
        end
        if expr then
          insert(buffer, "ret = " .. tostring(expr) .. ";")
        end
      end
      if max_operations then
        debug.sethook()
      end
      local lua_code = ([[return (function(nomsu, vars)
local ret;
%s
return ret;
end);]]):format(concat(buffer, "\n"))
      return return_value, lua_code, vars
    end,
    tree_to_value = function(self, tree, vars, filename)
      local code = "return (function(nomsu, vars)\nreturn " .. tostring(self:tree_to_lua(tree, filename)) .. ";\nend);"
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
      assert(tree, "No tree provided.")
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
      elseif "Thunk" == _exp_0 then
        if force_inline then
          return "{" .. tostring(concat((function()
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = tree.value
            for _index_0 = 1, #_list_0 do
              local v = _list_0[_index_0]
              _accum_0[_len_0] = self:tree_to_nomsu(v, true)
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)(), "; ")), true
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
          local arg_inline
          nomsu, arg_inline = self:tree_to_nomsu(arg, force_inline)
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
          local bit_inline
          nomsu, bit_inline = self:tree_to_nomsu(bit, force_inline)
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
          return "(d{" .. tostring(concat((function()
            local _accum_0 = { }
            local _len_0 = 1
            for k, v in pairs(value) do
              _accum_0[_len_0] = tostring(self:value_to_nomsu(k)) .. "=" .. tostring(self:value_to_nomsu(v))
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)(), "; ")) .. "})"
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
      assert(tree, "No tree provided.")
      if not tree.type then
        self:errorln(debug.traceback())
        self:error("Invalid tree: " .. tostring(repr(tree)))
      end
      local _exp_0 = tree.type
      if "File" == _exp_0 then
        local lua_bits = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local line = _list_0[_index_0]
          local expr, statement = self:tree_to_lua(line, filename)
          if statement then
            insert(lua_bits, statement)
          end
          if expr then
            insert(lua_bits, "ret = " .. tostring(expr) .. ";")
          end
        end
        return nil, concat(lua_bits, "\n")
      elseif "Nomsu" == _exp_0 then
        return "nomsu:parse(" .. tostring(repr(tree.value.src)) .. ", " .. tostring(repr(tree.line_no)) .. ").value[1]", nil
      elseif "Thunk" == _exp_0 then
        local lua_bits = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local arg = _list_0[_index_0]
          local expr, statement = self:tree_to_lua(arg, filename)
          if statement then
            insert(lua_bits, statement)
          end
          if expr then
            insert(lua_bits, "ret = " .. tostring(expr) .. ";")
          end
        end
        return ([[(function(nomsu, vars)
local ret;
%s
return ret;
end)]]):format(concat(lua_bits, "\n"))
      elseif "FunctionCall" == _exp_0 then
        insert(self.compilestack, tree)
        local def = self.defs[tree.stub]
        if def and def.is_macro then
          local expr, statement = self:run_macro(tree)
          if def.whiteset then
            if expr then
              expr = "(nomsu:assert_permission(" .. tostring(repr(tree.stub)) .. ") and " .. tostring(expr) .. ")"
            end
            if statement then
              statement = "nomsu:assert_permission(" .. tostring(repr(tree.stub)) .. ");\n" .. statement
            end
          end
          remove(self.compilestack)
          return expr, statement
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
              arg = {
                type = "Nomsu",
                value = arg,
                line_no = tree.line_no
              }
            end
            local expr, statement = self:tree_to_lua(arg, filename)
            if statement then
              self:error("Cannot use [[" .. tostring(arg.src) .. "]] as a function argument, since it's not an expression.")
            end
            insert(args, expr)
            arg_num = arg_num + 1
            _continue_0 = true
          until true
          if not _continue_0 then
            break
          end
        end
        remove(self.compilestack)
        return self.__class:comma_separated_items("nomsu:call(", args, ")"), nil
      elseif "String" == _exp_0 then
        if self.debug then
          self:writeln((colored.bright("STRING:")))
          self:print_tree(tree)
        end
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
            local expr, statement = self:tree_to_lua(bit, filename)
            if self.debug then
              self:writeln((colored.bright("INTERP:")))
              self:print_tree(bit)
              self:writeln(tostring(colored.bright("EXPR:")) .. " " .. tostring(expr) .. ", " .. tostring(colored.bright("STATEMENT:")) .. " " .. tostring(statement))
            end
            if statement then
              self:error("Cannot use [[" .. tostring(bit.src) .. "]] as a string interpolation value, since it's not an expression.")
            end
            insert(concat_parts, "nomsu:stringify(" .. tostring(expr) .. ")")
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
          return "''", nil
        elseif #concat_parts == 1 then
          return concat_parts[1], nil
        else
          return "(" .. tostring(concat(concat_parts, "..")) .. ")", nil
        end
      elseif "List" == _exp_0 then
        local items = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local item = _list_0[_index_0]
          local expr, statement = self:tree_to_lua(item, filename)
          if statement then
            self:error("Cannot use [[" .. tostring(item.src) .. "]] as a list item, since it's not an expression.")
          end
          insert(items, expr)
        end
        return self.__class:comma_separated_items("{", items, "}"), nil
      elseif "Number" == _exp_0 then
        return repr(tree.value), nil
      elseif "Var" == _exp_0 then
        if tree.value:match("^[a-zA-Z_][a-zA-Z0-9_]*$") then
          return "vars." .. tostring(tree.value), nil
        else
          return "vars[" .. tostring(repr(tree.value)) .. "]", nil
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
      if "List" == _exp_0 or "File" == _exp_0 or "Thunk" == _exp_0 or "FunctionCall" == _exp_0 or "String" == _exp_0 then
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local v = _list_0[_index_0]
          self:walk_tree(v, depth + 1)
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
      elseif "File" == _exp_0 or "Nomsu" == _exp_0 or "Thunk" == _exp_0 or "List" == _exp_0 or "FunctionCall" == _exp_0 or "String" == _exp_0 then
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
        x = x:gsub("\n%s*%.%.", " ")
        x = lpeg.Cs((operator / (function(op)
          return " " .. tostring(op) .. " "
        end) + 1) ^ 0):match(x)
        x = x:gsub("%s+", " "):gsub("^%s*", ""):gsub("%s*$", "")
        local stub = x:gsub("%%%S+", "%%"):gsub("\\", "")
        local arg_names
        do
          local _accum_0 = { }
          local _len_0 = 1
          for arg in x:gmatch("%%([^%s]*)") do
            _accum_0[_len_0] = arg
            _len_0 = _len_0 + 1
          end
          arg_names = _accum_0
        end
        local escaped_args = set((function()
          local _accum_0 = { }
          local _len_0 = 1
          for arg in x:gmatch("\\%%([^%s]*)") do
            _accum_0[_len_0] = arg
            _len_0 = _len_0 + 1
          end
          return _accum_0
        end)())
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
      if type(x) == desired_type then
        return x
      end
      if type(x) == 'table' and x.type == desired_type then
        return x
      end
      return self:error("Invalid type for %" .. tostring(varname) .. ". Expected " .. tostring(desired_type) .. ", but got " .. tostring(repr(x)) .. ".")
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
            local expr, statement = self:tree_to_lua(bit, filename)
            if statement then
              self:error("Cannot use [[" .. tostring(bit.src) .. "]] as a string interpolation value, since it's not an expression.")
            end
            insert(concat_parts, expr)
          end
        end
        return concat(concat_parts)
      end
      local lua_code
      lua_code = function(self, vars)
        local lua = nomsu_string_as_lua(self, vars.code)
        return nil, lua
      end
      self:defmacro("lua> %code", lua_code)
      local lua_value
      lua_value = function(self, vars)
        local lua = nomsu_string_as_lua(self, vars.code)
        return lua, nil
      end
      self:defmacro("=lua %code", lua_value)
      self:defmacro("__src__ %level", function(self, vars)
        return self:repr(self:source_code(self:tree_to_value(vars.level)))
      end)
      local run_file
      run_file = function(self, vars)
        if vars.filename:match(".*%.lua") then
          return dofile(vars.filename)(self, vars)
        end
        if vars.filename:match(".*%.nom") then
          if not self.skip_precompiled then
            local file = io.open(vars.filename:gsub("%.nom", ".compiled.nom"), "r")
          end
          local file = file or io.open(vars.filename)
          if not file then
            self:error("File does not exist: " .. tostring(vars.filename))
          end
          local contents = file:read('*a')
          file:close()
          return self:run(contents, vars.filename)
        else
          return self:error("Invalid filetype for " .. tostring(vars.filename))
        end
      end
      self:def("run file %filename", run_file)
      local _require
      _require = function(self, vars)
        local loaded = self.defs["#loaded_files"]
        if not loaded[vars.filename] then
          loaded[vars.filename] = run_file(self, {
            filename = vars.filename
          }) or true
        end
        return loaded[vars.filename]
      end
      return self:def("require %filename", _require)
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
  self.unescape_string = function(self, str)
    return str:gsub("\\(.)", (function(c)
      return STRING_ESCAPES[c] or c
    end))
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
  NomsuCompiler = _class_0
end
if arg then
  colors = require('consolecolors')
  local parser = re.compile([[        args <- {| {:flags: flags? :} ({:input: input :} ";" ("-o;"{:output: output :} ";")?)? (";")? |} !.
        flags <- (({| ({flag} ";")* |}) -> set)
        flag <- "-c" / "-i" / "-p" / "-O" / "--help" / "-h"
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
  c.skip_precompiled = not args.flags["-O"]
  if args.input then
    if args.flags["-c"] and not args.output then
      args.output = args.input:gsub("%.nom", ".compiled.nom")
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
      local retval, code = c:run(input, args.input, vars, nil, compiled_output)
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
