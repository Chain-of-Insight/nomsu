local re = require('re')
local lpeg = require('lpeg')
local utils = require('utils')
local repr = utils.repr
local colors = require('consolecolors')
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
lpeg.setmaxstack(10000)
local P, V, S, Cg, C, Cp, B, Cmt
P, V, S, Cg, C, Cp, B, Cmt = lpeg.P, lpeg.V, lpeg.S, lpeg.Cg, lpeg.C, lpeg.Cp, lpeg.B, lpeg.Cmt
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
local check_indent
check_indent = function(subject, end_pos, spaces)
  if #spaces > indent_stack[#indent_stack] then
    insert(indent_stack, #spaces)
    return end_pos
  end
end
local check_dedent
check_dedent = function(subject, end_pos, spaces)
  if #spaces < indent_stack[#indent_stack] then
    remove(indent_stack)
    return end_pos
  end
end
local check_nodent
check_nodent = function(subject, end_pos, spaces)
  if #spaces == indent_stack[#indent_stack] then
    return end_pos
  end
end
local nomsu = [=[    file <- ({ {| shebang?
        (ignored_line %nl)*
        statements (nodent statements)*
        (%nl ignored_line)* %nl?
        (({.+} ("" -> "Unexpected end of file")) => error)? |} }) -> File

    shebang <- "#!" [^%nl]* %nl

    inline_statements <- inline_statement (semicolon inline_statement)*
    noeol_statements <- (inline_statement semicolon)* noeol_statement
    statements <- (inline_statement semicolon)* statement

    statement <- functioncall / expression
    noeol_statement <- noeol_functioncall / noeol_expression
    inline_statement <- inline_functioncall / inline_expression

    inline_thunk <- ({ {| "{" inline_statements "}" |} }) -> Thunk
    eol_thunk <- ({ {| ":" %ws? noeol_statements eol |} }) -> Thunk
    indented_thunk <- ({ {| (":" / "{..}") indent
                statements (nodent statements)*
            (dedent / (({.+} ("" -> "Error while parsing thunk")) => error))
        |} }) -> Thunk

    inline_nomsu <- ({ ("\" inline_expression) }) -> Nomsu
    eol_nomsu <- ({ ("\" noeol_expression) }) -> Nomsu
    indented_nomsu <- ({ ("\" expression) }) -> Nomsu

    inline_expression <- number / variable / inline_string / inline_list / inline_nomsu
        / inline_thunk / ("(" inline_statement ")")
    noeol_expression <- indented_string / indented_nomsu / indented_list / indented_thunk
        / ("(..)" indent
            statement
        (dedent / (({.+} ("" -> "Error while parsing indented expression"))))
        ) / inline_expression
    expression <- eol_thunk / eol_nomsu / noeol_expression

    -- Function calls need at least one word in them
    inline_functioncall <- ({ {|
            (inline_expression tok_gap)* word (tok_gap (inline_expression / word))*
        |} }) -> FunctionCall
    noeol_functioncall <- ({ {|
            (noeol_expression tok_gap)* word (tok_gap (noeol_expression / word))*
        |} }) -> FunctionCall
    functioncall <- ({ {|
            (expression (dotdot / tok_gap))* word ((dotdot / tok_gap) (expression / word))*
        |} }) -> FunctionCall

    word <- ({ !number {%wordchar (!"'" %wordchar)*} }) -> Word
    
    inline_string <- ({ '"' {|
        ({~ (("\\" -> "\") / ('\"' -> '"') / ("\n" -> "
") / (!string_interpolation [^%nl"]))+ ~}
        / string_interpolation)* |} '"' }) -> String
    indented_string <- ({ '".."' indent {|
            indented_string_line (nodent {~ "" -> "
" ~} indented_string_line)*
          |} (dedent / (({.+} ("" -> "Error while parsing String")) => error))
        }) -> String
    indented_string_line <- "|" ({~ (("\\" -> "\") / (!string_interpolation [^%nl]))+ ~} / string_interpolation)*
    string_interpolation <- "\" ((noeol_expression dotdot?) / dotdot)

    number <- ({ (("-"? (([0-9]+ "." [0-9]+) / ("." [0-9]+) / ([0-9]+)))-> tonumber) }) -> Number

    -- Variables can be nameless (i.e. just %) and can't contain apostrophes
    -- which is a hack to allow %foo's to parse as "%foo" and "'s" separately
    variable <- ({ ("%" { (!"'" %wordchar)* }) }) -> Var

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
    tok_gap <- %ws / %prev_edge / &("[" / "\" / [.,:;{("#%'])
    comma <- %ws? "," %ws?
    semicolon <- %ws? ";" %ws?
    dotdot <- nodent ".." %ws?
]=]
local whitespace = S(" \t") ^ 1
local defs = {
  ws = whitespace,
  nl = P("\n"),
  tonumber = tonumber,
  wordchar = P(1) - S(' \t\n\r%#:;,.{}[]()"\\'),
  indented = Cmt(S(" \t") ^ 0 * (#(P(1) - S(" \t\n") + (-P(1)))), check_indent),
  nodented = Cmt(S(" \t") ^ 0 * (#(P(1) - S(" \t\n") + (-P(1)))), check_nodent),
  dedented = Cmt(S(" \t") ^ 0 * (#(P(1) - S(" \t\n") + (-P(1)))), check_dedent),
  prev_edge = B(S(" \t\n.,:;}])\"\\")),
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
    return error("\n" .. tostring(err_msg or "Parse error") .. " in " .. tostring(filename) .. " on line " .. tostring(line_no) .. ":\n\n" .. tostring(prev_line) .. "\n" .. tostring(err_line) .. "\n" .. tostring(pointer) .. "\n" .. tostring(next_line) .. "\n")
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
    def = function(self, signature, thunk, src, is_macro)
      if is_macro == nil then
        is_macro = false
      end
      assert(type(thunk) == 'function', "Bad thunk: " .. tostring(repr(thunk)))
      local canonical_args = nil
      local aliases = { }
      local _list_0 = self:get_stubs(signature)
      for _index_0 = 1, #_list_0 do
        local _des_0 = _list_0[_index_0]
        local stub, arg_names
        stub, arg_names = _des_0[1], _des_0[2]
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
          assert(utils.equivalent(utils.set(arg_names), canonical_args), "Mismatched args")
        else
          canonical_args = utils.set(arg_names)
        end
        insert(aliases, stub)
        self.defs[stub] = {
          thunk = thunk,
          stub = stub,
          arg_names = arg_names,
          src = src,
          is_macro = is_macro,
          aliases = aliases
        }
      end
    end,
    defmacro = function(self, signature, thunk, src)
      return self:def(signature, thunk, src, true)
    end,
    call = function(self, stub, ...)
      local def = self.defs[stub]
      if def == nil then
        self:error("Attempt to call undefined function: " .. tostring(stub))
      end
      if def.is_macro and self.callstack[#self.callstack] ~= "#macro" then
        self:error("Attempt to call macro at runtime: " .. tostring(stub) .. "\nThis can be caused by using a macro in a function that is defined before the macro.")
      end
      if not (self:check_permission(def)) then
        self:error("You do not have the authority to call: " .. tostring(stub))
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
      insert(self.callstack, stub)
      local rets = {
        thunk(self, args)
      }
      remove(self.callstack)
      return unpack(rets)
    end,
    run_macro = function(self, tree, kind)
      if kind == nil then
        kind = "Expression"
      end
      local stub, arg_names, args = self:get_stub(tree)
      if self.debug then
        self:write(tostring(colored.bright("RUNNING MACRO")) .. " " .. tostring(colored.underscore(colored.magenta(stub))) .. " ")
        self:writeln(tostring(colored.bright("WITH ARGS:")) .. " " .. tostring(colored.dim(repr(args))))
      end
      insert(self.callstack, "#macro")
      local expr, statement = self:call(stub, unpack(args))
      remove(self.callstack)
      return expr, statement
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
        if whiteset[caller] then
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
      local old_indent_stack
      old_indent_stack, indent_stack = indent_stack, {
        0
      }
      local tree = nomsu:match(str)
      indent_stack = old_indent_stack
      assert(tree, "Failed to parse: " .. tostring(str))
      if self.debug then
        self:writeln("PARSE TREE:")
        self:print_tree(tree, "    ")
      end
      return tree
    end,
    run = function(self, src, filename)
      local tree = self:parse(src, filename)
      assert(tree, "Tree failed to compile: " .. tostring(src))
      assert(tree.type == "File", "Attempt to run non-file: " .. tostring(tree.type))
      local buffer = { }
      local vars = { }
      local return_value = nil
      local _list_0 = tree.value
      for _index_0 = 1, #_list_0 do
        local statement = _list_0[_index_0]
        if self.debug then
          self:writeln(tostring(colored.bright("RUNNING NOMSU:")) .. "\n" .. tostring(colored.bright(colored.yellow(statement.src))))
          self:writeln(colored.bright("PARSED TO TREE:"))
          self:print_tree(statement)
        end
        local ok, expr, statements = pcall(self.tree_to_lua, self, statement)
        if not ok then
          self:writeln(tostring(colored.red("Error occurred in statement:")) .. "\n" .. tostring(colored.bright(colored.yellow(statement.src))))
          self:error(expr)
        end
        local code_for_statement = ([[                return (function(nomsu, vars)
                    %s
                    return %s;
                end);]]):format(statements or "", expr or "ret")
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
          error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(code))) .. "\n\n" .. tostring(err) .. "\n\nProduced by statement:\n" .. tostring(colored.bright(colored.yellow(statement.src))))
        end
        local run_statement = lua_thunk()
        local ret
        ok, ret = pcall(run_statement, self, vars)
        if expr then
          return_value = ret
        end
        if not ok then
          self:writeln(tostring(colored.red("Error occurred in statement:")) .. "\n" .. tostring(colored.yellow(statement.src)))
          self:writeln(debug.traceback())
          self:error(ret)
        end
        insert(buffer, tostring(statements or '') .. "\n" .. tostring(expr and "ret = " .. tostring(expr) or ''))
      end
      local lua_code = ([[            return (function(nomsu, vars)
                local ret;
                %s
                return ret;
            end);]]):format(concat(buffer, "\n"))
      return return_value, lua_code
    end,
    tree_to_value = function(self, tree, vars)
      local code = "return (function(nomsu, vars)\nreturn " .. tostring(self:tree_to_lua(tree)) .. ";\nend);"
      if self.debug then
        self:writeln(tostring(colored.bright("RUNNING LUA TO GET VALUE:")) .. "\n" .. tostring(colored.blue(colored.bright(code))))
      end
      local lua_thunk, err = load(code)
      if not lua_thunk then
        self:error("Failed to compile generated code:\n" .. tostring(colored.bright(colored.blue(code))) .. "\n\n" .. tostring(colored.red(err)))
      end
      return (lua_thunk())(self, vars or { })
    end,
    tree_to_lua = function(self, tree)
      assert(tree, "No tree provided.")
      if not tree.type then
        self:writeln(debug.traceback())
        self:error("Invalid tree: " .. tostring(repr(tree)))
      end
      local _exp_0 = tree.type
      if "File" == _exp_0 then
        return error("Should not be converting File to lua through this function.")
      elseif "Nomsu" == _exp_0 then
        return repr(tree.value), nil
      elseif "Thunk" == _exp_0 then
        local lua_bits = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local arg = _list_0[_index_0]
          local expr, statement = self:tree_to_lua(arg)
          if statement then
            insert(lua_bits, statement)
          end
          if expr then
            insert(lua_bits, "ret = " .. tostring(expr) .. ";")
          end
        end
        return ([[                    (function(nomsu, vars)
                        local ret;
                        %s
                        return ret;
                    end)]]):format(concat(lua_bits, "\n"))
      elseif "FunctionCall" == _exp_0 then
        local stub = self:get_stub(tree)
        if self.defs[stub] and self.defs[stub].is_macro then
          return self:run_macro(tree, "Expression")
        end
        local args = {
          repr(stub)
        }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local _continue_0 = false
          repeat
            local arg = _list_0[_index_0]
            if arg.type == 'Word' then
              _continue_0 = true
              break
            end
            local expr, statement = self:tree_to_lua(arg)
            if statement then
              self:error("Cannot use [[" .. tostring(arg.src) .. "]] as a function argument, since it's not an expression.")
            end
            insert(args, expr)
            _continue_0 = true
          until true
          if not _continue_0 then
            break
          end
        end
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
            local expr, statement = self:tree_to_lua(bit)
            if self.debug then
              self:writeln((colored.bright("INTERP:")))
              self:print_tree(bit)
              self:writeln(tostring(colored.bright("EXPR:")) .. " " .. tostring(expr) .. ", " .. tostring(colored.bright("STATEMENT:")) .. " " .. tostring(statement))
            end
            if statement then
              self:error("Cannot use [[" .. tostring(bit.src) .. "]] as a string interpolation value, since it's not an expression.")
            end
            insert(concat_parts, "nomsu.utils.repr_if_not_string(" .. tostring(expr) .. ")")
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
          local expr, statement = self:tree_to_lua(item)
          if statement then
            self:error("Cannot use [[" .. tostring(item.src) .. "]] as a list item, since it's not an expression.")
          end
          insert(items, expr)
        end
        return self.__class:comma_separated_items("{", items, "}"), nil
      elseif "Number" == _exp_0 then
        return repr(tree.value), nil
      elseif "Var" == _exp_0 then
        return "vars[" .. tostring(repr(tree.value)) .. "]", nil
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
        local stub = x:gsub("'", " '"):gsub("%%%S+", "%%"):gsub("%s+", " ")
        local arg_names
        do
          local _accum_0 = { }
          local _len_0 = 1
          for arg in x:gmatch("%%([^%s']*)") do
            _accum_0[_len_0] = arg
            _len_0 = _len_0 + 1
          end
          arg_names = _accum_0
        end
        return stub, arg_names
      end
      local _exp_0 = x.type
      if "String" == _exp_0 then
        return self:get_stub(x.value)
      elseif "FunctionCall" == _exp_0 then
        local stub, arg_names, args = { }, { }, { }
        local _list_0 = x.value
        for _index_0 = 1, #_list_0 do
          local token = _list_0[_index_0]
          local _exp_1 = token.type
          if "Word" == _exp_1 then
            insert(stub, token.value)
          elseif "Var" == _exp_1 then
            insert(stub, "%")
            if arg_names then
              insert(arg_names, token.value)
            end
            insert(args, token)
          else
            insert(stub, "%")
            arg_names = nil
            insert(args, token)
          end
        end
        return concat(stub, " "), arg_names, args
      else
        return self:error("Unsupported get stub type: " .. tostring(x.type))
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
    error = function(self, ...)
      self:writeln("ERROR!")
      if select(1, ...) then
        self:writeln(...)
      end
      self:writeln("Callstack:")
      for i = #self.callstack, 1, -1 do
        self:writeln("    " .. tostring(self.callstack[i]))
      end
      self:writeln("    <top level>")
      self.callstack = { }
      return error()
    end,
    typecheck = function(self, vars, varname, desired_type)
      local x = vars[varname]
      if type(x) == desired_type then
        return x
      end
      if type(x) == 'table' and x.type == desired_type then
        return x
      end
      return self:error("Invalid type for %" .. tostring(varname) .. ". Expected " .. tostring(desired_type) .. ", but got " .. tostring(x.type) .. ".")
    end,
    initialize_core = function(self)
      local lua_code
      lua_code = function(self, vars)
        local inner_vars = setmetatable({ }, {
          __index = function(_, key)
            return "vars[" .. tostring(repr(key)) .. "]"
          end
        })
        local lua = self:tree_to_value(vars.code, inner_vars)
        return nil, lua
      end
      self:defmacro("lua code %code", lua_code)
      local lua_value
      lua_value = function(self, vars)
        local inner_vars = setmetatable({ }, {
          __index = function(_, key)
            return "vars[" .. tostring(repr(key)) .. "]"
          end
        })
        local lua = self:tree_to_value(vars.code, inner_vars)
        return lua, nil
      end
      self:defmacro("lua expr %code", lua_value)
      local run_file
      run_file = function(self, vars)
        if vars.filename:match(".*%.lua") then
          return dofile(vars.filename)(self, vars)
        end
        if vars.filename:match(".*%.nom") then
          if not self.skip_precompiled then
            local file = io.open(vars.filename .. ".lua", "r")
            if file then
              local contents = file:read('*a')
              file:close()
              return load(contents)()(self, vars)
            end
          end
          local file = io.open(vars.filename)
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
        if not self.loaded_files[vars.filename] then
          self.loaded_files[vars.filename] = run_file(self, {
            filename = vars.filename
          }) or true
        end
        return self.loaded_files[vars.filename]
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
      self.defs = setmetatable({ }, {
        __index = parent and parent.defs
      })
      self.callstack = { }
      self.debug = false
      self.utils = utils
      self.repr = function(self, ...)
        return repr(...)
      end
      self.loaded_files = { }
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
  local parser = re.compile([[        args <- {| {:flags: flags? :} ({:input: input :} ";" ("-o;"{:output: output :} ";")?)? |} !.
        flags <- (({| ({flag} ";")* |}) -> set)
        flag <- "-c" / "-i" / "-p" / "-f" / "--help" / "-h"
        input <- "-" / [^;]+
        output <- "-" / [^;]+
    ]], {
    set = utils.set
  })
  local args = concat(arg, ";") .. ";"
  args = parser:match(args) or { }
  if not args or not args.flags or args.flags["--help"] or args.flags["-h"] then
    print("Usage: lua nomsu.lua [-c] [-i] [-p] [-f] [--help] [input [-o output]]")
    os.exit()
  end
  local c = NomsuCompiler()
  c.skip_precompiled = args.flags["-f"]
  if args.input then
    if args.flags["-c"] and not args.output then
      args.output = args.input .. ".lua"
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
      local retval, code = c:run(input, args.input)
      if compiled_output then
        compiled_output:write(code)
      end
    end
    if args.flags["-p"] then
      c.write = _write
    end
  end
  if not args.input or args.flags["-i"] then
    c:run('require "lib/core.nom"')
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
        return c:run(buff)
      end)
      if ok and ret ~= nil then
        print("= " .. repr(ret))
      end
    end
  end
end
return NomsuCompiler
