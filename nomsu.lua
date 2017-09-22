local re = require('re')
local lpeg = require('lpeg')
local utils = require('utils')
local repr = utils.repr
local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local pcall
pcall = function(fn, ...)
  return true, fn(...)
end
local INDENT = "    "
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
local parsetree_mt = {
  __tostring = function(self)
    return tostring(self.type) .. "(" .. tostring(repr(self.value)) .. ")"
  end
}
local ParseTree
ParseTree = function(x)
  return setmetatable(x, parsetree_mt)
end
local functiondef_mt = {
  __tostring = function(self)
    return "FunctionDef(" .. tostring(repr(self.aliases))
  end
}
local FunctionDef
FunctionDef = function(fn, aliases, src, is_macro)
  return setmetatable({
    fn = fn,
    aliases = aliases,
    src = src,
    is_macro = is_macro
  }, functiondef_mt)
end
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
local nomsu = [=[    file <- ({ {| shebang? {:body: block :} %nl* (({.+} ("" -> "Unexpected end of file")) => error)? |} }) -> File

    shebang <- "#!" [^%nl]* %nl

    block <- ({ {|
        (ignored_line %nl)*
        line_of_statements (nodent line_of_statements)*
        (%nl ignored_line)* |} }) -> Block
    inline_block <- ({ {| inline_line_of_statements |} }) -> Block

    line_of_statements <- statement (%ws? ";" %ws? statement)*
    inline_line_of_statements <- inline_statement (%ws? ";" %ws? inline_statement)*

    statement <- ({ functioncall / expression }) -> Statement
    inline_statement <- ({ inline_functioncall / expression }) -> Statement

    expression <- (
        longstring / string / number / variable / list / thunk / block_functioncall
        / ("(" %ws? (inline_thunk / inline_functioncall) %ws? ")"))

    -- Function calls need at least one word in them
    functioncall <- ({ {|
            (expression (dotdot / tok_gap))* word ((dotdot / tok_gap) (expression / word))*
        |} }) -> FunctionCall
    inline_functioncall <- ({ {|
            (expression tok_gap)* word (tok_gap (expression / word))*
        |} }) -> FunctionCall
    block_functioncall <- "(..)" indent
            functioncall
        (dedent / (({.+} ("" -> "Error while parsing block function call")) => error))

    word <- ({ !number {%wordchar (!"'" %wordchar)*} }) -> Word
    
    thunk <- ({ ":" ((indent block (dedent / (({.+} ("" -> "Error while parsing thunk")) => error)))
        / (%ws? inline_block)) }) -> Thunk
    inline_thunk <- ({ ":" %ws? inline_block }) -> Thunk

    string <- ({ (!longstring) '"' {(("\" [^%nl]) / [^"%nl])*} '"' }) -> String

    longstring <- ({ '".."' %ws?
        {| (longstring_line (indent
                longstring_line (nodent longstring_line)*
            (dedent / longstring_error))?)
          /(indent
                longstring_line (nodent longstring_line)*
            (dedent / longstring_error)) |} }) -> Longstring
    longstring_line <- "|" {| ({("\\" / (!string_interpolation [^%nl]))+} / string_interpolation)* |}
    longstring_error <- (({.+} ("" -> "Error while parsing Longstring")) => error)
    string_interpolation <- "\" %ws? (((inline_functioncall / expression) dotdot?) / dotdot) %ws? "\"

    number <- ({ {"-"? (([0-9]+ "." [0-9]+) / ("." [0-9]+) / ([0-9]+)) } }) -> Number

    -- Hack to allow %foo's to parse as "%foo" and "'s" separately
    variable <- ({ ("%" {%wordchar (!"'" %wordchar)*}) }) -> Var

    list <- ({ {|
         ("[..]" indent
                list_line (nodent list_line)*
          (dedent / (({.+} ("" -> "Error while parsing list")) => error)))
        /("[" %ws? (list_line %ws?)? "]")
      |} }) -> List
    list_line <- list_bit (%ws? "," tok_gap list_bit)* (%ws? ",")?
    list_bit <- inline_functioncall / expression

    block_comment <- "#.." [^%nl]* indent [^%nl]* (%nl ((%ws? (!. / &%nl)) / (!%dedented [^%nl]*)))* 
    line_comment  <- "#" [^%nl]*

    eol <- %ws? line_comment? (!. / &%nl)
    ignored_line <- (%nodented (block_comment / line_comment)) / (%ws? (!. / &%nl))
    indent <- eol (%nl ignored_line)* %nl %indented
    nodent <- eol (%nl ignored_line)* %nl %nodented
    dedent <- eol (%nl ignored_line)* (((!.) &%dedented) / (&(%nl %dedented)))
    tok_gap <- %ws / %prev_edge / &("[" / [.,:;{("#%'])
    dotdot <- nodent ".." %ws?
]=]
local whitespace = S(" \t") ^ 1
local defs = {
  ws = whitespace,
  nl = P("\n"),
  wordchar = P(1) - S(' \t\n\r%#:;,.{}[]()"\\'),
  indented = Cmt(S(" \t") ^ 0 * (#(P(1) - S(" \t\n") + (-P(1)))), check_indent),
  nodented = Cmt(S(" \t") ^ 0 * (#(P(1) - S(" \t\n") + (-P(1)))), check_nodent),
  dedented = Cmt(S(" \t") ^ 0 * (#(P(1) - S(" \t\n") + (-P(1)))), check_dedent),
  prev_edge = B(S(" \t\n.,:;}])\"")),
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
    def = function(self, aliases, fn, src, is_macro)
      if is_macro == nil then
        is_macro = false
      end
      if type(aliases) == 'string' then
        aliases = self:get_aliases(aliases)
      end
      if self.debug then
        self:writeln("Defining rule: " .. tostring(repr(aliases)))
      end
      local fn_def = FunctionDef(fn, { }, src, is_macro)
      return self:add_aliases(aliases, fn_def)
    end,
    defmacro = function(self, aliases, fn, src)
      return self:def(aliases, fn, src, true)
    end,
    add_aliases = function(self, aliases, fn_def)
      local first_alias, first_args = next(fn_def.aliases)
      if not first_alias then
        first_alias, first_args = next(aliases)
      end
      for alias, args in pairs(aliases) do
        local _continue_0 = false
        repeat
          if fn_def[alias] then
            _continue_0 = true
            break
          end
          if self.defs[alias] then
            self:remove_alias(alias)
          end
          if alias ~= first_alias and not utils.equivalent(utils.set(args), utils.set(first_args)) then
            self:error("Conflicting argument names between " .. tostring(first_alias) .. " and " .. tostring(alias))
          end
          fn_def.aliases[alias] = args
          self.defs[alias] = fn_def
          _continue_0 = true
        until true
        if not _continue_0 then
          break
        end
      end
    end,
    remove_alias = function(self, alias)
      local fn_def = self.defs[alias]
      if not fn_def then
        return 
      end
      fn_def.aliases[alias] = nil
      self.defs[alias] = nil
    end,
    remove_aliases = function(self, aliases)
      for alias in pairs(aliases) do
        self:remove_alias(alias)
      end
    end,
    get_fn_def = function(self, x)
      if not x then
        self:error("Nothing to get function def from")
      end
      local aliases = self:get_aliases(x)
      local alias, _ = next(aliases)
      return self.defs[alias]
    end,
    call = function(self, alias, ...)
      local fn_def = self.defs[alias]
      if fn_def == nil then
        self:error("Attempt to call undefined function: " .. tostring(alias))
      end
      if fn_def.is_macro and self.callstack[#self.callstack] ~= "__macro__" then
        self:error("Attempt to call macro at runtime: " .. tostring(alias) .. "\nThis can be caused by using a macro in a function that is defined before the macro.")
      end
      if not (self:check_permission(fn_def)) then
        self:error("You do not have the authority to call: " .. tostring(alias))
      end
      local fn, aliases
      fn, aliases = fn_def.fn, fn_def.aliases
      local args
      do
        local _tbl_0 = { }
        for i, name in ipairs(aliases[alias]) do
          _tbl_0[name] = select(i, ...)
        end
        args = _tbl_0
      end
      if self.debug then
        self:writeln("Calling " .. tostring(repr(alias)) .. " with args: " .. tostring(repr(args)))
      end
      insert(self.callstack, alias)
      local rets = {
        fn(self, args)
      }
      remove(self.callstack)
      return unpack(rets)
    end,
    run_macro = function(self, tree, kind)
      if kind == nil then
        kind = "Expression"
      end
      local args
      do
        local _accum_0 = { }
        local _len_0 = 1
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local a = _list_0[_index_0]
          if a.type ~= "Word" then
            _accum_0[_len_0] = a
            _len_0 = _len_0 + 1
          end
        end
        args = _accum_0
      end
      local alias, _ = self:get_alias(tree)
      insert(self.callstack, "__macro__")
      local ret, manual_mode = self:call(alias, unpack(args))
      remove(self.callstack)
      if not ret then
        self:error("No return value for macro: " .. tostring(name))
      end
      if kind == "Statement" and not manual_mode then
        if ret:match("^do\n") then
          error("Attempting to use macro return value as an expression, when it looks like a block:\n" .. tostring(ret))
        end
        ret = "ret = " .. ret
      end
      return ret
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
        self:writeln("PARSING:\n" .. tostring(str))
      end
      str = str:gsub("\r", "") .. "\n"
      local old_indent_stack
      old_indent_stack, indent_stack = indent_stack, {
        0
      }
      local tree = nomsu:match(str)
      indent_stack = old_indent_stack
      if self.debug then
        self:writeln("\nPARSE TREE:")
        self:print_tree(tree)
      end
      assert(tree, "Failed to parse: " .. tostring(str))
      return tree
    end,
    tree_to_value = function(self, tree, vars)
      local code = "\n        return (function(compiler, vars)\nreturn " .. tostring(self:tree_to_lua(tree)) .. "\nend)"
      local lua_thunk, err = load(code)
      if not lua_thunk then
        error("Failed to compile generated code:\n" .. tostring(code) .. "\n\n" .. tostring(err))
      end
      return (lua_thunk())(self, vars or { })
    end,
    tree_to_lua = function(self, tree)
      assert(tree, "No tree provided.")
      if not tree.type then
        self:error("Invalid tree: " .. tostring(repr(tree)))
      end
      local _exp_0 = tree.type
      if "File" == _exp_0 then
        local buffer = {
          [[return (function(compiler, vars)
                        local ret]]
        }
        local vars = { }
        local _list_0 = tree.value.body.value
        for _index_0 = 1, #_list_0 do
          local statement = _list_0[_index_0]
          local ok, code = pcall(self.tree_to_lua, self, statement)
          if not ok then
            self:writeln("Error occurred in statement:\n" .. tostring(statement.src))
            error(code)
          end
          local lua_code = "\n                    return (function(compiler, vars)\n" .. tostring(code) .. "\nend)"
          local lua_thunk, err = load(lua_code)
          if not lua_thunk then
            error("Failed to compile generated code:\n" .. tostring(code) .. "\n\n" .. tostring(err) .. "\n\nProduced by statement:\n" .. tostring(repr(statement)))
          end
          local value = lua_thunk()
          local return_value
          ok, return_value = pcall(value, self, vars)
          if not ok then
            self:writeln("Error occurred in statement:\n" .. tostring(statement.src))
            error(return_value)
          end
          insert(buffer, code)
        end
        insert(buffer, [[                        return ret
                    end)
                ]])
        return concat(buffer, "\n"), return_value
      elseif "Block" == _exp_0 then
        local buffer = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local statement = _list_0[_index_0]
          insert(buffer, self:tree_to_lua(statement))
        end
        return concat(buffer, "\n")
      elseif "Thunk" == _exp_0 then
        assert(tree.value.type == "Block", "Non-block value in Thunk")
        local lua = self:tree_to_lua(tree.value)
        if #tree.value.value == 1 then
          do
            local ret_value = lua:match("^%s*ret = (.*)")
            if ret_value then
              return ([[                            (function(compiler, vars)
                                return %s
                            end)]]):format(ret_value)
            end
          end
        end
        return ([[                    (function(compiler, vars)
                        local ret
                        %s
                        return ret
                    end)]]):format(lua)
      elseif "Statement" == _exp_0 then
        if tree.value.type == "FunctionCall" then
          local alias = self:get_alias(tree.value)
          if self.defs[alias] and self.defs[alias].is_macro then
            return self:run_macro(tree.value, "Statement")
          end
        end
        return "ret = " .. (self:tree_to_lua(tree.value))
      elseif "FunctionCall" == _exp_0 then
        local alias = self:get_alias(tree)
        if self.defs[alias] and self.defs[alias].is_macro then
          return self:run_macro(tree, "Expression")
        else
          local args
          do
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = tree.value
            for _index_0 = 1, #_list_0 do
              local a = _list_0[_index_0]
              if a.type ~= "Word" then
                _accum_0[_len_0] = self:tree_to_lua(a)
                _len_0 = _len_0 + 1
              end
            end
            args = _accum_0
          end
          insert(args, 1, repr(alias))
          return self.__class:comma_separated_items("compiler:call(", args, ")")
        end
      elseif "String" == _exp_0 then
        return repr(self.__class:unescape_string(tree.value))
      elseif "Longstring" == _exp_0 then
        local concat_parts = { }
        local string_buffer = ""
        for i, line in ipairs(tree.value) do
          if i > 1 then
            string_buffer = string_buffer .. "\n"
          end
          for _index_0 = 1, #line do
            local bit = line[_index_0]
            if type(bit) == "string" then
              string_buffer = string_buffer .. bit:gsub("\\\\", "\\")
            else
              if string_buffer ~= "" then
                insert(concat_parts, repr(string_buffer))
                string_buffer = ""
              end
              insert(concat_parts, "compiler.utils.repr_if_not_string(" .. tostring(self:tree_to_lua(bit)) .. ")")
            end
          end
        end
        if string_buffer ~= "" then
          insert(concat_parts, repr(string_buffer))
        end
        if #concat_parts == 0 then
          return "''"
        elseif #concat_parts == 1 then
          return concat_parts[1]
        else
          return "(" .. tostring(concat(concat_parts, "..")) .. ")"
        end
      elseif "Number" == _exp_0 then
        return tree.value
      elseif "List" == _exp_0 then
        if #tree.value == 0 then
          return "{}"
        elseif #tree.value == 1 then
          return "{" .. tostring(self:tree_to_lua(tree.value[1])) .. "}"
        else
          return self.__class:comma_separated_items("{", (function()
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = tree.value
            for _index_0 = 1, #_list_0 do
              local item = _list_0[_index_0]
              _accum_0[_len_0] = self:tree_to_lua(item)
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)(), "}")
        end
      elseif "Var" == _exp_0 then
        return "vars[" .. tostring(repr(tree.value)) .. "]"
      else
        return self:error("Unknown/unimplemented thingy: " .. tostring(tree.type))
      end
    end,
    get_alias = function(self, x)
      if not x then
        self:error("Nothing to get alias from")
      end
      if type(x) == 'string' then
        local alias = x:gsub("'", " '"):gsub("%%%S+", "%%"):gsub("%s+", " ")
        local args
        do
          local _accum_0 = { }
          local _len_0 = 1
          for arg in x:gmatch("%%(%S[^%s']*)") do
            _accum_0[_len_0] = arg
            _len_0 = _len_0 + 1
          end
          args = _accum_0
        end
        return alias, args
      end
      local _exp_0 = x.type
      if "String" == _exp_0 then
        return self:get_alias(x.value)
      elseif "Statement" == _exp_0 then
        return self:get_alias(x.value)
      elseif "FunctionCall" == _exp_0 then
        local alias, args = { }, { }, { }
        local _list_0 = x.value
        for _index_0 = 1, #_list_0 do
          local token = _list_0[_index_0]
          local _exp_1 = token.type
          if "Word" == _exp_1 then
            insert(alias, token.value)
          elseif "Var" == _exp_1 then
            insert(alias, "%")
            insert(args, token.value)
          else
            insert(alias, "%")
            insert(args, token)
          end
        end
        return concat(alias, " "), args
      end
    end,
    get_aliases = function(self, x)
      if not x then
        self:error("Nothing to get aliases from")
      end
      if type(x) == 'string' then
        local alias, args = self:get_alias(x)
        return {
          [alias] = args
        }
      end
      local _exp_0 = x.type
      if "String" == _exp_0 then
        return self:get_aliases({
          x.value
        })
      elseif "Statement" == _exp_0 then
        return self:get_aliases({
          x.value
        })
      elseif "FunctionCall" == _exp_0 then
        return self:get_aliases({
          x
        })
      elseif "List" == _exp_0 then
        x = x.value
      elseif "Block" == _exp_0 then
        x = x.value
      end
      do
        local _with_0 = { }
        for _index_0 = 1, #x do
          local y = x[_index_0]
          local alias, args = self:get_alias(y)
          _with_0[alias] = args
        end
        return _with_0
      end
    end,
    var_to_lua_identifier = function(self, var)
      if var.type ~= "Var" then
        self:error("Tried to convert something that wasn't a Var into a lua identifier: it was not a Var, it was: " .. label.type)
      end
      return "var" .. (var.value:gsub("%W", function(verboten)
        if verboten == "_" then
          return "__"
        else
          return ("_%x"):format(verboten:byte())
        end
      end))
    end,
    _yield_tree = function(self, tree, indent_level)
      if indent_level == nil then
        indent_level = 0
      end
      local ind
      ind = function(s)
        return INDENT:rep(indent_level) .. s
      end
      local _exp_0 = tree.type
      if "File" == _exp_0 then
        coroutine.yield(ind("File:"))
        return self:_yield_tree(tree.value.body, indent_level + 1)
      elseif "Errors" == _exp_0 then
        return coroutine.yield(ind("Error:\n" .. tostring(tree.value)))
      elseif "Block" == _exp_0 then
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local chunk = _list_0[_index_0]
          self:_yield_tree(chunk, indent_level)
        end
      elseif "Thunk" == _exp_0 then
        coroutine.yield(ind("Thunk:"))
        return self:_yield_tree(tree.value, indent_level + 1)
      elseif "Statement" == _exp_0 then
        return self:_yield_tree(tree.value, indent_level)
      elseif "FunctionCall" == _exp_0 then
        local alias = self:get_alias(tree)
        local args
        do
          local _accum_0 = { }
          local _len_0 = 1
          local _list_0 = tree.value
          for _index_0 = 1, #_list_0 do
            local a = _list_0[_index_0]
            if a.type ~= "Word" then
              _accum_0[_len_0] = a
              _len_0 = _len_0 + 1
            end
          end
          args = _accum_0
        end
        if #args == 0 then
          return coroutine.yield(ind("Call [" .. tostring(alias) .. "]!"))
        else
          coroutine.yield(ind("Call [" .. tostring(alias) .. "]:"))
          for _index_0 = 1, #args do
            local a = args[_index_0]
            self:_yield_tree(a, indent_level + 1)
          end
        end
      elseif "String" == _exp_0 then
        return coroutine.yield(ind(repr(tree.value)))
      elseif "Longstring" == _exp_0 then
        return coroutine.yield(ind(repr(tree.value)))
      elseif "Number" == _exp_0 then
        return coroutine.yield(ind(tree.value))
      elseif "Var" == _exp_0 then
        return coroutine.yield(ind("Var[" .. tostring(repr(tree.value)) .. "]"))
      elseif "List" == _exp_0 then
        if #tree.value == 0 then
          return coroutine.yield(ind("<Empty List>"))
        else
          coroutine.yield(ind("List:"))
          local _list_0 = tree.value
          for _index_0 = 1, #_list_0 do
            local item = _list_0[_index_0]
            self:_yield_tree(item, indent_level + 1)
          end
        end
      else
        return error("Unknown/unimplemented thingy: " .. tostring(tree.type))
      end
    end,
    print_tree = function(self, tree)
      for line in coroutine.wrap(function()
        return self:_yield_tree(tree)
      end) do
        self:writeln(line)
      end
    end,
    stringify_tree = function(self, tree)
      local result = { }
      for line in coroutine.wrap(function()
        return self:_yield_tree(tree)
      end) do
        insert(result, line)
      end
      return concat(result, "\n")
    end,
    run = function(self, src, filename, output_file)
      if output_file == nil then
        output_file = nil
      end
      if self.debug then
        self:writeln("COMPILING:\n" .. tostring(src))
      end
      local tree = self:parse(src, filename)
      assert(tree, "Tree failed to compile: " .. tostring(src))
      local code, retval = self:tree_to_lua(tree)
      if output_file then
        local output = io.open(output_file, "w")
        output:write(code)
      end
      return retval, code
    end,
    error = function(self, ...)
      self:writeln("ERROR!")
      self:writeln(...)
      self:writeln("Callstack:")
      for i = #self.callstack, 1, -1 do
        self:writeln("    " .. tostring(self.callstack[i]))
      end
      self:writeln("    <top level>")
      self.callstack = { }
      return error()
    end,
    test = function(self, src, filename, expected)
      local i = 1
      while i ~= nil do
        local start, stop = src:find("\n\n", i)
        local test = src:sub(i, start)
        i = stop
        start, stop = test:find("===")
        if not start or not stop then
          self:error("WHERE'S THE ===? in:\n" .. tostring(test))
        end
        local test_src
        test_src, expected = test:sub(1, start - 1), test:sub(stop + 1, -1)
        expected = expected:match('[\n]*(.*[^\n])')
        local tree = self:parse(test_src, filename)
        local got = self:stringify_tree(tree.value.body)
        if got ~= expected then
          self:error("TEST FAILED!\nSource:\n" .. tostring(test_src) .. "\nExpected:\n" .. tostring(expected) .. "\n\nGot:\n" .. tostring(got))
        end
      end
    end,
    initialize_core = function(self)
      self:defmacro("lua block %lua_code", function(self, vars, kind)
        if kind == "Expression" then
          error("Expected to be in statement.")
        end
        local inner_vars = setmetatable({ }, {
          __index = function(_, key)
            return "vars[" .. tostring(repr(key)) .. "]"
          end
        })
        local lua = self:tree_to_value(vars.lua_code, inner_vars)
        if not lua:match("^do\n.*\nend$") then
          lua = "do\n" .. tostring(lua) .. "\nend"
        end
        return lua, true
      end)
      self:defmacro("lua expr %lua_code", function(self, vars, kind)
        local lua_code = vars.lua_code.value
        local inner_vars = setmetatable({ }, {
          __index = function(_, key)
            return "vars[" .. tostring(repr(key)) .. "]"
          end
        })
        local lua = self:tree_to_value(vars.lua_code, inner_vars)
        return lua
      end)
      self:def("require %filename", function(self, vars)
        if not self.loaded_files[vars.filename] then
          local file = io.open(vars.filename)
          if not file then
            self:error("File does not exist: " .. tostring(vars.filename))
          end
          self.loaded_files[vars.filename] = (self:run(file:read('*a'), vars.filename)) or true
        end
        return self.loaded_files[vars.filename]
      end)
      return self:def("run file %filename", function(self, vars)
        local file = io.open(vars.filename)
        if not file then
          self:error("File does not exist: " .. tostring(vars.filename))
        end
        return self:run(file:read('*a'), vars.filename)
      end)
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
      self:initialize_core()
      self.utils = utils
      self.repr = function(self, ...)
        return repr(...)
      end
      self.loaded_files = { }
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
if arg and arg[1] then
  local c = NomsuCompiler()
  local input = io.open(arg[1]):read("*a")
  local _write = c.write
  if arg[2] == "-" then
    c.write = function() end
  end
  local retval, code = c:run(input, arg[1])
  c.write = _write
  if arg[2] then
    local output
    if arg[2] == "-" then
      output = io.output()
    else
      output = io.open(arg[2], 'w')
    end
    output:write([[    local load = function()
    ]])
    output:write(code)
    output:write([[
    end
    local NomsuCompiler = require('nomsu')
    local c = NomsuCompiler()
    return load()(c, {})
    ]])
  end
elseif arg then
  local c = NomsuCompiler()
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
return NomsuCompiler
