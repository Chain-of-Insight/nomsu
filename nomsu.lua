local re = require('re')
local lpeg = require('lpeg')
local utils = require('utils')
local insert = table.insert
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
local NomsuCompiler
do
  local _class_0
  local _base_0 = {
    writeln = function(self, ...)
      self:write(...)
      return self:write("\n")
    end,
    call = function(self, fn_name, ...)
      local fn_info = self.defs[fn_name]
      if fn_info == nil then
        self:error("Attempt to call undefined function: " .. tostring(fn_name))
      end
      if fn_info.is_macro then
        self:error("Attempt to call macro at runtime: " .. tostring(fn_name) .. "\nThis can be caused by using a macro in a function that is defined before the macro.")
      end
      if not (self:check_permission(fn_name)) then
        self:error("You do not have the authority to call: " .. tostring(fn_name))
      end
      insert(self.callstack, fn_name)
      local fn, arg_names
      fn, arg_names = fn_info.fn, fn_info.arg_names
      local args
      do
        local _tbl_0 = { }
        for i, name in ipairs(arg_names[fn_name]) do
          _tbl_0[name] = select(i, ...)
        end
        args = _tbl_0
      end
      if self.debug then
        self:writeln("Calling " .. tostring(fn_name) .. " with args: " .. tostring(utils.repr(args)))
      end
      local ret = fn(self, args)
      table.remove(self.callstack)
      return ret
    end,
    check_permission = function(self, fn_name)
      local fn_info = self.defs[fn_name]
      if fn_info == nil then
        self:error("Undefined function: " .. tostring(fn_name))
      end
      if fn_info.whiteset == nil then
        return true
      end
      local _list_0 = self.callstack
      for _index_0 = 1, #_list_0 do
        local caller = _list_0[_index_0]
        if fn_info.whiteset[caller] then
          return true
        end
      end
      return false
    end,
    def = function(self, spec, fn, src)
      if self.debug then
        self:writeln("Defining rule: " .. tostring(spec))
      end
      local invocations, arg_names = self:get_invocations(spec)
      for i = 2, #invocations do
        if not utils.equivalent(utils.set(arg_names[invocations[1]]), utils.set(arg_names[invocations[i]])) then
          self:error("Conflicting argument names " .. tostring(utils.repr(invocations[1])) .. " and " .. tostring(utils.repr(invocations[i])) .. " for " .. tostring(utils.repr(spec)))
        end
      end
      local fn_info = {
        fn = fn,
        arg_names = arg_names,
        invocations = invocations,
        src = src,
        is_macro = false
      }
      for _index_0 = 1, #invocations do
        local invocation = invocations[_index_0]
        self.defs[invocation] = fn_info
      end
    end,
    get_invocations_from_definition = function(self, def, vars)
      if def.type == "String" then
        return self.__class:unescape_string(def.value)
      end
      if def.type ~= "List" then
        self:error("Trying to get invocations from " .. tostring(def.type) .. ", but expected List or String.")
      end
      local invocations = { }
      local _list_0 = def.value
      for _index_0 = 1, #_list_0 do
        local _continue_0 = false
        repeat
          local item = _list_0[_index_0]
          if item.type == "String" then
            insert(invocations, item.value)
            _continue_0 = true
            break
          end
          if item.type ~= "FunctionCall" then
            self:error("Invalid list item: " .. tostring(item.type) .. ", expected FunctionCall or String")
          end
          local name_bits = { }
          local _list_1 = item.value
          for _index_1 = 1, #_list_1 do
            local token = _list_1[_index_1]
            if token.type == "Word" then
              insert(name_bits, token.value)
            elseif token.type == "Var" then
              insert(name_bits, token.src)
            else
              self:error("Unexpected token type in definition: " .. tostring(token.type) .. " (expected Word or Var)")
            end
          end
          insert(invocations, table.concat(name_bits, " "))
          _continue_0 = true
        until true
        if not _continue_0 then
          break
        end
      end
      return invocations
    end,
    get_invocations = function(self, text)
      if not text then
        self:error("No text provided!")
      end
      if type(text) == 'function' then
        error("Function passed to get_invocations")
      end
      if type(text) == 'string' then
        text = {
          text
        }
      end
      local invocations = { }
      local arg_names = { }
      for _index_0 = 1, #text do
        local _text = text[_index_0]
        local invocation = _text:gsub("'", " '"):gsub("%%%S+", "%%"):gsub("%s+", " ")
        local _arg_names
        do
          local _accum_0 = { }
          local _len_0 = 1
          for arg in _text:gmatch("%%(%S[^%s']*)") do
            _accum_0[_len_0] = arg
            _len_0 = _len_0 + 1
          end
          _arg_names = _accum_0
        end
        insert(invocations, invocation)
        arg_names[invocation] = _arg_names
      end
      return invocations, arg_names
    end,
    defmacro = function(self, spec, lua_gen_fn, src)
      if self.debug then
        self:writeln("DEFINING MACRO: " .. tostring(spec) .. tostring(src or ""))
      end
      local invocations, arg_names = self:get_invocations(spec)
      for i = 2, #invocations do
        if not utils.equivalent(utils.set(arg_names[invocations[1]]), utils.set(arg_names[invocations[i]])) then
          self:error("Conflicting argument names " .. tostring(utils.repr(invocations[1])) .. " and " .. tostring(utils.repr(invocations[i])) .. " for " .. tostring(utils.repr(spec)))
        end
      end
      local fn_info = {
        fn = lua_gen_fn,
        arg_names = arg_names,
        invocations = invocations,
        src = src,
        is_macro = true
      }
      for _index_0 = 1, #invocations do
        local invocation = invocations[_index_0]
        self.defs[invocation] = fn_info
      end
    end,
    parse = function(self, str, filename)
      if self.debug then
        self:writeln("PARSING:\n" .. tostring(str))
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
          table.remove(indent_stack)
          return end_pos
        end
      end
      local check_nodent
      check_nodent = function(subject, end_pos, spaces)
        if #spaces == indent_stack[#indent_stack] then
          return end_pos
        end
      end
      local lingo = [=[            file <- ({ {| shebang? {:body: block :} %nl* (({.+} ("" -> "Unexpected end of file")) => error)? |} }) -> File

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
            block_functioncall <- "(..)" indent functioncall (dedent / (({.+} ("" -> "Error while parsing block function call")) => error))

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
          local fn
          fn = function(src, value, errors)
            local token = {
              type = key,
              src = src,
              value = value,
              errors = errors
            }
            return token
          end
          t[key] = fn
          return fn
        end
      })
      lingo = re.compile(lingo, defs)
      local tree = lingo:match(str:gsub("\r", "") .. "\n")
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
        self:error("Invalid tree: " .. tostring(utils.repr(tree)))
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
            error("Failed to compile generated code:\n" .. tostring(code) .. "\n\n" .. tostring(err) .. "\n\nProduced by statement:\n" .. tostring(utils.repr(statement)))
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
        return table.concat(buffer, "\n"), return_value
      elseif "Block" == _exp_0 then
        local buffer = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local statement = _list_0[_index_0]
          insert(buffer, self:tree_to_lua(statement))
        end
        return table.concat(buffer, "\n")
      elseif "Thunk" == _exp_0 then
        assert(tree.value.type == "Block", "Non-block value in Thunk")
        return [[                    (function(compiler, vars)
                        local ret
                        ]] .. self:tree_to_lua(tree.value) .. "\n" .. [[                        return ret
                    end)
                ]]
      elseif "Statement" == _exp_0 then
        if tree.value.type == "FunctionCall" then
          local name = self:fn_name_from_tree(tree.value)
          if self.defs[name] and self.defs[name].is_macro then
            return self:run_macro(tree.value, "Statement")
          end
        end
        return "ret = " .. (self:tree_to_lua(tree.value))
      elseif "FunctionCall" == _exp_0 then
        local name = self:fn_name_from_tree(tree)
        if self.defs[name] and self.defs[name].is_macro then
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
          insert(args, 1, utils.repr(name))
          return self.__class:comma_separated_items("compiler:call(", args, ")")
        end
      elseif "String" == _exp_0 then
        return utils.repr(self.__class:unescape_string(tree.value))
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
                insert(concat_parts, utils.repr(string_buffer))
                string_buffer = ""
              end
              insert(concat_parts, "compiler.utils.repr_if_not_string(" .. tostring(self:tree_to_lua(bit)) .. ")")
            end
          end
        end
        if string_buffer ~= "" then
          insert(concat_parts, utils.repr(string_buffer))
        end
        if #concat_parts == 0 then
          return "''"
        elseif #concat_parts == 1 then
          return concat_parts[1]
        else
          return "(" .. tostring(table.concat(concat_parts, "..")) .. ")"
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
        return "vars[" .. tostring(utils.repr(tree.value)) .. "]"
      else
        return self:error("Unknown/unimplemented thingy: " .. tostring(tree.type))
      end
    end,
    fn_name_from_tree = function(self, tree)
      assert(tree.type == "FunctionCall", "Attempt to get fn name from non-functioncall tree: " .. tostring(tree.type))
      local name_bits = { }
      local _list_0 = tree.value
      for _index_0 = 1, #_list_0 do
        local token = _list_0[_index_0]
        insert(name_bits, (function()
          if token.type == "Word" then
            return token.value
          else
            return "%"
          end
        end)())
      end
      return table.concat(name_bits, " ")
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
    run_macro = function(self, tree, kind)
      if kind == nil then
        kind = "Expression"
      end
      local name = self:fn_name_from_tree(tree)
      if not (self.defs[name] and self.defs[name].is_macro) then
        self:error("Macro not found: " .. tostring(name))
      end
      if not (self:check_permission(name)) then
        self:error("You do not have the authority to call: " .. tostring(name))
      end
      local fn, arg_names
      do
        local _obj_0 = self.defs[name]
        fn, arg_names = _obj_0.fn, _obj_0.arg_names
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
      do
        local _tbl_0 = { }
        for i, name in ipairs(arg_names[name]) do
          _tbl_0[name] = args[i]
        end
        args = _tbl_0
      end
      insert(self.callstack, name)
      local ret, manual_mode = fn(self, args, kind)
      table.remove(self.callstack)
      if not ret then
        self:error("No return value for macro: " .. tostring(name))
      end
      if kind == "Statement" and not manual_mode then
        ret = "ret = " .. ret
      end
      return ret
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
        self:_yield_tree(tree.value.body, indent_level + 1)
      elseif "Errors" == _exp_0 then
        coroutine.yield(ind("Error:\n" .. tostring(tree.value)))
      elseif "Block" == _exp_0 then
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local chunk = _list_0[_index_0]
          self:_yield_tree(chunk, indent_level)
        end
      elseif "Thunk" == _exp_0 then
        coroutine.yield(ind("Thunk:"))
        self:_yield_tree(tree.value, indent_level + 1)
      elseif "Statement" == _exp_0 then
        self:_yield_tree(tree.value, indent_level)
      elseif "FunctionCall" == _exp_0 then
        local name = self:fn_name_from_tree(tree)
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
          coroutine.yield(ind("Call [" .. tostring(name) .. "]!"))
        else
          coroutine.yield(ind("Call [" .. tostring(name) .. "]:"))
          for _index_0 = 1, #args do
            local a = args[_index_0]
            self:_yield_tree(a, indent_level + 1)
          end
        end
      elseif "String" == _exp_0 then
        coroutine.yield(ind(utils.repr(tree.value)))
      elseif "Longstring" == _exp_0 then
        coroutine.yield(ind(utils.repr(tree.value)))
      elseif "Number" == _exp_0 then
        coroutine.yield(ind(tree.value))
      elseif "List" == _exp_0 then
        if #tree.value == 0 then
          coroutine.yield(ind("<Empty List>"))
        else
          coroutine.yield(ind("List:"))
          local _list_0 = tree.value
          for _index_0 = 1, #_list_0 do
            local item = _list_0[_index_0]
            self:_yield_tree(item, indent_level + 1)
          end
        end
      elseif "Var" == _exp_0 then
        coroutine.yield(ind("Var[" .. tostring(utils.repr(tree.value)) .. "]"))
      else
        error("Unknown/unimplemented thingy: " .. tostring(tree.type))
      end
      return nil
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
      return table.concat(result, "\n")
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
      self:defmacro([[lua block %lua_code]], function(self, vars, kind)
        if kind == "Expression" then
          error("Expected to be in statement.")
        end
        local inner_vars = setmetatable({ }, {
          __index = function(_, key)
            return error("vars[" .. tostring(utils.repr(key)) .. "]")
          end
        })
        return "do\n" .. self:tree_to_value(vars.lua_code, inner_vars) .. "\nend", true
      end)
      self:defmacro([[lua expr %lua_code]], function(self, vars, kind)
        local lua_code = vars.lua_code.value
        local inner_vars = setmetatable({ }, {
          __index = function(_, key)
            return error("vars[" .. tostring(utils.repr(key)) .. "]")
          end
        })
        return self:tree_to_value(vars.lua_code, inner_vars)
      end)
      self:def("require %filename", function(self, vars)
        if not self.loaded_files[vars.filename] then
          local file = io.open(vars.filename)
          if not file then
            self:error("File does not exist: " .. tostring(vars.filename))
          end
          self.loaded_files[vars.filename] = self:run(file:read('*a'), vars.filename)
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
    return table.concat(bits)
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
      print("= " .. utils.repr(ret))
    end
  end
end
return NomsuCompiler
