local re = require('re')
local lpeg = require('lpeg')
local utils = require('utils')
local INDENT = "    "
lpeg.setmaxstack(10000)
local P, V, S, Cg, C, Cp, B, Cmt
P, V, S, Cg, C, Cp, B, Cmt = lpeg.P, lpeg.V, lpeg.S, lpeg.Cg, lpeg.C, lpeg.Cp, lpeg.B, lpeg.Cmt
local get_line_indentation
get_line_indentation = function(line)
  local indent_amounts = {
    [" "] = 1,
    ["\t"] = 4
  }
  do
    local sum = 0
    local leading_space = line:match("[\t ]*")
    for c in leading_space:gmatch("[\t ]") do
      sum = sum + indent_amounts[c]
    end
    return sum
  end
end
local make_parser
make_parser = function(lingo, extra_definitions)
  local indent_stack = {
    0
  }
  local push
  push = function(n)
    return table.insert(indent_stack, n)
  end
  local pop
  pop = function()
    return table.remove(indent_stack)
  end
  local check_indent
  check_indent = function(subject, end_pos, spaces)
    local num_spaces = get_line_indentation(spaces)
    if num_spaces <= indent_stack[#indent_stack] then
      return nil
    end
    push(num_spaces)
    return end_pos
  end
  local check_dedent
  check_dedent = function(subject, end_pos, spaces)
    local num_spaces = get_line_indentation(spaces)
    if num_spaces >= indent_stack[#indent_stack] then
      return nil
    end
    pop()
    return end_pos
  end
  local check_nodent
  check_nodent = function(subject, end_pos, spaces)
    local num_spaces = get_line_indentation(spaces)
    if num_spaces ~= indent_stack[#indent_stack] then
      return nil
    end
    return end_pos
  end
  local wordchar = P(1) - S(' \t\n\r%#:;,.{}[]()"\\')
  local nl = P("\n")
  local whitespace = S(" \t") ^ 1
  local blank_line = whitespace ^ -1 * nl
  local line_comment = re.compile([=[ "#" [^%nl]* ]=], {
    nl = nl
  })
  local block_comment = re.compile([=[        "#.." (!%nl .)* (%indent (!%dedent %nl [^%nl]*)*)
    ]=], {
    nl = nl,
    whitespace = whitespace,
    indent = #(nl * blank_line ^ 0 * Cmt(S(" \t") ^ 0, check_indent)),
    dedent = #(nl * blank_line ^ 0 * Cmt(S(" \t") ^ 0, check_dedent)),
    new_line = nl * blank_line ^ 0 * Cmt(S(" \t") ^ 0, check_nodent)
  })
  blank_line = ((Cmt(whitespace ^ -1, check_nodent) * (block_comment + line_comment)) ^ -1 + whitespace ^ -1) * nl
  local defs = {
    wordchar = wordchar,
    nl = nl,
    ws = whitespace,
    blank_line = blank_line,
    block_comment = block_comment,
    line_comment = line_comment,
    eol = #nl + (P("") - P(1)),
    word_boundary = whitespace ^ -1 + B(P("..")) + B(S("\";)]")) + #S("\":([") + #((whitespace + nl) ^ 0 * P("..")),
    indent = #(nl * blank_line ^ 0 * Cmt(whitespace ^ -1, check_indent)),
    dedent = #(nl * blank_line ^ 0 * Cmt(whitespace ^ -1, check_dedent)),
    new_line = nl * blank_line ^ 0 * Cmt(whitespace ^ -1, check_nodent),
    error_handler = function(src, pos, errors)
      local line_no = 1
      for _ in src:sub(1, -#errors):gmatch("\n") do
        line_no = line_no + 1
      end
      local err_pos = #src - #errors + 1
      if errors:sub(1, 1) == "\n" then
        err_pos = err_pos + #errors:match("[ \t]*", 2)
      end
      local start_of_err_line = err_pos
      while src:sub(start_of_err_line, start_of_err_line) ~= "\n" do
        start_of_err_line = start_of_err_line - 1
      end
      local start_of_prev_line = start_of_err_line - 1
      while src:sub(start_of_prev_line, start_of_prev_line) ~= "\n" do
        start_of_prev_line = start_of_prev_line - 1
      end
      local prev_line, err_line, next_line = src:match("([^\n]*)\n([^\n]*)\n([^\n]*)", start_of_prev_line + 1)
      local pointer = ("-"):rep(err_pos - start_of_err_line + 0) .. "^"
      return error("\nParse error on line " .. tostring(line_no) .. ":\n\n" .. tostring(prev_line) .. "\n" .. tostring(err_line) .. "\n" .. tostring(pointer) .. "\n" .. tostring(next_line) .. "\n")
    end
  }
  if extra_definitions then
    for k, v in pairs(extra_definitions) do
      defs[k] = v
    end
  end
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
  return re.compile(lingo, defs)
end
local NomsuCompiler
do
  local _class_0
  local _base_0 = {
    call = function(self, fn_name, ...)
      local fn_info = self.defs[fn_name]
      if fn_info == nil then
        self:error("Attempt to call undefined function: " .. tostring(fn_name))
      end
      if fn_info.is_macro then
        self:error("Attempt to call macro at runtime: " .. tostring(fn_name))
      end
      if not (self:check_permission(fn_name)) then
        self:error("You do not have the authority to call: " .. tostring(fn_name))
      end
      table.insert(self.callstack, fn_name)
      local fn, arg_names
      fn, arg_names = fn_info.fn, fn_info.arg_names
      local args
      do
        local _tbl_0 = { }
        for i, name in ipairs(arg_names) do
          _tbl_0[name] = select(i, ...)
        end
        args = _tbl_0
      end
      if self.debug then
        print("Calling " .. tostring(fn_name) .. " with args: " .. tostring(utils.repr(args)))
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
    def = function(self, spec, fn)
      if self.debug then
        print("Defining rule: " .. tostring(spec))
      end
      local invocations, arg_names = self:get_invocations(spec)
      local fn_info = {
        fn = fn,
        arg_names = arg_names,
        invocations = invocations,
        is_macro = false
      }
      for _index_0 = 1, #invocations do
        local invocation = invocations[_index_0]
        self.defs[invocation] = fn_info
      end
    end,
    get_invocations = function(self, text)
      if type(text) == 'string' then
        text = {
          text
        }
      end
      local invocations = { }
      local arg_names
      for _index_0 = 1, #text do
        local _text = text[_index_0]
        local invocation = _text:gsub("%%%S+", "%%")
        local _arg_names
        do
          local _accum_0 = { }
          local _len_0 = 1
          for arg in _text:gmatch("%%(%S+)") do
            _accum_0[_len_0] = arg
            _len_0 = _len_0 + 1
          end
          _arg_names = _accum_0
        end
        table.insert(invocations, invocation)
        if arg_names then
          if not utils.equivalent(utils.set(arg_names), utils.set(_arg_names)) then
            self:error("Conflicting argument names " .. tostring(utils.repr(arg_names)) .. " and " .. tostring(utils.repr(_arg_names)) .. " for " .. tostring(utils.repr(text)))
          end
        else
          arg_names = _arg_names
        end
      end
      return invocations, arg_names
    end,
    defmacro = function(self, spec, lua_gen_fn)
      local invocations, arg_names = self:get_invocations(spec)
      local fn_info = {
        fn = lua_gen_fn,
        arg_names = arg_names,
        invocations = invocations,
        is_macro = true
      }
      for _index_0 = 1, #invocations do
        local invocation = invocations[_index_0]
        self.defs[invocation] = fn_info
      end
    end,
    run = function(self, text)
      if self.debug then
        print("RUNNING TEXT:\n" .. tostring(text))
      end
      local code = self:compile(text)
      if self.debug then
        print("\nGENERATED LUA CODE:\n" .. tostring(code))
      end
      return code
    end,
    parse = function(self, str)
      if self.debug then
        print("PARSING:\n" .. tostring(str))
      end
      local lingo = [=[            file <- ({ {| %blank_line* {:body: block :} %blank_line* (errors)? |} }) -> File
            errors <- (({.+}) => error_handler)
            block <- ({ {| statement (%new_line statement)* |} }) -> Block
            statement <- ({ (functioncall / expression) }) -> Statement
            one_liner <- ({ {|
                    (({ 
                        (({ {|
                            (expression (%word_boundary fn_bit)+) / (word (%word_boundary fn_bit)*)
                        |} }) -> FunctionCall)
                        / (expression)
                     }) -> Statement)
                |} }) -> Block

            functioncall <- ({ {| (expression %word_boundary fn_bits) / (word (%word_boundary fn_bits)?) |} }) -> FunctionCall
            fn_bit <- (expression / word)
            fn_bits <-
                ((".." %ws? %line_comment? (%indent %new_line indented_fn_bits %dedent) (%new_line ".." %ws? fn_bits)?)
                 / (%new_line ".." fn_bit (%word_boundary fn_bits)?)
                 / (fn_bit (%word_boundary fn_bits)?))
            indented_fn_bits <-
                fn_bit ((%new_line / %word_boundary) indented_fn_bits)?
            
            thunk <-
                ({ ":" %ws? %line_comment?
                   ((%indent %new_line block ((%dedent (%new_line "..")?) / errors))
                    / (one_liner (%ws? (%new_line? ".."))?)) }) -> Thunk

            word <- ({ !number {%wordchar (!"'" %wordchar)*} }) -> Word
            expression <- ({ (longstring / string / number / variable / list / thunk / subexpression) }) -> Expression

            string <- ({ (!longstring) '"' {(("\" [^%nl]) / [^"%nl])*} '"' }) -> String
            longstring <- ({ '".."' %ws?
                {|
                    (("|" {| ({("\\" / (!string_interpolation [^%nl]))+} / string_interpolation)* |})
                     / %line_comment)?
                    (%indent
                        (%new_line "|" {|
                            ({("\\" / (!string_interpolation [^%nl]))+} / string_interpolation)*
                        |})+
                    ((%dedent (%new_line '..')?) / errors))?
                |}}) -> Longstring
            string_interpolation <- "\" %ws? (functioncall / expression) %ws? "\"
            number <- ({ {'-'? [0-9]+ ("." [0-9]+)?} }) -> Number
            variable <- ({ ("%" {%wordchar (!"'" %wordchar)*}) }) -> Var

            subexpression <-
                ("(" %ws? (functioncall / expression) %ws? ")")
                / ("(..)" %ws? %line_comment? %indent %new_line ((({ {| indented_fn_bits |} }) -> FunctionCall) / expression) %dedent (%new_line "..")?)

            list <- ({ {|
                ("[..]" %ws? %line_comment? %indent %new_line indented_list ","? ((%dedent (%new_line "..")?) / errors))
                / ("[" %ws? (list_items ","?)?  %ws?"]")
              |} }) -> List
            list_items <- ((functioncall / expression) (list_sep list_items)?)
            list_sep <- %ws? "," %ws?
            indented_list <-
                (functioncall / expression) (((list_sep (%line_comment? %new_line)?) / (%line_comment? %new_line)) indented_list)?
        ]=]
      lingo = make_parser(lingo)
      local tree = lingo:match(str:gsub("\r", "") .. "\n")
      if self.debug then
        print("\nPARSE TREE:")
        self:print_tree(tree)
      end
      assert(tree, "Failed to parse: " .. tostring(str))
      return tree
    end,
    tree_to_value = function(self, tree, vars)
      local code = "\n        local utils = require('utils')\n        return (function(compiler, vars)\nreturn " .. tostring(self:tree_to_lua(tree)) .. "\nend)"
      local lua_thunk, err = load(code)
      if not lua_thunk then
        error("Failed to compile generated code:\n" .. tostring(code) .. "\n\n" .. tostring(err))
      end
      return (lua_thunk())(self, vars or { })
    end,
    tree_to_lua = function(self, tree, kind)
      if kind == nil then
        kind = "Expression"
      end
      assert(tree, "No tree provided.")
      local indent = ""
      local buffer = { }
      local to_lua
      to_lua = function(t, kind)
        local ret = self:tree_to_lua(t, kind)
        return ret
      end
      local add
      add = function(code)
        return table.insert(buffer, code)
      end
      local _exp_0 = tree.type
      if "File" == _exp_0 then
        add([[return (function(compiler, vars)
                        local ret]])
        local vars = { }
        local _list_0 = tree.value.body.value
        for _index_0 = 1, #_list_0 do
          local statement = _list_0[_index_0]
          local code = to_lua(statement)
          local lua_thunk, err = load("\n                    local utils = require('utils')\n                    return (function(compiler, vars)\n" .. tostring(code) .. "\nend)")
          if not lua_thunk then
            error("Failed to compile generated code:\n" .. tostring(code) .. "\n\n" .. tostring(err) .. "\n\nProduced by statement:\n" .. tostring(utils.repr(statement)))
          end
          local ok
          ok, err = pcall(lua_thunk)
          if not ok then
            error(err)
          end
          ok, err = pcall(err, self, vars)
          if not ok then
            self:error(err)
          end
          add(code)
        end
        add([[                        return ret
                    end)
                ]])
      elseif "Block" == _exp_0 then
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local statement = _list_0[_index_0]
          add(to_lua(statement))
        end
      elseif "Thunk" == _exp_0 then
        assert(tree.value.type == "Block", "Non-block value in Thunk")
        add([[                    (function(compiler, vars)
                        local ret]])
        add(to_lua(tree.value))
        add([[                        return ret
                    end)
                ]])
      elseif "Statement" == _exp_0 then
        if tree.value.type == "FunctionCall" then
          local name = self:fn_name_from_tree(tree.value)
          if self.defs[name] and self.defs[name].is_macro then
            add(self:run_macro(tree.value, "Statement"))
          else
            add("ret = " .. (to_lua(tree.value):match("%s*(.*)")))
          end
        else
          add("ret = " .. (to_lua(tree.value):match("%s*(.*)")))
        end
      elseif "Expression" == _exp_0 then
        add(to_lua(tree.value))
      elseif "FunctionCall" == _exp_0 then
        local name = self:fn_name_from_tree(tree)
        if self.defs[name] and self.defs[name].is_macro then
          add(self:run_macro(tree, "Expression"))
        else
          local args
          do
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = tree.value
            for _index_0 = 1, #_list_0 do
              local a = _list_0[_index_0]
              if a.type ~= "Word" then
                _accum_0[_len_0] = to_lua(a)
                _len_0 = _len_0 + 1
              end
            end
            args = _accum_0
          end
          table.insert(args, 1, utils.repr(name, true))
          add(self.__class:comma_separated_items("compiler:call(", args, ")"))
        end
      elseif "String" == _exp_0 then
        local escapes = {
          n = "\n",
          t = "\t",
          b = "\b",
          a = "\a",
          v = "\v",
          f = "\f",
          r = "\r"
        }
        local unescaped = tree.value:gsub("\\(.)", (function(c)
          return escapes[c] or c
        end))
        add(utils.repr(unescaped, true))
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
                table.insert(concat_parts, utils.repr(string_buffer, true))
                string_buffer = ""
              end
              table.insert(concat_parts, "utils.repr(" .. tostring(to_lua(bit)) .. ")")
            end
          end
        end
        if string_buffer ~= "" then
          table.insert(concat_parts, utils.repr(string_buffer, true))
        end
        if #concat_parts == 0 then
          add("''")
        elseif #concat_parts == 1 then
          add(concat_parts[1])
        else
          add("(" .. tostring(table.concat(concat_parts, "..")) .. ")")
        end
      elseif "Number" == _exp_0 then
        add(tree.value)
      elseif "List" == _exp_0 then
        if #tree.value == 0 then
          add("{}")
        elseif #tree.value == 1 then
          add("{" .. tostring(to_lua(tree.value[1])) .. "}")
        else
          add(self.__class:comma_separated_items("{", (function()
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = tree.value
            for _index_0 = 1, #_list_0 do
              local item = _list_0[_index_0]
              _accum_0[_len_0] = to_lua(item)
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)(), "}"))
        end
      elseif "Var" == _exp_0 then
        add("vars[" .. tostring(utils.repr(tree.value, true)) .. "]")
      else
        error("Unknown/unimplemented thingy: " .. tostring(tree.type))
      end
      buffer = table.concat(buffer, "\n")
      return buffer
    end,
    fn_name_from_tree = function(self, tree)
      assert(tree.type == "FunctionCall", "Attempt to get fn name from non-functioncall tree: " .. tostring(tree.type))
      local name_bits = { }
      local _list_0 = tree.value
      for _index_0 = 1, #_list_0 do
        local token = _list_0[_index_0]
        table.insert(name_bits, (function()
          if token.type == "Word" then
            return token.value
          else
            return "%"
          end
        end)())
      end
      return table.concat(name_bits, " ")
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
        for i, name in ipairs(arg_names) do
          _tbl_0[name] = args[i]
        end
        args = _tbl_0
      end
      table.insert(self.callstack, name)
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
      elseif "Expression" == _exp_0 then
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
        coroutine.yield(ind(utils.repr(tree.value, true)))
      elseif "Longstring" == _exp_0 then
        coroutine.yield(ind(utils.repr(tree.value, true)))
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
        print(line)
      end
    end,
    stringify_tree = function(self, tree)
      local result = { }
      for line in coroutine.wrap(function()
        return self:_yield_tree(tree)
      end) do
        table.insert(result, line)
      end
      return table.concat(result, "\n")
    end,
    compile = function(self, src, output_file)
      if output_file == nil then
        output_file = nil
      end
      if self.debug then
        print("COMPILING:\n" .. tostring(src))
      end
      local tree = self:parse(src)
      assert(tree, "Tree failed to compile: " .. tostring(src))
      local code = self:tree_to_lua(tree)
      if output_file then
        local output = io.open(output_file, "w")
        output:write(code)
      end
      return code
    end,
    error = function(self, ...)
      print(...)
      print("Callstack:")
      for i = #self.callstack, 1, -1 do
        print("    " .. tostring(self.callstack[i]))
      end
      return error()
    end,
    test = function(self, src, expected)
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
        local tree = self:parse(test_src)
        local got = self:stringify_tree(tree.value.body)
        if got ~= expected then
          self:error("TEST FAILED!\nSource:\n" .. tostring(test_src) .. "\nExpected:\n" .. tostring(expected) .. "\n\nGot:\n" .. tostring(got))
        end
      end
    end,
    initialize_core = function(self)
      local as_lua_code
      as_lua_code = function(self, str, vars)
        local _exp_0 = str.type
        if "String" == _exp_0 then
          return self:tree_to_value(str, vars)
        elseif "Longstring" == _exp_0 then
          return self:tree_to_value(str, vars)
        else
          return self:tree_to_lua(str)
        end
      end
      self:defmacro([[lua block %lua_code]], function(self, vars, kind)
        if kind == "Expression" then
          error("Expected to be in statement.")
        end
        local lua_code = vars.lua_code.value
        local _exp_0 = lua_code.type
        if "List" == _exp_0 then
          return table.concat((function()
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = lua_code.value
            for _index_0 = 1, #_list_0 do
              local i = _list_0[_index_0]
              _accum_0[_len_0] = as_lua_code(self, i.value, vars)
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)()), true
        else
          return as_lua_code(self, lua_code, vars), true
        end
      end)
      self:defmacro([[lua expr %lua_code]], function(self, vars, kind)
        local lua_code = vars.lua_code.value
        local _exp_0 = lua_code.type
        if "List" == _exp_0 then
          return table.concat((function()
            local _accum_0 = { }
            local _len_0 = 1
            local _list_0 = lua_code.value
            for _index_0 = 1, #_list_0 do
              local i = _list_0[_index_0]
              _accum_0[_len_0] = as_lua_code(self, i.value, vars)
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)())
        else
          return as_lua_code(self, lua_code, vars)
        end
      end)
      self:def("rule %spec %body", function(self, vars)
        return self:def(vars.spec, vars.body)
      end)
      self:defmacro([[macro %spec %body]], function(self, vars, kind)
        if kind == "Expression" then
          error("Macro definitions cannot be used as expressions.")
        end
        self:defmacro(self:tree_to_value(vars.spec, vars), self:tree_to_value(vars.body, vars))
        return "", true
      end)
      self:defmacro([[macro block %spec %body]], function(self, vars, kind)
        if kind == "Expression" then
          error("Macro definitions cannot be used as expressions.")
        end
        local invocation = self:tree_to_value(vars.spec, vars)
        local fn = self:tree_to_value(vars.body, vars)
        self:defmacro(invocation, (function(self, vars, kind)
          if kind == "Expression" then
            error("Macro: " .. tostring(invocation) .. " was defined to be a block, not an expression.")
          end
          return fn(self, vars, kind), true
        end))
        return "", true
      end)
      return self:def("run file %filename", function(self, vars)
        local file = io.open(vars.filename)
        return self:run(file:read('*a'))
      end)
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, parent)
      self.defs = setmetatable({ }, {
        __index = parent and parent.defs
      })
      self.callstack = { }
      self.debug = false
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
  self.comma_separated_items = function(self, open, items, close)
    return utils.accumulate("\n", function()
      local buffer = open
      local so_far = 0
      for i, item in ipairs(items) do
        if i < #items then
          item = item .. ", "
        end
        if so_far + #item >= 80 and #buffer > 0 then
          coroutine.yield(buffer)
          so_far = so_far - #buffer
          buffer = item
        else
          so_far = so_far + #item
          buffer = buffer .. item
        end
      end
      buffer = buffer .. close
      return coroutine.yield(buffer)
    end)
  end
  NomsuCompiler = _class_0
end
if arg and arg[1] then
  local c = NomsuCompiler()
  local input = io.open(arg[1]):read("*a")
  local _print = print
  local _io_write = io.write
  if arg[2] == "-" then
    local nop
    nop = function() end
    print, io.write = nop, nop
  end
  local code = c:run(input)
  if arg[2] then
    local output
    if arg[2] == "-" then
      print, io.write = _print, _io_write
      output = io.output()
    else
      output = io.open(arg[2], 'w')
    end
    output:write([[    local utils = require('utils')
    local load = function()
    ]])
    output:write(code)
    output:write([[
    end
    local NomsuCompiler = require('nomsu')
    local c = NomsuCompiler()
    load()(c, {})
    ]])
  end
end
return NomsuCompiler
