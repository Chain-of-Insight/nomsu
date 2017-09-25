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

    inline_block <- ({ {| "(" inline_statements ")" |} }) -> Block
    eol_block <- ({ {| ":" %ws? noeol_statements eol |} }) -> Block
    indented_block <- ({ {| (":" / "(..)") indent
                statements
            (dedent / (({.+} ("" -> "Error while parsing block")) => error))
        |} }) -> Block

    inline_nomsu <- ({ ("\" inline_block ) }) -> Nomsu
    eol_nomsu <- ({ ("\" eol_block ) }) -> Nomsu
    indented_nomsu <- ({ ("\" {indented_block} ) }) -> Nomsu

    inline_expression <- number / variable / inline_string / inline_list / inline_block / inline_nomsu
    noeol_expression <- indented_string / indented_block / indented_nomsu / indented_list / inline_expression
    expression <- eol_block / eol_nomsu / noeol_expression

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
        ({~ (("\\" -> "\") / ('\"' -> '"') / (!string_interpolation [^%nl"]))+ ~}
        / string_interpolation)* |} '"' }) -> String
    indented_string <- ({ '".."' indent {|
            indented_string_line (nodent {~ "" -> "
" ~} indented_string_line)*
          |} (dedent / (({.+} ("" -> "Error while parsing String")) => error))
        }) -> String
    indented_string_line <- "|" ({~ (("\\" -> "\") / (!string_interpolation [^%nl]))+ ~} / string_interpolation)*
    string_interpolation <- "\" (inline_block / indented_block / dotdot)

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

    block_comment <- "#.." [^%nl]* indent [^%nl]* (%nl ((%ws? (!. / &%nl)) / (!%dedented [^%nl]*)))* 
    line_comment  <- "#" [^%nl]*

    eol <- %ws? line_comment? (!. / &%nl)
    ignored_line <- (%nodented (block_comment / line_comment)) / (%ws? (!. / &%nl))
    indent <- eol (%nl ignored_line)* %nl %indented
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
    def = function(self, invocation, thunk, src)
      if type(invocation) ~= 'string' then
        self:error("Invocation should be string, not: " .. tostring(repr(invocation)))
      end
      if self.debug then
        self:writeln("Defining rule: " .. tostring(repr(invocation)))
      end
      local stub = invocation:gsub("'", " '"):gsub("%%%S+", "%%"):gsub("%s+", " ")
      local args
      do
        local _accum_0 = { }
        local _len_0 = 1
        for arg in invocation:gmatch("%%(%S[^%s']*)") do
          _accum_0[_len_0] = arg
          _len_0 = _len_0 + 1
        end
        args = _accum_0
      end
      for i = 1, #args - 1 do
        for j = i + 1, #args do
          if args[i] == args[j] then
            self:error("Duplicate argument in function def: " .. tostring(args[i]))
          end
        end
      end
      do
        local _with_0 = {
          thunk = thunk,
          invocation = invocation,
          args = args,
          src = src,
          is_macro = false
        }
        self.defs[invocation] = _with_0
        local _ = nil
        return _with_0
      end
    end,
    defmacro = function(self, invocation, thunk, src)
      do
        local _with_0 = self:def(invocation, thunk, src)
        _with_0.is_macro = true
        return _with_0
      end
    end,
    call = function(self, alias, ...)
      local def = self.defs[alias]
      if def == nil then
        self:error("Attempt to call undefined function: " .. tostring(alias))
      end
      if def.is_macro and self.callstack[#self.callstack] ~= "#macro" then
        self:error("Attempt to call macro at runtime: " .. tostring(alias) .. "\nThis can be caused by using a macro in a function that is defined before the macro.")
      end
      if not (self:check_permission(def)) then
        self:error("You do not have the authority to call: " .. tostring(alias))
      end
      local thunk, args
      thunk, args = def.thunk, def.args
      do
        local _tbl_0 = { }
        for i, name in ipairs(args) do
          _tbl_0[name] = select(i, ...)
        end
        args = _tbl_0
      end
      if self.debug then
        self:writeln("Calling " .. tostring(repr(alias)) .. " with args: " .. tostring(repr(args)))
      end
      insert(self.callstack, alias)
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
      local args, alias
      alias, args = self:get_alias(tree)
      insert(self.callstack, "#macro")
      local expr, statement = self:call(alias, unpack(args))
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
        self:writeln("PARSING:\n" .. tostring(str))
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
      assert(tree.type == "File")
      local buffer = { }
      local vars = { }
      local return_value = nil
      local _list_0 = tree.value
      for _index_0 = 1, #_list_0 do
        local statement = _list_0[_index_0]
        local ok, expr, statements = pcall(self.tree_to_lua, self, statement)
        if not ok then
          self:writeln("Error occurred in statement:\n" .. tostring(statement.src))
          self:error(expr)
        end
        local code_for_statement = ([[                return (function(nomsu, vars)
                    %s
                    return %s
                end)]]):format(statements or "", expr or "")
        if self.debug then
          self:writeln("RUNNING LUA:\n" .. tostring(code_for_statement))
        end
        local lua_thunk, err = load(code_for_statement)
        if not lua_thunk then
          error("Failed to compile generated code:\n" .. tostring(code_for_statement) .. "\n\n" .. tostring(err) .. "\n\nProduced by statement:\n" .. tostring(statement.src))
        end
        local run_statement = lua_thunk()
        local ret
        ok, ret = pcall(run_statement, self, vars)
        if expr then
          return_value = ret
        end
        if not ok then
          self:writeln("Error occurred in statement:\n" .. tostring(statement.src))
          self:error(return_value)
        end
        insert(buffer, tostring(statements or '') .. "\n" .. tostring(expr and "ret = " .. tostring(expr) or ''))
      end
      local lua_code = ([[            return function(nomsu, vars)
                local ret
                %s
                return ret
            end]]):format(concat(buffer, "\n"))
      return return_value, lua_code
    end,
    tree_to_value = function(self, tree, vars)
      local code = "\n        return (function(nomsu, vars)\nreturn " .. tostring(self:tree_to_lua(tree)) .. "\nend)"
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
        return error("Should not be converting File to lua through this function.")
      elseif "Nomsu" == _exp_0 then
        return repr(tree.value), nil
      elseif "Block" == _exp_0 then
        local lua_bits = { }
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local arg = _list_0[_index_0]
          local expr, statement = self:tree_to_lua(arg)
          if expr and not statement and #tree.value == 1 then
            return expr, nil
          end
          if statement then
            insert(lua_bits, statement)
          end
          if expr then
            insert(lua_bits, "ret = " .. tostring(expr))
          end
        end
        return ([[                    function(nomsu, vars)
                        local ret
                        %s
                        return ret
                    end]]):format(concat(lua_bits, "\n"))
      elseif "FunctionCall" == _exp_0 then
        local alias = self:get_alias(tree)
        if self.defs[alias] and self.defs[alias].is_macro then
          return self:run_macro(tree, "Expression")
        end
        local args = {
          repr(alias)
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
        return "(" .. tostring(concat(concat_parts, "..")) .. ")", nil
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
        return repr(tree.value)
      elseif "Var" == _exp_0 then
        return "vars[" .. tostring(repr(tree.value)) .. "]"
      else
        return self:error("Unknown/unimplemented thingy: " .. tostring(tree.type))
      end
    end,
    print_tree = function(self, tree, ind)
      if ind == nil then
        ind = ""
      end
      if type(tree) ~= 'table' or not tree.type then
        self:writeln(tostring(ind) .. tostring(repr(tree)))
        return 
      end
      self:writeln(tostring(ind) .. tostring(tree.type) .. ":")
      local _exp_0 = tree.type
      if "List" == _exp_0 or "File" == _exp_0 or "Block" == _exp_0 or "FunctionCall" == _exp_0 or "String" == _exp_0 then
        local _list_0 = tree.value
        for _index_0 = 1, #_list_0 do
          local v = _list_0[_index_0]
          self:print_tree(v, ind .. "    ")
        end
      else
        return self:print_tree(tree.value, ind .. "    ")
      end
    end,
    replaced_vars = function(self, tree, vars)
      if type(tree) ~= 'table' then
        return tree
      end
      local _exp_0 = tree.type
      if "Var" == _exp_0 then
        if vars[tree.value] then
          tree = vars[tree.value]
        end
      elseif "File" == _exp_0 or "Thunk" == _exp_0 or "Statement" == _exp_0 or "Block" == _exp_0 or "List" == _exp_0 or "FunctionCall" == _exp_0 or "String" == _exp_0 then
        local new_value = self:replaced_vars(tree.value)
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
          new_values[k] = self:replaced_vars(v)
          any_different = any_different or (new_values[k] ~= tree[k])
        end
        if any_different then
          tree = new_values
        end
      end
      return tree
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
      self:writeln(...)
      self:writeln("Callstack:")
      for i = #self.callstack, 1, -1 do
        self:writeln("    " .. tostring(self.callstack[i]))
      end
      self:writeln("    <top level>")
      self.callstack = { }
      return error()
    end,
    initialize_core = function(self)
      self:defmacro("lua code %statements with value %value", function(self, vars)
        local inner_vars = setmetatable({ }, {
          __index = function(_, key)
            return "vars[" .. tostring(repr(key)) .. "]"
          end
        })
        local statements = self:tree_to_value(vars.statements, inner_vars)
        local value = self:tree_to_value(vars.value, inner_vars)
        return value, statements
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
      self.utils = utils
      self.repr = function(self, ...)
        return repr(...)
      end
      self.loaded_files = { }
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
  c.debug = true
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
    output:write(([[    local NomsuCompiler = require('nomsu')
    local c = NomsuCompiler()
    local run = %s
    return run(c, {})
    ]]):format(code))
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
