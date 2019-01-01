local lpeg = require('lpeg')
local R, P, S
R, P, S = lpeg.R, lpeg.P, lpeg.S
local re = require('re')
local List, Dict, Text
do
  local _obj_0 = require('containers')
  List, Dict, Text = _obj_0.List, _obj_0.Dict, _obj_0.Text
end
local insert, remove, concat
do
  local _obj_0 = table
  insert, remove, concat = _obj_0.insert, _obj_0.remove, _obj_0.concat
end
local unpack = unpack or table.unpack
local match, sub, gsub, format, byte, find
do
  local _obj_0 = string
  match, sub, gsub, format, byte, find = _obj_0.match, _obj_0.sub, _obj_0.gsub, _obj_0.format, _obj_0.byte, _obj_0.find
end
local LuaCode, Source
do
  local _obj_0 = require("code_obj")
  LuaCode, Source = _obj_0.LuaCode, _obj_0.Source
end
local SyntaxTree = require("syntax_tree")
local Importer, import_to_1_from, _1_forked
do
  local _obj_0 = require('importer')
  Importer, import_to_1_from, _1_forked = _obj_0.Importer, _obj_0.import_to_1_from, _obj_0._1_forked
end
local Files = require("files")
table.map = function(t, fn)
  return setmetatable((function()
    local _accum_0 = { }
    local _len_0 = 1
    for _, v in ipairs(t) do
      _accum_0[_len_0] = fn(v)
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end)(), getmetatable(t))
end
local pretty_error = require("pretty_errors")
local compile_error
compile_error = function(source, err_msg, hint)
  if hint == nil then
    hint = nil
  end
  local file
  if SyntaxTree:is_instance(source) then
    file = source:get_source_file()
    source = source.source
  elseif type(source) == 'string' then
    source = Source:from_string(source)
  end
  if source and not file then
    file = Files.read(source.filename)
  end
  local err_str = pretty_error({
    title = "Compile error",
    error = err_msg,
    hint = hint,
    source = file,
    start = source.start,
    stop = source.stop,
    filename = source.filename
  })
  return error(err_str, 0)
end
local math_expression = re.compile([[ (([*/^+-] / [0-9]+) " ")* [*/^+-] !. ]])
local MAX_LINE = 80
local compile = setmetatable({
  action = Importer({
    [""] = function(compile, fn, ...)
      local lua = LuaCode()
      local fn_lua = compile(fn)
      lua:add(fn_lua)
      if not (fn_lua:text():match("^%(.*%)$") or fn_lua:text():match("^[_a-zA-Z][_a-zA-Z0-9.]*$")) then
        lua:parenthesize()
      end
      lua:add("(")
      for i = 1, select('#', ...) do
        if i > 1 then
          lua:add(", ")
        end
        lua:add(compile((select(i, ...))))
      end
      lua:add(")")
      return lua
    end,
    ["Lua"] = function(compile, code)
      if not code then
        return LuaCode("LuaCode()")
      end
      if code.type ~= "Text" then
        return LuaCode("LuaCode:from(", tostring(code.source):as_lua(), ", ", compile(code), ")")
      end
      local operate_on_text
      operate_on_text = function(text)
        local lua = LuaCode:from(text.source, "LuaCode:from(", tostring(text.source):as_lua())
        for _index_0 = 1, #text do
          local bit = text[_index_0]
          local bit_lua
          if type(bit) == "string" then
            bit_lua = bit:as_lua()
          elseif bit.type == "Text" then
            bit_lua = operate_on_text(bit)
          elseif bit.type == "Block" then
            bit_lua = LuaCode:from(bit.source, "(function()", "\n    local _lua = LuaCode:from(", tostring(bit.source):as_lua(), ")", "\n    local function add(...) _lua:add(...) end", "\n    local function join_with(glue)", "\n        local old_bits = _lua.bits", "\n        _lua = LuaCode:from(_lua.source)", "\n        _lua:concat_add(old_bits, glue)", "\n    end", "\n    ", compile(bit), "\n    return _lua", "\nend)()")
          else
            bit_lua = compile(bit)
          end
          local bit_leading_len = #(bit_lua:match("^[^\n]*"))
          lua:add(lua:trailing_line_len() + bit_leading_len > MAX_LINE and ",\n    " or ", ")
          lua:add(bit_lua)
        end
        lua:add(")")
        return lua
      end
      return operate_on_text(code)
    end,
    ["lua >"] = function(compile, code)
      if code.type ~= "Text" then
        return code
      end
      local operate_on_text
      operate_on_text = function(text)
        local lua = LuaCode:from(text.source)
        for _index_0 = 1, #text do
          local bit = text[_index_0]
          if type(bit) == "string" then
            lua:add(bit)
          elseif bit.type == "Text" then
            lua:add(operate_on_text(bit))
          else
            lua:add(compile(bit))
          end
        end
        return lua
      end
      return operate_on_text(code)
    end,
    ["= lua"] = function(compile, code)
      return compile.action["lua >"](compile, code)
    end,
    ["use"] = function(compile, path)
      return LuaCode("run_file_1_in(" .. tostring(compile(path)) .. ", _ENV, OPTIMIZATION)")
    end,
    ["use 1 with prefix"] = function(compile, path, prefix)
      return LuaCode("run_file_1_in(" .. tostring(compile(path)) .. ", _ENV, OPTIMIZATION, ", compile(prefix), ")")
    end,
    ["test"] = function(compile, body)
      if not (body.type == 'Block') then
        compile_error(body, "This should be a Block")
      end
      local test_nomsu = body:get_source_code():match(":[ ]*(.*)")
      do
        local indent = test_nomsu:match("\n([ ]*)")
        if indent then
          test_nomsu = test_nomsu:gsub("\n" .. indent, "\n")
        end
      end
      local test_text = compile(SyntaxTree({
        type = "Text",
        source = body.source,
        test_nomsu
      }))
      return LuaCode("TESTS[" .. tostring(tostring(body.source):as_lua()) .. "] = ", test_text)
    end,
    ["is jit"] = function(compile, code)
      return LuaCode("jit")
    end,
    ["Lua version"] = function(compile, code)
      return LuaCode("_VERSION")
    end,
    ["nomsu environment"] = function(compile)
      return LuaCode("_ENV")
    end
  })
}, {
  __import = import_to_1_from,
  __call = function(compile, tree)
    local _exp_0 = tree.type
    if "Action" == _exp_0 then
      local stub = tree.stub
      local compile_action = compile.action[stub]
      if not compile_action and math_expression:match(stub) then
        local lua = LuaCode:from(tree.source)
        for i, tok in ipairs(tree) do
          if type(tok) == 'string' then
            lua:add(tok)
          else
            local tok_lua = compile(tok)
            if tok.type == "Action" then
              tok_lua:parenthesize()
            end
            lua:add(tok_lua)
          end
          if i < #tree then
            lua:add(" ")
          end
        end
        return lua
      end
      if compile_action then
        local args
        do
          local _accum_0 = { }
          local _len_0 = 1
          for _index_0 = 1, #tree do
            local arg = tree[_index_0]
            if type(arg) ~= "string" then
              _accum_0[_len_0] = arg
              _len_0 = _len_0 + 1
            end
          end
          args = _accum_0
        end
        local ret = compile_action(compile, unpack(args))
        if ret == nil then
          local info = debug.getinfo(compile_action, "S")
          local filename = Source:from_string(info.source).filename
          compile_error(tree, "The compile-time action here (" .. tostring(stub) .. ") failed to return any value.", "Look at the implementation of (" .. tostring(stub) .. ") in " .. tostring(filename) .. ":" .. tostring(info.linedefined) .. " and make sure it's returning something.")
        end
        if not (SyntaxTree:is_instance(ret)) then
          ret.source = ret.source or tree.source
          return ret
        end
        if ret ~= tree then
          return compile(ret)
        end
      end
      local lua = LuaCode:from(tree.source)
      lua:add((stub):as_lua_id(), "(")
      for argnum, arg in ipairs(tree:get_args()) do
        local arg_lua = compile(arg)
        if arg.type == "Block" then
          arg_lua = LuaCode:from(arg.source, "(function()\n    ", arg_lua, "\nend)()")
        end
        if lua:trailing_line_len() + #arg_lua:text() > MAX_LINE then
          lua:add(argnum > 1 and ",\n    " or "\n    ")
        elseif argnum > 1 then
          lua:add(", ")
        end
        lua:add(arg_lua)
      end
      lua:add(")")
      return lua
    elseif "MethodCall" == _exp_0 then
      local lua = LuaCode:from(tree.source)
      local target_lua = compile(tree[1])
      local target_text = target_lua:text()
      if not (target_text:match("^%(.*%)$") or target_text:match("^[_a-zA-Z][_a-zA-Z0-9.]*$") or tree[1].type == "IndexChain") then
        target_lua:parenthesize()
      end
      for i = 2, #tree do
        if i > 2 then
          lua:add("\n")
        end
        lua:add(target_lua, ":")
        lua:add((tree[i].stub):as_lua_id(), "(")
        for argnum, arg in ipairs(tree[i]:get_args()) do
          local arg_lua = compile(arg)
          if arg.type == "Block" then
            arg_lua = LuaCode:from(arg.source, "(function()\n    ", arg_lua, "\nend)()")
          end
          if lua:trailing_line_len() + #arg_lua:text() > MAX_LINE then
            lua:add(argnum > 1 and ",\n    " or "\n    ")
          elseif argnum > 1 then
            lua:add(", ")
          end
          lua:add(arg_lua)
        end
        lua:add(")")
      end
      return lua
    elseif "EscapedNomsu" == _exp_0 then
      local lua = LuaCode:from(tree.source, "SyntaxTree{")
      local needs_comma, i = false, 1
      local as_lua
      as_lua = function(x)
        if type(x) == 'number' then
          return tostring(x)
        elseif SyntaxTree:is_instance(x) then
          return compile(x)
        elseif Source:is_instance(x) then
          return tostring(x):as_lua()
        else
          return x:as_lua()
        end
      end
      for k, v in pairs((SyntaxTree:is_instance(tree[1]) and tree[1].type == "EscapedNomsu" and tree) or tree[1]) do
        local entry_lua = LuaCode()
        if k == i then
          i = i + 1
        elseif type(k) == 'string' and match(k, "[_a-zA-Z][_a-zA-Z0-9]*") then
          entry_lua:add(k, "= ")
        else
          entry_lua:add("[", as_lua(k), "]= ")
        end
        entry_lua:add(as_lua(v))
        if needs_comma then
          lua:add(",")
        end
        if lua:trailing_line_len() + #(entry_lua:text():match("^[\n]*")) > MAX_LINE then
          lua:add("\n    ")
        elseif needs_comma then
          lua:add(" ")
        end
        lua:add(entry_lua)
        needs_comma = true
      end
      lua:add("}")
      return lua
    elseif "Block" == _exp_0 then
      local lua = LuaCode:from(tree.source)
      for i, line in ipairs(tree) do
        if i > 1 then
          lua:add("\n")
        end
        lua:add(compile(line))
      end
      return lua
    elseif "Text" == _exp_0 then
      local lua = LuaCode:from(tree.source)
      local added = 0
      local string_buffer = ""
      local add_bit
      add_bit = function(bit)
        if added > 0 then
          if lua:trailing_line_len() + #bit > MAX_LINE then
            lua:add("\n  ")
          end
          lua:add("..")
        end
        lua:add(bit)
        added = added + 1
      end
      for i, bit in ipairs(tree) do
        local _continue_0 = false
        repeat
          if type(bit) == "string" then
            string_buffer = string_buffer .. bit
            _continue_0 = true
            break
          end
          if string_buffer ~= "" then
            for i = 1, #string_buffer, MAX_LINE do
              add_bit(string_buffer:sub(i, i + MAX_LINE - 1):as_lua())
            end
            string_buffer = ""
          end
          local bit_lua = compile(bit)
          if bit.type == "Block" and #bit == 1 then
            bit = bit[1]
          end
          if bit.type == "Block" then
            bit_lua = LuaCode:from(bit.source, "List(function(add)", "\n    ", bit_lua, "\nend):joined()")
          elseif bit.type ~= "Text" and bit.type ~= "Number" then
            bit_lua = LuaCode:from(bit.source, "tostring(", bit_lua, ")")
          end
          add_bit(bit_lua)
          _continue_0 = true
        until true
        if not _continue_0 then
          break
        end
      end
      if string_buffer ~= "" then
        for i = 1, #string_buffer, MAX_LINE do
          add_bit(string_buffer:sub(i, i + MAX_LINE - 1):as_lua())
        end
        string_buffer = ""
      end
      if added == 0 then
        add_bit('""')
      end
      if added > 1 then
        lua:parenthesize()
      end
      return lua
    elseif "List" == _exp_0 or "Dict" == _exp_0 then
      if #tree == 0 then
        return LuaCode:from(tree.source, tree.type, "{}")
      end
      local lua = LuaCode:from(tree.source)
      local chunks = 0
      local i = 1
      while tree[i] do
        if tree[i].type == 'Block' then
          if chunks > 0 then
            lua:add(" + ")
          end
          lua:add(tree.type, "(function(", (tree.type == 'List' and "add" or ("add, " .. ("add 1 ="):as_lua_id())), ")")
          lua:add("\n    ", compile(tree[i]), "\nend)")
          chunks = chunks + 1
          i = i + 1
        else
          if chunks > 0 then
            lua:add(" + ")
          end
          local sep = ''
          local items_lua = LuaCode:from(tree[i].source)
          while tree[i] do
            if tree[i].type == "Block" then
              break
            end
            local item_lua = compile(tree[i])
            if item_lua:text():match("^%.[a-zA-Z_]") then
              item_lua = item_lua:text():sub(2)
            end
            if tree.type == 'Dict' and tree[i].type == 'Index' then
              item_lua = LuaCode:from(tree[i].source, item_lua, "=true")
            end
            items_lua:add(sep, item_lua)
            if tree[i].type == "Comment" then
              items_lua:add("\n")
              sep = ''
            elseif items_lua:trailing_line_len() > MAX_LINE then
              sep = ',\n    '
            else
              sep = ', '
            end
            i = i + 1
          end
          if items_lua:is_multiline() then
            lua:add(LuaCode:from(items_lua.source, tree.type, "{\n    ", items_lua, "\n}"))
          else
            lua:add(LuaCode:from(items_lua.source, tree.type, "{", items_lua, "}"))
          end
          chunks = chunks + 1
        end
      end
      return lua
    elseif "Index" == _exp_0 then
      local key_lua = compile(tree[1])
      local key_str = match(key_lua:text(), '^"([a-zA-Z_][a-zA-Z0-9_]*)"$')
      if key_str and key_str:is_lua_id() then
        return LuaCode:from(tree.source, ".", key_str)
      elseif sub(key_lua:text(), 1, 1) == "[" then
        return LuaCode:from(tree.source, "[ ", key_lua, "]")
      else
        return LuaCode:from(tree.source, "[", key_lua, "]")
      end
    elseif "DictEntry" == _exp_0 then
      local key = tree[1]
      if key.type ~= "Index" then
        key = SyntaxTree({
          type = "Index",
          source = key.source,
          key
        })
      end
      return LuaCode:from(tree.source, compile(key), "=", (tree[2] and compile(tree[2]) or "true"))
    elseif "IndexChain" == _exp_0 then
      local lua = compile(tree[1])
      if lua:text():match("['\"}]$") or lua:text():match("]=*]$") then
        lua:parenthesize()
      end
      for i = 2, #tree do
        local key = tree[i]
        if key.type ~= "Index" then
          key = SyntaxTree({
            type = "Index",
            source = key.source,
            key
          })
        end
        lua:add(compile(key))
      end
      return lua
    elseif "Number" == _exp_0 then
      return LuaCode:from(tree.source, tostring(tree[1]))
    elseif "Var" == _exp_0 then
      if type(tree[1]) == 'string' then
        return LuaCode:from(tree.source, (concat(tree, " ")):as_lua_id())
      else
        assert(tree[1].type == 'Action')
        return LuaCode:from(tree.source, tree[1]:get_stub():as_lua_id())
      end
    elseif "FileChunks" == _exp_0 then
      return error("Can't convert FileChunks to a single block of lua, since each chunk's " .. "compilation depends on the earlier chunks")
    elseif "Comment" == _exp_0 then
      return LuaCode:from(tree.source, "-- ", (tree[1]:gsub('\n', '\n-- ')))
    elseif "Error" == _exp_0 then
      return error("Can't compile errors")
    else
      return error("Unknown type: " .. tostring(tree.type))
    end
  end
})
return {
  compile = compile,
  compile_error = compile_error
}
