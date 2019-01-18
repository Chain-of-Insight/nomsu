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
local Files = require("files")
local fail_at
fail_at = require('nomsu_compiler').fail_at
local MAX_LINE = 80
local compile_actions = {
  [""] = function(self, _t, fn, ...)
    local lua = LuaCode()
    local fn_lua = self:compile(fn)
    lua:add(fn_lua)
    if not (fn_lua:text():match("^%(.*%)$") or fn_lua:text():match("^[_a-zA-Z][_a-zA-Z0-9.]*$")) then
      lua:parenthesize()
    end
    lua:add("(")
    for i = 1, select('#', ...) do
      if i > 1 then
        lua:add(", ")
      end
      lua:add(self:compile((select(i, ...))))
    end
    lua:add(")")
    return lua
  end,
  ["Lua"] = function(self, _t, code)
    if not code then
      return LuaCode("LuaCode()")
    end
    if code.type ~= "Text" then
      return LuaCode("LuaCode:from(", tostring(code.source):as_lua(), ", ", self:compile(code), ")")
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
          bit_lua = LuaCode:from(bit.source, "(function()", "\n    local _lua = LuaCode:from(", tostring(bit.source):as_lua(), ")", "\n    local function add(...) _lua:add(...) end", "\n    local function join_with(glue)", "\n        local old_bits = _lua.bits", "\n        _lua = LuaCode:from(_lua.source)", "\n        _lua:concat_add(old_bits, glue)", "\n    end", "\n    ", self:compile(bit), "\n    return _lua", "\nend)()")
        else
          bit_lua = self:compile(bit)
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
  ["lua >"] = function(self, _t, code)
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
          lua:add(self:compile(bit))
        end
      end
      return lua
    end
    return operate_on_text(code)
  end,
  ["= lua"] = function(self, _t, code)
    return self:compile(SyntaxTree({
      type = "Action",
      "lua",
      ">",
      code
    }))
  end,
  ["1 as lua"] = function(self, _t, code)
    return LuaCode("_ENV:compile(", self:compile(code), ")")
  end,
  ["use"] = function(self, _t, path)
    return LuaCode("_ENV:use(" .. tostring(self:compile(path)) .. ")")
  end,
  ["export"] = function(self, _t, path)
    return LuaCode("_ENV:export(" .. tostring(self:compile(path)) .. ")")
  end,
  ["run"] = function(self, _t, path)
    return LuaCode("_ENV:run(" .. tostring(self:compile(path)) .. ")")
  end,
  ["test"] = function(self, _t, body)
    if not (body.type == 'Block') then
      fail_at(body, "Compile error: This should be a Block")
    end
    local test_nomsu = body:get_source_code():match(":[ ]*(.*)")
    do
      local indent = test_nomsu:match("\n([ ]*)")
      if indent then
        test_nomsu = test_nomsu:gsub("\n" .. indent, "\n")
      end
    end
    local test_text = self:compile(SyntaxTree({
      type = "Text",
      source = body.source,
      test_nomsu
    }))
    return LuaCode("TESTS[" .. tostring(tostring(body.source):as_lua()) .. "] = ", test_text)
  end,
  ["is jit"] = function(self, _t, code)
    return LuaCode("jit")
  end,
  ["Lua version"] = function(self, _t, code)
    return LuaCode("_VERSION")
  end,
  ["nomsu environment"] = function(self, _t)
    return LuaCode("_ENV")
  end,
  ["nomsu environment name"] = function(self, _t)
    return LuaCode('"_ENV"')
  end,
  ["this file was run directly"] = function(self, _t)
    return LuaCode('WAS_RUN_DIRECTLY')
  end,
  ["the command line arguments"] = function(self, _t)
    return LuaCode('COMMAND_LINE_ARGS')
  end
}
return compile_actions
