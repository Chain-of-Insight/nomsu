-- This file contains a set of compile actions needed for bootstrapping
{:match, :sub, :gsub, :format, :byte, :find} = string
{:LuaCode, :Source} = require "code_obj"
SyntaxTree = require "syntax_tree"
Files = require "files"

{:fail_at} = require('nomsu_compiler')

MAX_LINE = 80 -- For beautification purposes, try not to make lines much longer than this value
compile_actions = {
    [""]: (fn, ...)=>
        lua = LuaCode!
        fn_lua = @compile(fn)
        lua\add fn_lua
        unless fn_lua\text!\match("^%(.*%)$") or fn_lua\text!\match("^[_a-zA-Z][_a-zA-Z0-9.]*$")
            lua\parenthesize!
        lua\add "("
        for i=1,select('#',...)
            lua\add(", ") if i > 1
            lua\add @compile((select(i, ...)))
        lua\add ")"
        return lua

    ["Lua"]: (code)=>
        if not code
            return LuaCode("LuaCode()")
        if code.type != "Text"
            return LuaCode("LuaCode:from(", tostring(code.source)\as_lua!, ", ", @compile(code), ")")

        operate_on_text = (text)->
            lua = LuaCode\from(text.source, "LuaCode:from(", tostring(text.source)\as_lua!)
            for bit in *text
                local bit_lua
                if type(bit) == "string"
                    bit_lua = bit\as_lua!
                elseif bit.type == "Text"
                    bit_lua = operate_on_text(bit)
                elseif bit.type == "Block"
                    bit_lua = LuaCode\from bit.source, "(function()",
                        "\n    local _lua = LuaCode:from(", tostring(bit.source)\as_lua!, ")",
                        "\n    local function add(...) _lua:add(...) end",
                        "\n    local function join_with(glue)",
                        "\n        local old_bits = _lua.bits",
                        "\n        _lua = LuaCode:from(_lua.source)",
                        "\n        _lua:concat_add(old_bits, glue)",
                        "\n    end",
                        "\n    ", @compile(bit),
                        "\n    return _lua",
                        "\nend)()"
                else
                    bit_lua = @compile(bit)

                bit_leading_len = #(bit_lua\match("^[^\n]*"))
                lua\add(lua\trailing_line_len! + bit_leading_len > MAX_LINE and ",\n    " or ", ")
                lua\add(bit_lua)
            lua\add ")"
            return lua

        return operate_on_text code

    ["lua >"]: (code)=>
        if code.type != "Text"
            return code
        operate_on_text = (text)->
            lua = LuaCode\from(text.source)
            for bit in *text
                if type(bit) == "string"
                    lua\add bit
                elseif bit.type == "Text"
                    lua\add(operate_on_text(bit))
                else
                    lua\add @compile(bit)
            return lua
        return operate_on_text code

    ["= lua"]: (code)=>
        @compile(SyntaxTree{type:"Action", "lua", ">", code})

    ["1 as lua"]: (code)=>
        LuaCode("_ENV:compile(", @compile(code), ")")

    ["use"]: (path)=>
        LuaCode("_ENV:use(#{@compile(path)})")

    ["export"]: (path)=>
        LuaCode("_ENV:export(#{@compile(path)})")

    ["run"]: (path)=>
        LuaCode("_ENV:run(#{@compile(path)})")

    ["test"]: (body)=>
        unless body.type == 'Block'
            fail_at(body, "Compile error: This should be a Block")
        test_nomsu = body\get_source_code!\match(":[ ]*(.*)")
        if indent = test_nomsu\match("\n([ ]*)")
            test_nomsu = test_nomsu\gsub("\n"..indent, "\n")
        test_text = @compile(SyntaxTree{type:"Text", source:body.source, test_nomsu})
        return LuaCode "TESTS[#{tostring(body.source)\as_lua!}] = ", test_text

    ["is jit"]: (code)=> LuaCode("jit")
    ["Lua version"]: (code)=> LuaCode("_VERSION")
    ["nomsu environment"]: ()=> LuaCode("_ENV")
    ["nomsu environment name"]: ()=> LuaCode('"_ENV"')
    ["this file was run directly"]: => LuaCode('WAS_RUN_DIRECTLY')
    ["the command line arguments"]: => LuaCode('COMMAND_LINE_ARGS')
}

return compile_actions
