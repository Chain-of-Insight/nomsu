local NomsuCode
NomsuCode = require("code_obj").NomsuCode
local find, sub, match
do
  local _obj_0 = string
  find, sub, match = _obj_0.find, _obj_0.sub, _obj_0.match
end
local R, P, S
do
  local _obj_0 = require('lpeg')
  R, P, S = _obj_0.R, _obj_0.P, _obj_0.S
end
local re = require('re')
local MAX_LINE = 80
local GOLDEN_RATIO = ((math.sqrt(5) - 1) / 2)
local utf8_char_patt = (R("\194\223") * R("\128\191") + R("\224\239") * R("\128\191") * R("\128\191") + R("\240\244") * R("\128\191") * R("\128\191") * R("\128\191"))
local operator_patt = S("'`~!@%#^&*+=|<>?/-") ^ 1 * -1
local identifier_patt = (R("az", "AZ", "09") + P("_") + utf8_char_patt) ^ 1 * -1
local is_operator
is_operator = function(s)
  return not not operator_patt:match(s)
end
local is_identifier
is_identifier = function(s)
  return not not identifier_patt:match(s)
end
local inline_escaper = re.compile("{~ (%utf8_char / ('\"' -> '\\\"') / ('\n' -> '\\n') / ('\t' -> '\\t') / ('\b' -> '\\b') / ('\a' -> '\\a') / ('\v' -> '\\v') / ('\f' -> '\\f') / ('\r' -> '\\r') / ('\\' -> '\\\\') / ([^ -~] -> escape) / .)* ~}", {
  utf8_char = utf8_char_patt,
  escape = (function(self)
    return ("\\%03d"):format(self:byte())
  end)
})
local inline_escape
inline_escape = function(s)
  return inline_escaper:match(s)
end
local escaper = re.compile("{~ (%utf8_char / ('\\' -> '\\\\') / [\n\r\t -~] / (. -> escape))* ~}", {
  utf8_char = utf8_char_patt,
  escape = (function(self)
    return ("\\%03d"):format(self:byte())
  end)
})
local escape
escape = function(s)
  return escaper:match(s)
end
local tree_to_inline_nomsu
tree_to_inline_nomsu = function(tree)
  local _exp_0 = tree.type
  if "Action" == _exp_0 then
    local nomsu = NomsuCode:from(tree.source)
    local num_args, num_words = 0, 0
    for i, bit in ipairs(tree) do
      if type(bit) == "string" then
        num_words = num_words + 1
        local clump_words
        if type(tree[i - 1]) == 'string' then
          clump_words = is_operator(bit) ~= is_operator(tree[i - 1])
        else
          clump_words = bit == "'"
        end
        if i > 1 and not clump_words then
          nomsu:add(" ")
        end
        nomsu:add(bit)
      else
        num_args = num_args + 1
        local arg_nomsu = tree_to_inline_nomsu(bit)
        if bit.type == "Block" then
          if i > 1 and i < #tree then
            nomsu:add(" ")
          end
          if not (i == #tree) then
            arg_nomsu:parenthesize()
          end
        else
          if i > 1 and tree[i - 1] ~= "#" then
            nomsu:add(" ")
          end
          if bit.type == "Action" or bit.type == "MethodCall" then
            arg_nomsu:parenthesize()
          end
        end
        nomsu:add(arg_nomsu)
      end
    end
    if num_args == 1 and num_words == 0 then
      nomsu:add("()")
    end
    return nomsu
  elseif "MethodCall" == _exp_0 then
    local target_nomsu = tree_to_inline_nomsu(tree[1])
    if tree[1].type == "Block" then
      target_nomsu:parenthesize()
    end
    local nomsu = NomsuCode:from(tree.source, target_nomsu, ", ")
    for i = 2, #tree do
      if i > 2 then
        nomsu:add("; ")
      end
      nomsu:add(tree_to_inline_nomsu(tree[i]))
    end
    return nomsu
  elseif "EscapedNomsu" == _exp_0 then
    local inner_nomsu = tree_to_inline_nomsu(tree[1])
    if not (tree[1].type == "List" or tree[1].type == "Dict" or tree[1].type == "Var") then
      inner_nomsu:parenthesize()
    end
    return NomsuCode:from(tree.source, "\\", inner_nomsu)
  elseif "Block" == _exp_0 then
    local nomsu = NomsuCode:from(tree.source, ":")
    for i, line in ipairs(tree) do
      nomsu:add(i == 1 and " " or "; ")
      nomsu:add(tree_to_inline_nomsu(line))
    end
    if #tree > 1 then
      nomsu:parenthesize()
    end
    return nomsu
  elseif "Text" == _exp_0 then
    local add_text
    add_text = function(nomsu, tree)
      for i, bit in ipairs(tree) do
        if type(bit) == 'string' then
          local escaped = inline_escape(bit)
          nomsu:add(inline_escape(bit))
        elseif bit.type == "Text" then
          add_text(nomsu, bit)
        else
          local interp_nomsu = tree_to_inline_nomsu(bit)
          if bit.type ~= "Var" and bit.type ~= "List" and bit.type ~= "Dict" then
            interp_nomsu:parenthesize()
          elseif bit.type == "Var" and type(bit[1]) == 'string' and type(tree[i + 1]) == 'string' and not match(tree[i + 1], "^[ \n\t,.:;#(){}[%]]") then
            interp_nomsu:parenthesize()
          end
          nomsu:add("\\", interp_nomsu)
        end
      end
    end
    local nomsu = NomsuCode:from(tree.source)
    add_text(nomsu, tree)
    return NomsuCode:from(tree.source, '"', nomsu, '"')
  elseif "List" == _exp_0 or "Dict" == _exp_0 then
    local nomsu = NomsuCode:from(tree.source, (tree.type == "List" and "[" or "{"))
    for i, item in ipairs(tree) do
      if i > 1 then
        nomsu:add(", ")
      end
      local item_nomsu = tree_to_inline_nomsu(item, true)
      if item.type == "MethodCall" then
        item_nomsu:parenthesize()
      end
      nomsu:add(item_nomsu)
    end
    nomsu:add(tree.type == "List" and "]" or "}")
    return nomsu
  elseif "DictEntry" == _exp_0 then
    local key, value = tree[1], tree[2]
    local nomsu = NomsuCode:from(tree.source)
    if key.type ~= "Index" then
      key = {
        type = "Index",
        source = key.source,
        key
      }
    end
    nomsu:add(tree_to_inline_nomsu(key))
    if value then
      nomsu:add(" = ")
      local value_nomsu = tree_to_inline_nomsu(value)
      if value.type == "Block" or value.type == "Action" or value.type == "MethodCall" then
        value_nomsu:parenthesize()
      end
      nomsu:add(value_nomsu)
    end
    return nomsu
  elseif "Index" == _exp_0 then
    local key = tree[1]
    local nomsu = NomsuCode:from(key.source, ".")
    local key_nomsu
    if key.type == "Text" and #key == 1 and is_identifier(key[1]) then
      key_nomsu = key[1]
    else
      key_nomsu = tree_to_inline_nomsu(key)
    end
    local _exp_1 = key.type
    if "Block" == _exp_1 or "Action" == _exp_1 or "MethodCall" == _exp_1 or "IndexChain" == _exp_1 then
      key_nomsu:parenthesize()
    end
    return NomsuCode:from(key.source, ".", key_nomsu)
  elseif "IndexChain" == _exp_0 then
    local nomsu = NomsuCode:from(tree.source)
    local target = tree[1]
    local target_nomsu = tree_to_inline_nomsu(target)
    local _exp_1 = target.type
    if "Action" == _exp_1 or "MethodCall" == _exp_1 then
      target_nomsu:parenthesize()
    elseif "Number" == _exp_1 then
      if target_nomsu:text():match("%.") then
        target_nomsu:parenthesize()
      end
    end
    nomsu:add(target_nomsu)
    for i = 2, #tree do
      if tree[i].type ~= "Index" then
        tree[i] = {
          type = "Index",
          source = tree[i].source,
          tree[i]
        }
      end
      nomsu:add(tree_to_inline_nomsu(tree[i]))
    end
    return nomsu
  elseif "Number" == _exp_0 then
    local s
    if tree.hex and tree[1] < 0 then
      s = ("-0x%X"):format(-tree[1])
    elseif tree.hex then
      s = ("0x%X"):format(tree[1])
    else
      s = tostring(tree[1])
    end
    return NomsuCode:from(tree.source, s)
  elseif "Var" == _exp_0 then
    local varname = tree[1]
    if type(varname) == "string" then
      return NomsuCode:from(tree.source, "$", varname)
    else
      return NomsuCode:from(tree.source, "$(", tree_to_inline_nomsu(varname), ")")
    end
  elseif "FileChunks" == _exp_0 then
    return error("Can't inline a FileChunks")
  elseif "Comment" == _exp_0 then
    return NomsuCode:from(tree.source)
  elseif "Error" == _exp_0 then
    return error("Can't compile errors")
  else
    return error("Unknown type: " .. tostring(tree.type))
  end
end
local tree_to_nomsu
tree_to_nomsu = function(tree)
  local nomsu = NomsuCode:from(tree.source)
  local recurse
  recurse = function(t, argnum)
    if argnum == nil then
      argnum = nil
    end
    local space = MAX_LINE - nomsu:trailing_line_len()
    local try_inline = true
    for subtree in coroutine.wrap(function()
      return (t:map(coroutine.yield) and nil)
    end) do
      local _exp_0 = subtree.type
      if "Comment" == _exp_0 then
        try_inline = false
      elseif "Block" == _exp_0 then
        if #subtree > 1 then
          try_inline = false
        end
      elseif "Text" == _exp_0 then
        local indented = tree_to_nomsu(subtree)
        local indented_lines
        do
          local _accum_0 = { }
          local _len_0 = 1
          local _list_0 = indented:text():lines()
          for _index_0 = 1, #_list_0 do
            local line = _list_0[_index_0]
            if line:match("^    +([^ ].*)") then
              _accum_0[_len_0] = line
              _len_0 = _len_0 + 1
            end
          end
          indented_lines = _accum_0
        end
        for i = #indented_lines, 1, -1 do
          if indented_lines[i]:match("^ *\\;$") then
            table.remove(indented_lines, i)
          end
        end
        if #indented_lines > 1 or (#indented_lines == 1 and #indented_lines[1] > MAX_LINE + 8) then
          try_inline = false
        end
      end
    end
    local inline_nomsu
    if try_inline then
      inline_nomsu = tree_to_inline_nomsu(t)
      if (t.type == "Action" or t.type == "MethodCall") then
        inline_nomsu:parenthesize()
      end
      if #inline_nomsu:text() <= space or #inline_nomsu:text() <= 8 then
        if t.type ~= "Text" then
          return inline_nomsu
        end
      end
    end
    local indented = tree_to_nomsu(t)
    if t.type == "Action" or t.type == "MethodCall" then
      if indented:is_multiline() then
        if argnum == nil or argnum == 1 then
          return NomsuCode:from(t.source, "(\n    ", indented, "\n)")
        else
          return NomsuCode:from(t.source, "\n    ", indented)
        end
      elseif argnum and argnum > 1 then
        return NomsuCode:from(t.source, "\n    ", indented)
      else
        indented:parenthesize()
      end
    end
    local indented_lines
    do
      local _accum_0 = { }
      local _len_0 = 1
      local _list_0 = indented:text():lines()
      for _index_0 = 1, #_list_0 do
        local line = _list_0[_index_0]
        if line:match("^    +([^ ].*)") then
          _accum_0[_len_0] = line
          _len_0 = _len_0 + 1
        end
      end
      indented_lines = _accum_0
    end
    if t.type == "Text" then
      for i = #indented_lines, 1, -1 do
        if indented_lines[i]:match("^ *\\;$") then
          table.remove(indented_lines, i)
        end
      end
    end
    if inline_nomsu and (#inline_nomsu:text() < MAX_LINE or #inline_nomsu:text() <= space) and #indented_lines <= 1 then
      return inline_nomsu
    end
    return indented
  end
  local _exp_0 = tree.type
  if "FileChunks" == _exp_0 then
    if tree.shebang then
      nomsu:add(tree.shebang)
    end
    for chunk_no, chunk in ipairs(tree) do
      if chunk_no > 1 then
        nomsu:add("\n\n" .. tostring(("~"):rep(80)) .. "\n\n")
      end
      if chunk.type == "Block" then
        nomsu:add(NomsuCode:from(chunk.source, table.unpack(tree_to_nomsu(chunk).bits, 2)))
      else
        nomsu:add(tree_to_nomsu(chunk))
      end
    end
    if not (nomsu:match("\n$")) then
      nomsu:add('\n')
    end
    return nomsu
  elseif "Action" == _exp_0 then
    local next_space = ""
    local word_buffer = { }
    local num_args, num_words = 0, 0
    for i, bit in ipairs(tree) do
      local _continue_0 = false
      repeat
        if type(bit) == "string" then
          num_words = num_words + 1
          if #word_buffer > 0 and is_operator(bit) == is_operator(word_buffer[#word_buffer]) then
            table.insert(word_buffer, " ")
          end
          table.insert(word_buffer, bit)
          _continue_0 = true
          break
        end
        if #word_buffer > 0 then
          local words = table.concat(word_buffer)
          if next_space == " " then
            if nomsu:trailing_line_len() + #words > MAX_LINE and nomsu:trailing_line_len() > 8 then
              next_space = "\n.."
            elseif word_buffer[1] == "'" then
              next_space = ""
            end
          end
          nomsu:add(next_space, words)
          word_buffer = { }
          next_space = " "
        end
        num_args = num_args + 1
        local bit_nomsu = recurse(bit, i)
        if bit.type == "Block" then
          if not bit_nomsu:is_multiline() and (#bit_nomsu:text() > nomsu:trailing_line_len() * GOLDEN_RATIO and #bit_nomsu:text() > 8) or #bit_nomsu:text() + nomsu:trailing_line_len() > MAX_LINE then
            bit_nomsu = tree_to_nomsu(bit)
          end
        elseif (not bit_nomsu:is_multiline() and nomsu:trailing_line_len() + #bit_nomsu:text() > MAX_LINE and nomsu:trailing_line_len() > 8) then
          if next_space == " " and #bit_nomsu:text() < MAX_LINE then
            if i == #tree then
              bit_nomsu = tree_to_inline_nomsu(bit)
              next_space = "\n    "
            elseif bit.type == "List" or bit.type == "Dict" then
              bit_nomsu = tree_to_nomsu(bit)
            else
              next_space = "\n.."
            end
          elseif bit.type == 'Action' or bit.type == "MethodCall" then
            bit_nomsu = NomsuCode:from(bit.source, "\n    ", tree_to_nomsu(bit))
          else
            bit_nomsu = tree_to_nomsu(bit)
          end
        end
        if not (next_space == " " and bit_nomsu:text():match("^[:\n]")) then
          nomsu:add(next_space)
        end
        nomsu:add(bit_nomsu)
        next_space = (bit.type == "Block" or bit_nomsu:text():matches("\n    [^\n]*$")) and "\n.." or " "
        _continue_0 = true
      until true
      if not _continue_0 then
        break
      end
    end
    if #word_buffer > 0 then
      local words = table.concat(word_buffer)
      if next_space == " " then
        if nomsu:trailing_line_len() + #words > MAX_LINE and nomsu:trailing_line_len() > 8 then
          next_space = "\n.."
        elseif word_buffer[1] == "'" then
          next_space = ""
        end
      end
      nomsu:add(next_space, words)
      next_space = " "
    end
    if num_args == 1 and num_words == 0 then
      if next_space ~= " " then
        nomsu:add(next_space)
      end
      nomsu:add("()")
    end
    return nomsu
  elseif "MethodCall" == _exp_0 then
    local target_nomsu = recurse(tree[1])
    if tree[1].type == "Block" and not target_nomsu:is_multiline() then
      target_nomsu:parenthesize()
    end
    nomsu:add(target_nomsu, ", ")
    local inner_nomsu = NomsuCode()
    for i = 2, #tree do
      if i > 2 then
        inner_nomsu:add("\n")
      end
      inner_nomsu:add(tree_to_nomsu(tree[i]))
    end
    if #tree == 2 and nomsu:trailing_line_len() + #inner_nomsu:text():match("^[^\n]*") < MAX_LINE then
      nomsu:add(inner_nomsu)
    else
      nomsu:add("\n    ", inner_nomsu)
    end
    return nomsu
  elseif "EscapedNomsu" == _exp_0 then
    nomsu = recurse(tree[1])
    if tree[1].type == 'Block' and not nomsu:is_multiline() then
      nomsu:parenthesize()
    end
    return NomsuCode:from(tree.source, "\\", nomsu)
  elseif "Block" == _exp_0 then
    local prev_line, needs_space = nil, { }
    for i, line in ipairs(tree) do
      local line_nomsu = tree_to_nomsu(line)
      if i > 1 then
        nomsu:add("\n")
        if tree[i - 1].type ~= "Comment" then
          needs_space[i] = (line_nomsu:is_multiline() and prev_line:is_multiline())
          if tree[i].type == "Comment" or needs_space[i] or needs_space[i - 1] then
            nomsu:add("\n")
          end
        end
      end
      nomsu:add(line_nomsu)
      prev_line = line_nomsu
    end
    return NomsuCode:from(tree.source, ":\n    ", nomsu)
  elseif "Text" == _exp_0 then
    local max_line = MAX_LINE + 8
    local add_text
    add_text = function(tree)
      for i, bit in ipairs(tree) do
        if type(bit) == 'string' then
          bit = escape(bit)
          for j, line in ipairs(bit:lines()) do
            if j > 1 then
              if nomsu:text():match(" $") then
                nomsu:add("\\;")
              end
              nomsu:add("\n")
            elseif #line > 10 and nomsu:trailing_line_len() > max_line then
              nomsu:add("\\\n..")
            end
            while #line > 0 do
              local space = max_line - nomsu:trailing_line_len()
              local split = find(line, "[%p%s]", space)
              if not split or split > space + 10 then
                split = space + 10
              end
              if #line - split < 10 then
                split = #line
              end
              local bite
              bite, line = sub(line, 1, split), sub(line, split + 1, -1)
              nomsu:add(bite)
              if #line > 0 then
                nomsu:add("\\\n..")
              end
            end
          end
        elseif bit.type == "Text" then
          add_text(bit)
        else
          nomsu:add("\\")
          local interp_nomsu = recurse(bit)
          if interp_nomsu:is_multiline() then
            local curr_indent = nomsu:text():match("\n( *)[^\n]*$") or nomsu:text():match("^( *)")
            interp_nomsu = NomsuCode((interp_nomsu:text():gsub("\n", "\n" .. curr_indent)))
          else
            local space = max_line - nomsu:trailing_line_len()
            if bit.type == "Var" then
              local next_str = tree[i + 1]
              while type(next_str) == 'table' and next_str.type == 'Text' do
                next_str = next_str[1]
              end
              if type(next_str) == 'string' and not match(next_str, "^[ \n\t,.:;#(){}[%]]") then
                interp_nomsu:parenthesize()
              end
            elseif #interp_nomsu:text() > space then
              local interp_nomsu2
              if bit.type == "Action" or bit.type == "MethodCall" then
                interp_nomsu2 = NomsuCode:from(bit.source, "(\n    ", tree_to_nomsu(bit), "\n)")
              else
                interp_nomsu2 = tree_to_nomsu(bit)
              end
              if #interp_nomsu2:text():lines() > 3 or #interp_nomsu2:text() >= MAX_LINE * GOLDEN_RATIO then
                local curr_indent = nomsu:text():match("\n( *)[^\n]*$") or nomsu:text():match("^( *)")
                interp_nomsu2 = NomsuCode((interp_nomsu2:text():gsub("\n", "\n" .. curr_indent)))
                interp_nomsu = interp_nomsu2
              else
                nomsu:add("\n..\\")
                if bit.type == "EscapedNomsu" or bit.type == "Block" or bit.type == "IndexChain" then
                  interp_nomsu:parenthesize()
                end
              end
            elseif bit.type == "EscapedNomsu" or bit.type == "Block" or bit.type == "IndexChain" then
              interp_nomsu:parenthesize()
            end
          end
          nomsu:add(interp_nomsu)
          if interp_nomsu:is_multiline() and bit.type == "Block" then
            nomsu:add("\n..")
          end
        end
      end
    end
    add_text(tree)
    if nomsu:text():match(" $") then
      nomsu:add("\\;")
    end
    return NomsuCode:from(tree.source, '("\n    ', nomsu, '\n")')
  elseif "List" == _exp_0 or "Dict" == _exp_0 then
    if #tree == 0 then
      nomsu:add(tree.type == "List" and "[]" or "{}")
      return nomsu
    end
    local sep = ''
    for i, item in ipairs(tree) do
      local item_nomsu
      if item.type == 'MethodCall' then
        item_nomsu = recurse(item)
      elseif item.type == 'Comment' then
        item_nomsu = tree_to_nomsu(item)
        if i > 1 then
          sep = '\n'
        end
      elseif item.type == 'Block' and #item == 1 then
        item_nomsu = tree_to_nomsu(item[1])
        item_nomsu:prepend(": ")
        if i > 1 then
          sep = '\n'
        end
      else
        item_nomsu = tree_to_inline_nomsu(item)
        if nomsu:trailing_line_len() + #item_nomsu:text() > MAX_LINE then
          if i > 1 then
            sep = '\n'
          end
          item_nomsu = item.type == "Action" and tree_to_nomsu(item) or recurse(item)
        end
      end
      nomsu:add(sep)
      nomsu:add(item_nomsu)
      if item_nomsu:is_multiline() or item.type == 'Comment' or item.type == "Block" or nomsu:trailing_line_len() + #tostring(item_nomsu) >= MAX_LINE then
        sep = '\n'
      else
        sep = ', '
      end
    end
    if tree.type == "List" then
      return NomsuCode:from(tree.source, "[\n    ", nomsu, "\n]")
    else
      return NomsuCode:from(tree.source, "{\n    ", nomsu, "\n}")
    end
  elseif "DictEntry" == _exp_0 then
    local key, value = tree[1], tree[2]
    nomsu = NomsuCode:from(tree.source)
    if key.type ~= "Index" then
      key = {
        type = "Index",
        source = key.source,
        key
      }
    end
    nomsu:add(tree_to_nomsu(key))
    if value then
      local value_nomsu = recurse(value)
      nomsu:add(" = ", value_nomsu)
    end
    return nomsu
  elseif "Comment" == _exp_0 then
    nomsu:add("#", (tree[1]:gsub("\n", "\n    ")))
    return nomsu
  elseif "IndexChain" == _exp_0 or "Index" == _exp_0 or "Number" == _exp_0 or "Var" == _exp_0 or "Comment" == _exp_0 or "Error" == _exp_0 then
    return tree_to_inline_nomsu(tree)
  else
    return error("Unknown type: " .. tostring(tree.type))
  end
end
return {
  tree_to_nomsu = tree_to_nomsu,
  tree_to_inline_nomsu = tree_to_inline_nomsu
}
