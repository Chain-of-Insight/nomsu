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
local GOLDEN_RATIO = ((1 + math.sqrt(5)) / 2)
local utf8_char_patt = (R("\194\223") * R("\128\191") + R("\224\239") * R("\128\191") * R("\128\191") + R("\240\244") * R("\128\191") * R("\128\191") * R("\128\191"))
local operator_patt = S("'`~!@$^&*+=|<>?/-") ^ 1 * -1
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
    if tree.target then
      local inline_target = tree_to_inline_nomsu(tree.target)
      if tree.target.type == "Action" then
        inline_target:parenthesize()
      end
      nomsu:append(inline_target, "::")
    end
    for i, bit in ipairs(tree) do
      if type(bit) == "string" then
        local clump_words
        if type(tree[i - 1]) == 'string' then
          clump_words = is_operator(bit) ~= is_operator(tree[i - 1])
        else
          clump_words = bit == "'"
        end
        if i > 1 and not clump_words then
          nomsu:append(" ")
        end
        nomsu:append(bit)
      else
        local arg_nomsu = tree_to_inline_nomsu(bit)
        if bit.type == "Block" then
          if i > 1 and i < #tree then
            nomsu:append(" ")
          end
          if not (i == #tree) then
            arg_nomsu:parenthesize()
          end
        else
          if i > 1 then
            nomsu:append(" ")
          end
          if bit.type == "Action" then
            arg_nomsu:parenthesize()
          end
        end
        nomsu:append(arg_nomsu)
      end
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
      nomsu:append(i == 1 and " " or "; ")
      nomsu:append(tree_to_inline_nomsu(line))
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
          nomsu:append(inline_escape(bit))
        elseif bit.type == "Text" then
          add_text(nomsu, bit)
        else
          local interp_nomsu = tree_to_inline_nomsu(bit)
          if bit.type ~= "Var" and bit.type ~= "List" and bit.type ~= "Dict" then
            interp_nomsu:parenthesize()
          elseif bit.type == "Var" and type(tree[i + 1]) == 'string' and not match(tree[i + 1], "^[ \n\t,.:;#(){}[%]]") then
            interp_nomsu:parenthesize()
          end
          nomsu:append("\\", interp_nomsu)
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
        nomsu:append(", ")
      end
      nomsu:append(tree_to_inline_nomsu(item))
    end
    nomsu:append(tree.type == "List" and "]" or "}")
    return nomsu
  elseif "DictEntry" == _exp_0 then
    local key, value = tree[1], tree[2]
    local nomsu
    if key.type == "Text" and #key == 1 and is_identifier(key[1]) then
      nomsu = NomsuCode:from(key.source, key[1])
    else
      nomsu = tree_to_inline_nomsu(key)
    end
    if key.type == "Action" or key.type == "Block" then
      nomsu:parenthesize()
    end
    assert(value.type ~= "Block", "Didn't expect to find a Block as a value in a dict")
    nomsu:append(": ")
    if value then
      local value_nomsu = tree_to_inline_nomsu(value)
      if value.type == "Block" then
        value_nomsu:parenthesize()
      end
      nomsu:append(value_nomsu)
    end
    return nomsu
  elseif "IndexChain" == _exp_0 then
    local nomsu = NomsuCode:from(tree.source)
    for i, bit in ipairs(tree) do
      if i > 1 then
        nomsu:append(".")
      end
      local bit_nomsu
      if i > 1 and bit.type == "Text" and #bit == 1 and type(bit[1]) == 'string' and is_identifier(bit[1]) then
        bit_nomsu = bit[1]
      else
        bit_nomsu = tree_to_inline_nomsu(bit)
      end
      assert(bit.type ~= "Block")
      if bit.type == "Action" or bit.type == "IndexChain" or (bit.type == "Number" and i < #tree) then
        bit_nomsu:parenthesize()
      end
      nomsu:append(bit_nomsu)
    end
    return nomsu
  elseif "Number" == _exp_0 then
    return NomsuCode:from(tree.source, tostring(tree[1]))
  elseif "Var" == _exp_0 then
    return NomsuCode:from(tree.source, "%", tree[1])
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
  recurse = function(t)
    local space = MAX_LINE - nomsu:trailing_line_len()
    local try_inline = true
    for subtree in coroutine.wrap(function()
      return (t:map(coroutine.yield) and nil)
    end) do
      if subtree.type == "Block" then
        if #subtree > 1 then
          try_inline = false
        end
      end
    end
    local inline_nomsu
    if try_inline then
      inline_nomsu = tree_to_inline_nomsu(t)
      if #inline_nomsu:text() <= space or #inline_nomsu:text() <= 8 then
        if t.type == "Action" then
          inline_nomsu:parenthesize()
        end
        return inline_nomsu
      end
    end
    local indented = tree_to_nomsu(t)
    if t.type == "Action" then
      if indented:is_multiline() then
        return NomsuCode:from(t.source, "(..)\n    ", indented)
      else
        indented:parenthesize()
      end
    end
    if inline_nomsu and indented:text():match("^[^\n]*\n[^\n]*$") and nomsu:trailing_line_len() <= 8 then
      return inline_nomsu
    end
    return indented
  end
  local _exp_0 = tree.type
  if "FileChunks" == _exp_0 then
    if tree.shebang then
      nomsu:append(tree.shebang, "\n")
    end
    for chunk_no, chunk in ipairs(tree) do
      if chunk_no > 1 then
        nomsu:append("\n\n" .. tostring(("~"):rep(80)) .. "\n\n")
      end
      if chunk.type == "Block" then
        nomsu:append(NomsuCode:from(chunk.source, table.unpack(tree_to_nomsu(chunk).bits, 2)))
      else
        nomsu:append(tree_to_nomsu(chunk))
      end
    end
    if not (nomsu:match("\n$")) then
      nomsu:append('\n')
    end
    return nomsu
  elseif "Action" == _exp_0 then
    local next_space = ""
    if tree.target then
      local target_nomsu = recurse(tree.target)
      if tree.target.type == "Block" and not target_nomsu:is_multiline() then
        target_nomsu:parenthesize()
      end
      nomsu:append(target_nomsu)
      nomsu:append(target_nomsu:is_multiline() and "\n..::" or "::")
    end
    local word_buffer = { }
    for i, bit in ipairs(tree) do
      local _continue_0 = false
      repeat
        if type(bit) == "string" then
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
              next_space = " \\\n.."
            elseif word_buffer[1] == "'" then
              next_space = ""
            end
          end
          nomsu:append(next_space, words)
          word_buffer = { }
          next_space = " "
        end
        local bit_nomsu = recurse(bit)
        if bit.type == "Block" and not bit_nomsu:is_multiline() then
          if #bit_nomsu:text() > nomsu:trailing_line_len() * GOLDEN_RATIO and #bit_nomsu:text() > 8 then
            bit_nomsu = tree_to_nomsu(bit)
          end
        end
        if (next_space == " " and not bit_nomsu:is_multiline() and nomsu:trailing_line_len() + #bit_nomsu:text() > MAX_LINE and nomsu:trailing_line_len() > 8) then
          if bit.type == 'Action' then
            bit_nomsu = NomsuCode:from(bit.source, "(..)\n    ", tree_to_nomsu(bit))
          else
            next_space = " \\\n.."
          end
        end
        if not (next_space == " " and bit.type == "Block") then
          nomsu:append(next_space)
        end
        nomsu:append(bit_nomsu)
        next_space = (bit_nomsu:is_multiline() or bit.type == 'Block') and "\n.." or " "
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
          next_space = " \\\n.."
        elseif word_buffer[1] == "'" then
          next_space = ""
        end
      end
      nomsu:append(next_space, words)
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
        nomsu:append("\n")
        if tree[i - 1].type ~= "Comment" then
          needs_space[i] = (line_nomsu:is_multiline() and prev_line:is_multiline())
          if tree[i].type == "Comment" or needs_space[i] or needs_space[i - 1] then
            nomsu:append("\n")
          end
        end
      end
      nomsu:append(line_nomsu)
      prev_line = line_nomsu
    end
    return NomsuCode:from(tree.source, ":\n    ", nomsu)
  elseif "Text" == _exp_0 then
    local max_line = math.floor(1.25 * MAX_LINE)
    local add_text
    add_text = function(tree)
      for i, bit in ipairs(tree) do
        if type(bit) == 'string' then
          bit = escape(bit)
          for j, line in ipairs(bit:lines()) do
            if j > 1 then
              nomsu:append("\n")
            elseif #line > 10 and nomsu:trailing_line_len() > max_line then
              nomsu:append("\\\n..")
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
              nomsu:append(bite)
              if #line > 0 then
                nomsu:append("\\\n..")
              end
            end
          end
        elseif bit.type == "Text" then
          add_text(bit)
        else
          nomsu:append("\\")
          local interp_nomsu = recurse(bit)
          if not (interp_nomsu:is_multiline()) then
            if bit.type == "Var" then
              if type(tree[i + 1]) == 'string' and not match(tree[i + 1], "^[ \n\t,.:;#(){}[%]]") then
                interp_nomsu:parenthesize()
              end
            elseif bit.type == "EscapedNomsu" or bit.type == "Block" then
              interp_nomsu:parenthesize()
            end
          end
          nomsu:append(interp_nomsu)
          if interp_nomsu:is_multiline() then
            nomsu:append("\n..")
          end
        end
      end
    end
    add_text(tree)
    return NomsuCode:from(tree.source, '"\\\n    ..', nomsu, '"')
  elseif "List" == _exp_0 or "Dict" == _exp_0 then
    if #tree == 0 then
      nomsu:append(tree.type == "List" and "[]" or "{}")
      return nomsu
    end
    local sep = ''
    for i, item in ipairs(tree) do
      local item_nomsu = tree_to_inline_nomsu(item)
      if #item_nomsu:text() > MAX_LINE then
        item_nomsu = recurse(item)
      end
      if item.type == 'Comment' then
        item_nomsu = tree_to_nomsu(item)
      end
      nomsu:append(sep)
      nomsu:append(item_nomsu)
      if item_nomsu:is_multiline() or item.type == 'Comment' or nomsu:trailing_line_len() + #tostring(item_nomsu) >= MAX_LINE then
        sep = '\n'
      else
        sep = ', '
      end
    end
    if tree.type == "List" then
      return NomsuCode:from(tree.source, "[..]\n    ", nomsu)
    else
      return NomsuCode:from(tree.source, "{..}\n    ", nomsu)
    end
  elseif "DictEntry" == _exp_0 then
    local key, value = tree[1], tree[2]
    if key.type == "Text" and #key == 1 and is_identifier(key[1]) then
      nomsu = NomsuCode:from(key.source, key[1])
    else
      nomsu = tree_to_inline_nomsu(key)
    end
    if key.type == "Block" then
      nomsu:parenthesize()
    end
    local value_nomsu = tree_to_nomsu(value)
    if (value.type == "Block" or value.type == "EscapedNomsu") and not value_nomsu:is_multiline() then
      value_nomsu:parenthesize()
    end
    nomsu:append(": ", value_nomsu)
    return nomsu
  elseif "Comment" == _exp_0 then
    nomsu:append("#", (tree[1]:gsub("\n", "\n    ")))
    return nomsu
  elseif "IndexChain" == _exp_0 or "Number" == _exp_0 or "Var" == _exp_0 or "Comment" == _exp_0 or "Error" == _exp_0 then
    return tree_to_inline_nomsu(tree)
  else
    return error("Unknown type: " .. tostring(tree.type))
  end
end
return {
  tree_to_nomsu = tree_to_nomsu,
  tree_to_inline_nomsu = tree_to_inline_nomsu
}
