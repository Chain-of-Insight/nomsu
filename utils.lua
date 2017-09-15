local utils
utils = {
  is_list = function(t)
    local i = 1
    for _ in pairs(t) do
      if t[i] == nil then
        return false
      end
      i = i + 1
    end
    return true
  end,
  repr = function(x, add_quotes)
    if add_quotes == nil then
      add_quotes = false
    end
    local _exp_0 = type(x)
    if 'table' == _exp_0 then
      if utils.is_list(x) then
        return "{" .. tostring(table.concat((function()
          local _accum_0 = { }
          local _len_0 = 1
          for _index_0 = 1, #x do
            local i = x[_index_0]
            _accum_0[_len_0] = utils.repr(i, true)
            _len_0 = _len_0 + 1
          end
          return _accum_0
        end)(), ", ")) .. "}"
      else
        return "{" .. tostring(table.concat((function()
          local _accum_0 = { }
          local _len_0 = 1
          for k, v in pairs(x) do
            _accum_0[_len_0] = "[" .. tostring(utils.repr(k, true)) .. "]= " .. tostring(utils.repr(v, true))
            _len_0 = _len_0 + 1
          end
          return _accum_0
        end)(), ", ")) .. "}"
      end
    elseif 'string' == _exp_0 then
      if not add_quotes then
        return x
      elseif not x:find([["]]) and not x:find("\n") then
        return "\"" .. x .. "\""
      elseif not x:find([[']]) and not x:find("\n") then
        return "\'" .. x .. "\'"
      else
        for i = 0, math.huge do
          local eq = ("="):rep(i)
          if not x:find("%]" .. tostring(eq) .. "%]") and not x:match(".*]" .. tostring(eq) .. "$") then
            if x:sub(1, 1) == "\n" then
              return "[" .. tostring(eq) .. "[\n" .. x .. "]" .. tostring(eq) .. "]"
            else
              return "[" .. tostring(eq) .. "[" .. x .. "]" .. tostring(eq) .. "]"
            end
          end
        end
      end
    else
      return tostring(x)
    end
  end,
  split = function(str, sep)
    if sep == nil then
      sep = "%s"
    end
    local _accum_0 = { }
    local _len_0 = 1
    for chunk in str:gmatch("[^" .. tostring(sep) .. "]+") do
      _accum_0[_len_0] = chunk
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end,
  accumulate = function(glue, co)
    if co == nil then
      glue, co = "", glue
    end
    local bits = { }
    for bit in coroutine.wrap(co) do
      table.insert(bits, bit)
    end
    return table.concat(bits, glue)
  end,
  range = function(start, stop, step)
    if stop == nil then
      start, stop, step = 1, start, 1
    elseif step == nil then
      step = 1
    end
    return setmetatable({
      start = start,
      stop = stop,
      step = step
    }, {
      __ipairs = function(self)
        local iter
        iter = function(self, i)
          if i <= (self.stop - self.start) / self.step then
            return i + 1, self.start + i * self.step
          end
        end
        return iter, self, 0
      end
    })
  end,
  keys = function(t)
    local _accum_0 = { }
    local _len_0 = 1
    for k in pairs(t) do
      _accum_0[_len_0] = k
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end,
  values = function(t)
    local _accum_0 = { }
    local _len_0 = 1
    for _, v in pairs(t) do
      _accum_0[_len_0] = v
      _len_0 = _len_0 + 1
    end
    return _accum_0
  end,
  set = function(list)
    local _tbl_0 = { }
    for _index_0 = 1, #list do
      local i = list[_index_0]
      _tbl_0[i] = true
    end
    return _tbl_0
  end,
  sum = function(t)
    do
      local tot = 0
      for _, x in pairs(t) do
        tot = tot + x
      end
      return tot
    end
  end,
  product = function(t)
    do
      local prod = 1
      for _, x in pairs(t) do
        prod = prod * x
      end
      return prod
    end
  end,
  all = function(t)
    for _, x in pairs(t) do
      if not x then
        return false
      end
    end
    return true
  end,
  any = function(t)
    for _, x in pairs(t) do
      if x then
        return true
      end
    end
    return false
  end,
  min = function(list, keyFn)
    if keyFn == nil then
      keyFn = (function(x)
        return x
      end)
    end
    assert(utils.is_list(list), "min() expects to be operating on a list")
    do
      local best = list[1]
      if type(keyFn) == 'table' then
        local keyTable = keyFn
        keyFn = function(k)
          return keyTable[k]
        end
      end
      for i = 2, #list do
        if keyFn(list[i]) < keyFn(best) then
          best = list[i]
        end
      end
      return best
    end
  end,
  max = function(list, keyFn)
    if keyFn == nil then
      keyFn = (function(x)
        return x
      end)
    end
    assert(utils.is_list(list), "min() expects to be operating on a list")
    do
      local best = list[1]
      if type(keyFn) == 'table' then
        local keyTable = keyFn
        keyFn = function(k)
          return keyTable[k]
        end
      end
      for i = 2, #list do
        if keyFn(list[i]) > keyFn(best) then
          best = list[i]
        end
      end
      return best
    end
  end,
  sort = function(list, keyFn, reverse)
    if keyFn == nil then
      keyFn = (function(x)
        return x
      end)
    end
    if reverse == nil then
      reverse = false
    end
    assert(utils.is_list(list), "min() expects to be operating on a list")
    if type(keyFn) == 'table' then
      local keyTable = keyFn
      keyFn = function(k)
        return keyTable[k]
      end
    end
    local comparison
    if reverse then
      comparison = (function(x, y)
        return (keyFn(x) > keyFn(y))
      end)
    else
      comparison = (function(x, y)
        return (keyFn(x) < keyFn(y))
      end)
    end
    return table.sort(list, comparison)
  end,
  equivalent = function(x, y)
    if x == y then
      return true
    end
    if type(x) ~= type(y) then
      return false
    end
    if type(x) ~= 'table' then
      return false
    end
    for k, v in pairs(x) do
      if y[k] ~= v then
        return false
      end
    end
    for k, v in pairs(y) do
      if x[k] ~= v then
        return false
      end
    end
    return true
  end,
  key_for = function(t, value)
    for k, v in pairs(t) do
      if v == value then
        return k
      end
    end
    return nil
  end,
  clamp = function(x, min, max)
    if x < min then
      return min
    elseif x > max then
      return max
    else
      return x
    end
  end,
  mix = function(min, max, amount)
    return (1 - amount) * min + amount * max
  end,
  sign = function(x)
    if x == 0 then
      return 0
    elseif x < 0 then
      return -1
    else
      return 1
    end
  end,
  round = function(x, increment)
    if increment == nil then
      increment = 1
    end
    if x >= 0 then
      return math.floor(x / increment + .5) * increment
    else
      return math.ceil(x / increment - .5) * increment
    end
  end
}
return utils
