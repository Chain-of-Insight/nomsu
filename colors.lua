if COLOR_ENABLED then
  local codes = {
    normal = 0,
    reset = 0,
    bright = 1,
    bold = 1,
    dim = 2,
    italic = 3,
    underscore = 4,
    ["slow blink"] = 5,
    ["fast blink"] = 6,
    reverse = 7,
    inverse = 7,
    inverted = 7,
    hidden = 8,
    strikethrough = 9,
    black = 30,
    red = 31,
    green = 32,
    yellow = 33,
    blue = 34,
    magenta = 35,
    cyan = 36,
    white = 37,
    on_black = 40,
    on_red = 41,
    on_green = 42,
    on_yellow = 43,
    on_blue = 44,
    on_magenta = 45,
    on_cyan = 46,
    on_white = 47
  }
  local inverses = {
    [0] = 0,
    [1] = 22,
    [2] = 22,
    [3] = 23,
    [4] = 24,
    [5] = 25,
    [6] = 25,
    [7] = 27,
    [8] = 28,
    [9] = 29,
    [30] = 39,
    [31] = 39,
    [32] = 39,
    [33] = 39,
    [34] = 39,
    [35] = 39,
    [36] = 39,
    [37] = 39,
    [38] = 39,
    [40] = 49,
    [41] = 49,
    [42] = 49,
    [43] = 49,
    [44] = 49,
    [45] = 49,
    [46] = 49,
    [47] = 49,
    [48] = 49
  }
  return function(colors, s)
    colors = colors:gsub("on (%a+)", function(self)
      return codes['on_' .. self]
    end)
    colors = colors:gsub("(%a+)", function(self)
      return codes[self]
    end)
    colors = colors:gsub(" ", ";")
    local colorize = "\x1b[" .. colors .. 'm'
    local reset = colorize:gsub("%d+", function(c)
      return inverses[tonumber(c)]
    end)
    return s and (colorize .. s .. reset) or colorize
  end
else
  return function(colors, s)
    return (s or '')
  end
end
