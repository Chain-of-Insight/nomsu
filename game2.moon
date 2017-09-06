#!/usr/bin/env moon
utils = require 'utils'
Game = require 'nomic'
g = Game(require'core')

print("===========================================================================================")


g\run[[

say [..]
    "this is a stupidly long list", "the items go way past the 80 character", "limit that older consoles"
    "had.", "It just keeps going and going"

rule "dumbfunc %a %b %c %d %e":
    say "doop"

dumbfunc..
    "this is a stupidly long set of arguments" "the items go way past the 80 character" "limit that older consoles"
    "had." "It just keeps going and going"

]]
g\run[[
rule "four": return 4
rule "say both %one and %two":
    say %one
    say %two

say both ..
    "a list:"
    and [..]
        1,2,3,(four),(5)

say "done"
]]

g\run[[
rule "do %thing also %also-thing":
    do %thing
    do %also-thing
    return 99
]]
g\run[[
do: say "one liner"
..also: say "another one liner"
]]
g\run[[

say (..)
    do:
        say "hi"
        return 5
        say "bye"

]]
g\run[[
say (do: return "wow")
if 1: say "hi1" ..else: say "bye1"

if 1: say "hi2"
..else: say "bye2"

]]
g\run[[
rule "foo %x":
    if %x:
        say "YES"
        55
    ..else:
        say "NO"
        -99

say (foo 1)
say (foo (false))

]]

g\run[[
say (1 + (-(2 * 3)))
]]

g\run[[
for "x" in ["A","B","C"]:
    say %x
]]
g\run[[
say (for "x" in [1,2,3]:%x + 100)
say (..)
    for "x" in [1,2,3]:
        %x + 200
]]

g\run[[
if (1 == 1):
    say "Simple macros work!"
unless (1 > 2):
    say "This one too!"
]]

g\simplemacro [[smet %varname = %value]],[[
lua ["vars[\"", %varname, "\"] = ", %value]
lua ["vars[\"", %varname, "\"] = 2*vars[\"", %varname, "\"]"]
]]

g\run[[
smet "fnord" = 23
say %fnord
]]

g\run[[

rule "fizz buzz":
    for "i" = 1 to 100:
        if ((%i mod 15) == 0):
            say "fizzbuzz"
        ..else: if ((%i mod 3) == 0):
            say "fizz"
        ..else: if ((%i mod 5) == 0):
            say "buzz"
        ..else:
            say %i

fizz buzz

]]
