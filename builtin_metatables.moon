-- This file defines some methods on Lua numbers, bools, actions, nil, and coroutines
require "text"

number_mt =
    __type: "a Number"
    __len: => @
    as_lua: tostring
    as_nomsu: tostring
    as_text: tostring
    as_a_number: => @
    rounded: => math.floor(@ + .5)
    rounded_down: math.floor
    rounded_up: math.ceil
    to_the_nearest: (rounder)=> rounder * math.floor(@/rounder + 0.5)
    base16: => ("%X")\format(@)
number_mt.__index = number_mt
debug.setmetatable 0, number_mt

bool_mt =
    __type: "a Boolean"
    __len: => @ and 1 or 0
    as_lua: tostring
    as_nomsu: => @ and "yes" or "no"
    as_text: => @ and "yes" or "no"
    _and: (cond)=> @ and cond
    _or: (cond)=> @ or cond
    xor: (cond)=> @ == (not cond)
bool_mt.__index = bool_mt
debug.setmetatable true, bool_mt

fn_mt =
    __type: "an Action"
    as_text: => (tostring(@)\gsub("function", "Action"))
    __add: (other)=>
        if type(@) == 'function' and type(other) == 'function'
            (...)-> (@(...) + other(...))
        elseif type(@) == 'function' then (...)-> (@(...) + other)
        else (...)-> (@ + other(...))
    __sub: (other)=>
        if type(@) == 'function' and type(other) == 'function'
            (...)-> (@(...) - other(...))
        elseif type(@) == 'function' then (...)-> (@(...) - other)
        else (...)-> (@ - other(...))
    __mul: (other)=>
        if type(@) == 'function' and type(other) == 'function'
            (...)-> (@(...) * other(...))
        elseif type(@) == 'function' then (...)-> (@(...) * other)
        else (...)-> (@ * other(...))
    __div: (other)=>
        if type(@) == 'function' and type(other) == 'function'
            (...)-> (@(...)  other(...))
        elseif type(@) == 'function' then (...)-> (@(...)  other)
        else (...)-> (@  other(...))
    __mod: (other)=>
        if type(@) == 'function' and type(other) == 'function'
            (...)-> (@(...) % other(...))
        elseif type(@) == 'function' then (...)-> (@(...) % other)
        else (...)-> (@ % other(...))
    __band: (other)=>
        if type(@) == 'function' and type(other) == 'function'
            (...)-> (@(...) and other(...))
        elseif type(@) == 'function' then (...)-> (@(...) and other)
        else (...)-> (@ and other(...))
    __bor: (other)=>
        if type(@) == 'function' and type(other) == 'function'
            (...)-> (@(...) or other(...))
        elseif type(@) == 'function' then (...)-> (@(...) or other)
        else (...)-> (@ or other(...))
    __bxor: (other)=>
        if type(@) == 'function' and type(other) == 'function'
            (...)-> (@(...) != other(...))
        elseif type(@) == 'function' then (...)-> (@(...) != other)
        else (...)-> (@ != other(...))
fn_mt.__index = fn_mt
debug.setmetatable (->), fn_mt

co_mt =
    __type: "a Coroutine"
    as_text: => (tostring(@)\gsub("thread", "Coroutine"))
co_mt.__index = co_mt
debug.setmetatable(coroutine.create(->), co_mt)

nil_mt =
    __type: "Nil"
    as_lua: => "nil"
    as_nomsu: => "nil"
    as_text: => "nil"
nil_mt.__index = nil_mt
debug.setmetatable nil, nil_mt
