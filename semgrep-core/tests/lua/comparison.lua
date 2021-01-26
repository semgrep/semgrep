local foo = {}

-- ERROR: match
local bar = foo == foo

local baz = foo:bar() == foo:baz()

-- ERROR: match
local qux = foo:bar() == foo:bar()

-- ERROR: match
foo = hoge == hoge
