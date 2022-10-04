-- ERROR: match
foo = foo

foo = {}

-- ERROR: match
foo = foo

qux = foo:bar() == foo:baz()

qux = foo:bar() == foo:bar()

local a, b, c = 1, 2, 3

-- ERROR: match
a, b, c = a

-- ERROR: match
a = a, b, c
