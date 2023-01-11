local o = { foo: 1, bar: 2};
[
std.objectHas(o, 'foo'),
std.objectHas(o, 'nope')
]

// possible errors to handle
//std.objectHas(1, 'foo')
//std.objectHas(o, 2)
