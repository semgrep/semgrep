
//ERROR: match
@Anno
class Foo1 {}

//ERROR: match
@Anno1
@Anno2
class Foo2 {}

//OK:
@Anno(x)
class Foo3 {}

//OK:
class Foo4 {}