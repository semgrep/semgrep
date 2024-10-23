
// MATCH:
val good1 = new Bar()
// MATCH:
val good2 = Foo.apply()

// OK:
val bad1 = foo.apply()
// OK:
val bad2 = Foo.a.apply()