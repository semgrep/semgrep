
class Foo {
  final var x = 5
  const var y = 5

  init {
    // ERROR:
    res1 = foo(x)
    // ERROR:
    res2 = foo(y)
  }
}