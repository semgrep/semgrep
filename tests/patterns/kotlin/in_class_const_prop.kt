
var x : T

class Foo {
  var y : T

  init {
    // ERROR:
    res1 = x.foo
    // ERROR:
    res2 = y.foo
  }
}