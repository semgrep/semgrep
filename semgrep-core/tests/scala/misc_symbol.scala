object Foo {
  def bar() {
    //ERROR: match
    var x = 'Foo
    x = 'Bar
    return x
  }
}
