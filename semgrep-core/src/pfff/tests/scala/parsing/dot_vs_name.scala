object Foo {
  def foo() {
    val x = y.foo.bar;
    val y = foo().bar;
  }
}
