object Foo {

  def foo() = {
    // here we don't want the newline; we want to skip them
    bar(2)(
      3)
  }

}
