object Foo {
  // deprecated in 2.13 (and Scala 3), want : Unit = now
  def bar(a:Int, b:Int) {
    bar(1, 2)
  }
}
