class Foo @Inject()(val cc: String) extends Bar {
  def hello(): Unit = println("hi")
}
