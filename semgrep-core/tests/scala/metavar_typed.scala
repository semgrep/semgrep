def foo = {
  val x: Int = 0
  val y: String = ""
  //ERROR: match
  foo(x);

  //OK:
  foo(y);
}
