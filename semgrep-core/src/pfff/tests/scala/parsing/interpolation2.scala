object Foo {
  val x = q"""if($min1 == 0) "" else "(" + $min1 + ")" """

  // 4 quotes in a row
  val x = q"""if($min1 == 0) "" else "(" + $min1 + ")""""
}
