object Foo {
  def Escape[_: P] = "\\" ~/ CharIn("""btnfr'\\"]""")
}
