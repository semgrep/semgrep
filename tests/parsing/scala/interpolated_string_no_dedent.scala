
// We don't want a DEDENT or a NEWLINE to be posted before
// this $z. If it is, it will mess up our string interpolation
// parsing.

def foo(): String =
  val x = 2 

  s"""foo: $x (bar: $y)
$z"""

def foo(): String =
  val x = 2 

  s"""foo: $x (bar: $y)
$z"""
  val y = 3
