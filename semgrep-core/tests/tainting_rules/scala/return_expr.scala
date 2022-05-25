def no_ret(x: Int) : Int = {
  // ERROR: match
  3 * foo(x) + 412
}

def ret(x: Int) : Int = {
  // ERROR: match
  return foo(x) + 4
}
