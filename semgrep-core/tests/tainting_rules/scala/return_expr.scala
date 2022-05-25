def no_ret(x: Int) : Int = {
  // ruleid: test
  3 * foo(x) + 412
}

def ret(x: Int) : Int = {
  // ruleid: test
  return foo(x) + 4
}
