def foo():
  #ERROR: match, but it should report a range that stops after bar
  foo()
  foobar()
  bar()
  # this below should not be reported in the match
  stuff()
  morestuff()

