def foo(x):
  #ok:test
  a = x + b
  #ok:test
  c = a + x
  #ruleid:test
  return (c + x)
