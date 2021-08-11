def foo(x):
  #ruleid:test
  a = x + b
  #ruleid:test
  c = a + x
  #ruleid:test
  return (c + x)

#ruleid:test
bar = x

#ruleid:test
baz = 2*bar
