#ok:test
bar = x

def foo(x):
  #ruleid:test
  a = x + b
  #ruleid:test
  c = a + x
  #ruleid:test
  return (c + x)
