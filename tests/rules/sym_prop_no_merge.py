def test(c):
  if c:
    x = a
    y = x.foo
  else:
    x = b
    y = x.foo
  #OK:
  return y
