def test1(n):
  x = "bar"
  y = x * n
  # MATCH:
  foo(y)

def test2(n):
  if (n > 42):
    x = "bar"
  else:
    x = "baz"
  y = x * n
  # MATCH:
  foo(y)
