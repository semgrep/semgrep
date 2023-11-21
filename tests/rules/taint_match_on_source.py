def test(c):
  #ruleid: test
  x = source()
  if c:
    y = x
  else:
    y = "safe"
  sink(y)
