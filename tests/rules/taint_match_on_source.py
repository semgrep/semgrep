def test(c):
  #ruleid: tainting
  x = source()
  if c:
    y = x
  else:
    y = "safe"
  sink(y)
