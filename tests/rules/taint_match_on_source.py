# No security problem
def test1(c):
  #ruleid: test
  x = source()
  if c:
    y = x
  else:
    y = "safe"
  sink(y)
