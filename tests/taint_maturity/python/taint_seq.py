def test1():
  #ruleid: taint-maturity
  sink("tainted")

def test2():
  x = "safe"
  a = x
  x = "tainted"
  y = x
  z = y
  #ruleid: taint-maturity
  sink(z)
  #OK: taint-maturity
  sink(a)
  #OK: taint-maturity
  safe(z)

def test3():
  #ok: taint-maturity
  sink(sanitize("tainted"))

def test4():
  x = "tainted"
  x = sanitize(x)
  #ok: taint-maturity
  sink(x)
