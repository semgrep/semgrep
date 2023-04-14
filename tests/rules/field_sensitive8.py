def test():
  x = source()
  x.a = sanitize()
  x.a.i = source()
  #ruleid: test
  sink(x.a.i)
  #ok: test
  sink(x.a)
  #ruleid: test
  sink(x)
