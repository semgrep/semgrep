def test():
  x = source()
  x.a = sanitize()
  #ok: test
  sink(x.a)
  #ruleid: test
  sink(x) # `x` is still tainted
  x.a.i = source()
  #ruleid: test
  sink(x.a.i)
  #ruleid: test
  sink(x.a) # `x.a` now triggers a finding because `x.a.i` is reachable
  #ruleid: test
  sink(x)
