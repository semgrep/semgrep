def test1():
  source()
  foo()
  bar()
  #ruleid: test
  sink()

def test2():
  source()
  foo()
  bar()
  if baz():
    #ruleid: test
    sink()

def test2():
  if foo():
    source()
  bar()
  if baz():
    #ruleid: test
    sink()
