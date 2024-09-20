def foo():
  try:
    raise Foo()
  finally:
    #ruleid: test
    sink(source)
