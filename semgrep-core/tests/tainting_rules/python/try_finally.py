def foo():
  try:
    raise Foo()
  finally:
    #ERROR:
    sink(source)
