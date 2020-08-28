def foo():
  try:
    foo()
  except Exn as e:
    return e
