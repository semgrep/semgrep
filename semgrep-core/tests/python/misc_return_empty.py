def foo():
  # no match, but no exn either!
  return

def bar():
  #ERROR: match
  return {}
