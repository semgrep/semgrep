def test():
  a  = ()
  # this introduces a newvar: even if it seems an rvalue
  for k, v in foo():
    # this is not an lvalue for k!
    a[k] = 1
