def foo():
  #ERROR: match
  if True:
     x = 1
  else:
     x = 2


def foo():
  # this should not match
  if True:
     x = 1
