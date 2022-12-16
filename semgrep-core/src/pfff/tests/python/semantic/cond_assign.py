def test():
  a = 1
  b = 2
  # can't assign to conditional expression!
  # (a if True else b) = 3
  print (a if True else b)



test()
