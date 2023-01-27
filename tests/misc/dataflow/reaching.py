# scheck -dfg_generic reaching.py
def foo():
  a = 1
  if True:
    a = 2
  else:
    a = 3
  print(a)
