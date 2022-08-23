# the function is only necessary because propagation only runs inside of functions at the moment
def foo():
  # ruleid: symbol_prop_redundancy
  x = g(5)
  f(x)
