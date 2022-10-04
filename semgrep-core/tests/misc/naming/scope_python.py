def foo(x):
   return 1

def foo(x):
   return x

def bar(x):
   return x

def foo(x):
   def bar(x):
     # not the x from foo!
     return x
   yield bar(x)

glob = 1

# those two should return the same md5sum
def foo(x):
  global glob;
  return complex_call(glob)
def foo2(x):
  global glob;
  return complex_call(
      glob
      ) # space and comments should not matter

# this should have a different md5sum
def foo3(x):
  global glob
  return complex_call_different(glob)

# note that those will have different md5sum, because the AST contains
# the resolved information of the parameter, which will differ
def foo(x):
  return complex_call(x)
def foo2(x):
  return complex_call(x)
