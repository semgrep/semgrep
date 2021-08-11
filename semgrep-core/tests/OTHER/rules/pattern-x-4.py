#ok:test
bar = x

def bar(x):
  #ok:test
  a = x + b
  #ok:test
  c = a + x
  #ok:test
  return (c + x)
