GLOBAL = 1

def foo():
  # the global one
  print(GLOBAL)


def foo2():
  # actually a new local
  GLOBAL=2
  print(GLOBAL)

def foo3():
  # actually a new local
  global GLOBAL
  GLOBAL=3
  # back to the global one
  print(GLOBAL)

print(GLOBAL)
