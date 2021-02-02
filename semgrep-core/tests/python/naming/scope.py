import foo
from foo import imported

global_ = "global"

def foo(param1, param2):
  local = "local"
  # actually a new local!
  global_ = "local"
  # actually not a new local but imported one?
  imported = "imported"
  return local
  return param1
  return imported


def foo2():
  global global_
  # now it references the global
  global_ = "global2"
  return global_


def foo3(x, y):
   def bar(x):
     nonlocal y
     return x+y
   yield bar(x)

print(foo.imported)
print(global_)
foo(1,2)
print(global_)
foo2()
print(global_)
