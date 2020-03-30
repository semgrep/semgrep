def foo1():
  # ERROR:
  w = "foo"
  query = f"hello {w} hel"

def foo2():
  # ERROR:
  ww = "bar"
  query = f"ASD{ww}ASASD"

def foo3():
  # OK:
  www = "bar"
  query = f"SELECT {www}"

def foo4():
  # OK:
  ww = "foo"
  www = "bar"
  query = f"SELECT {www} and {ww}"

def foo5():
  # OK:
  num = 1
  query = f"num = {num} !"

def foo6():
  # OK:
  complex_func = foo()
  query = f"complex = {complex_func} !"
