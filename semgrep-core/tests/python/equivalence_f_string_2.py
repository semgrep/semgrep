def foo1():
  # ERROR:
  select = "select * "
  query = f"{select} from foo"

def foo2():
  # ERROR:
  select = "select * "
  name = "foo"
  query = f"{select} from foo " + f"where name={name}"

def foo2a():
  # ERROR:
  select = "select * "
  name = "foo"
  query = f"{select} from foo where name={name}"

def foo5():
  # OK:
  num = 1
  query = f"{num} is 1"

def foo6():
  # OK:
  complex_func = foo()
  query = f"{complex_func} is foo"
