def foo4():
  # ERROR:
  name = "foo"
  locale = "bar"
  query = f"SELECT * from fooo where name={name} and age={locale}"

def foo5():
  # OK:
  name = "foo"
  age = 1
  query = f"SELECT * from fooo where name={name} and age={age}"