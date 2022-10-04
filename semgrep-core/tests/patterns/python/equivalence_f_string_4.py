def foo(x):
  A = x
  # ERROR:
  B = x + x
  query = f"{A} {B}"

