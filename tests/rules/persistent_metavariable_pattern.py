
def foo():
  x = 1

  # ruleid: persistent-metavariable-pattern
  foo(bar(5))

  y = 23