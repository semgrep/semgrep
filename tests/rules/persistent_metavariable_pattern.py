
def foo():
  x = 1

  foo(
    # ruleid: persistent-metavariable-pattern
    bar(5),
  )

  y = 23