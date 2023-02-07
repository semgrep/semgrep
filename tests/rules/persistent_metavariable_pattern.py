
def foo():
  x = 1

  foo(
    bar(
      # ruleid: persistent-metavariable-pattern
      5
    ),
  )

  y = 23