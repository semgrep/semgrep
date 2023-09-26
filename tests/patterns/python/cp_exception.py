def throw_must_not_go_through_else():
  x = 0
  try:
    raise RuntimeError()
  except Exception as e:
    # assume this path may be taken
    x = 0
  else:
    # no flow through here because an exception was thrown
    x = 9999

  # ERROR:
  x == 0

def no_throw_goes_through_else():
  x = 0
  try:
    pass
  except Exception as e:
    x = 9999
  else:
    x = 0

  # ERROR:
  x == 0

def may_throw_goes_through_catch_and_else():
  x = 0
  try:
    any_function_call_may_raise()
  except Exception as e:
    # exception may be thrown
    x = 0
  else:
    # exception may not be thrown either
    x = 1

  # OK:
  x == 0

def exception_or_not_goes_through_finally():
  x = 0
  try:
    any_function_call_may_raise()
  except Exception as e:
    x = 0
  else:
    x = 0
  finally:
    x = 1

  # OK:
  x == 0

def non_nested_try_statements_are_independent():
  x = 0
  try:
    try:
      any_function_call_may_raise()
    finally:
      pass
  except Exception as e:
    # Exception may be propagated here.
    x = 1
  # OK:
  x == 0

  y = 0
  # No propagation happening here. This is a separate try-statement.
  try:
    try:
      pass
    finally:
      pass
  except Exception as e:
    y = 1

  # ERROR:
  y == 0
