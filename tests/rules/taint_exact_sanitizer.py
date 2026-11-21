def yay_works():
  decoded = baz(user_input)
  # ruleid: missing
  qux(decoded)

def uh_oh():
  decoded = baz(user_input)
  # Because the sanitizer is "exact", the `decoded` is not considered sanitized
  # as it would have normally been.
  # ruleid: missing
  return foo.bar(qux(decoded))

def ok_inline():
  # The sanitizer call itself is sanitized when passed to the sink inline.
  # ok: missing
  qux(foo.bar(user_input))

def ok_assign():
  # The sanitizer call itself is still sanitized when its result is first
  # assigned to a variable, exactly as when passed to the sink inline.
  clean = foo.bar(user_input)
  # ok: missing
  qux(clean)

def ok_assign_nested():
  clean = foo.bar(baz(user_input))
  # ok: missing
  qux(clean)

def bad_assign():
  dirty = user_input
  # ruleid: missing
  qux(dirty)
