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
