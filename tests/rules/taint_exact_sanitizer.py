def yay_works():
  decoded = baz(user_input)
  # ruleid: missing
  qux(decoded)

def uh_oh():
  decoded = baz(user_input)
  # ruleid: missing
  return foo.bar(qux(decoded))
