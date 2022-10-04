# https://github.com/returntocorp/semgrep/issues/3316

def test1(user_input)
  a = "foo"
  #ERROR:
  test(a)
  a.concat("bar")
  #ERROR:
  test(a)
  a.concat(user_input)
  # a isn't a constant string anymore
  #OK:
  test(a)
end
