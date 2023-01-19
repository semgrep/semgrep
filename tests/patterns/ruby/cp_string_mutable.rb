# https://github.com/returntocorp/semgrep/issues/3316
def test2(user_input)
  a = "foo"
  #ERROR:
  test(a)
  a << user_input # equivalent to a.<<(user_input)
  # a isn't a constant string anymore
  #OK:
  test(a)
end
