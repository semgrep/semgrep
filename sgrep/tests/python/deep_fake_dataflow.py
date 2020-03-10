def foo():
  bar()
  #ERROR: match
  x = requests.get("foo")
  bar()
  if True:
    render(o.format(x))

