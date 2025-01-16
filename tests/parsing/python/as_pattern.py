try:
  1/0
except Exception as e:
  print("Caught exception: %s" % e)

with f() as f:
  pass

with (
  f() as a,
  g() as b,
):
  a, b