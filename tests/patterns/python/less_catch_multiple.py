# MATCH:
try:
  pass
except C:
  pass
except B:
  pass
except A:
  pass

# MATCH:
try:
  pass
except C:
  pass
except A:
  pass
except B:
  pass