# ERROR:
try:
  pass
except B:
  pass
except A:
  pass

# ERROR:
try:
  pass
except A:
  pass
except B:
  pass

try:
  pass
except:
  pass

try:
  pass
except B:
  pass