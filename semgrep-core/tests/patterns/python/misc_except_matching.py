# ERROR:
try:
  pass
except ValueError:
  pass

# ERROR:
try:
  pass
except (ValueError, TypeError):
  pass

# ERROR:
try:
  pass
except (ValueError):
  pass
