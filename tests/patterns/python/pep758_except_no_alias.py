# PEP 758 (Python 3.14): in 'except A, B:', B is a type, not a bound name.

# ERROR:
try:
  pass
except ValueError as e:
  pass

# OK:
try:
  pass
except ValueError, TypeError:
  pass

# ERROR:
try:
  pass
except* ValueError as e:
  pass

# OK:
try:
  pass
except* ValueError, TypeError:
  pass
