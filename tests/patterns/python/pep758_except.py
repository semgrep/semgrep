# PEP 758 (Python 3.14): 'except A, B:' is the same as 'except (A, B):'.

# ERROR:
try:
  pass
except ValueError, TypeError:
  pass

# ERROR:
try:
  pass
except TypeError, ValueError, KeyError:
  pass

# ERROR:
try:
  pass
except (TypeError, ValueError):
  pass

def f():
  # ERROR:
  try:
    pass
  except ValueError, TypeError:
    pass

# ERROR:
try:
  pass
except* ValueError, TypeError:
  pass

# ERROR:
try:
  pass
except* TypeError, ValueError, KeyError:
  pass

# OK: no ValueError
try:
  pass
except TypeError, KeyError:
  pass
