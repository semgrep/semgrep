# PEP 758 (Python 3.14): a dotted type in 'except A, x.B:' is a caught type, not
# a bound name. Before the fix it was dropped from the tuple entirely, silently.

# ERROR:
try:
  pass
except ValueError, json.JSONDecodeError:
  pass

# ERROR:
try:
  pass
except (ValueError, json.JSONDecodeError):
  pass

# ERROR:
try:
  pass
except* ValueError, json.JSONDecodeError:
  pass

# OK: no json.JSONDecodeError
try:
  pass
except ValueError, TypeError:
  pass
