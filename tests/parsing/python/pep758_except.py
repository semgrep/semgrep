# PEP 758 (Python 3.14): unparenthesized exception type lists.
try:
    pass
except ValueError, TypeError:
    pass
except KeyError, IndexError, OSError:
    pass

try:
    pass
except* ValueError, TypeError:
    pass
except* OSError as e:
    pass
