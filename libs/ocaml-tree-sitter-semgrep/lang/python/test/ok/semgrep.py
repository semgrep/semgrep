# PEP 758 (Python 3.14) bare multi-exception except clause
def f(x):
    match x:
        case 1:
            return 1
    try:
        pass
    except OSError, ValueError, KeyError:
        pass

try:
    pass
except A, B, C, D:
    pass
