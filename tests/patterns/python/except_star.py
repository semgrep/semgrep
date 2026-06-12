# PEP 654 exception groups & `except*` (Python 3.11).
def f():
    # ERROR: match
    try:
        g()
    except* ValueError as e:
        print(e)
