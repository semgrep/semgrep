def direct():
    # literal passed directly
    # ruleid: constprop-metavar-compare
    sink(42)


def propagated():
    x = 42
    # single-assignment const -> propagated Lit 42 is read by the comparison
    # ruleid: constprop-metavar-compare
    sink(x)


def wrong_value():
    y = 41
    # propagated, but 41 != 42
    # ok: constprop-metavar-compare
    sink(y)


def not_constant():
    z = 42
    z = input()
    # reassigned -> NotCst, comparison cannot confirm == 42
    # ok: constprop-metavar-compare
    sink(z)
