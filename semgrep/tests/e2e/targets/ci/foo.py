# Used by test_ci.py

def bar():
    a == a
    a == a
    a == a  # nosemgrep
    a == a

    x == x  # nosemgrep

    y == y

    z == z  # nosemgrep

    x == 5
    y == 5  # nosemgrep

    baz == 4  # nosemgrep
    baz == 4

    potato == 3
