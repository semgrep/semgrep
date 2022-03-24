# Used by test_ci.py

def bar():
    a == a
    a == a
    a == a  # nosem
    a == a

    x == x  # nosem

    y == y

    z == z  # nosem

    x == 5
    y == 5  # nosem

    baz == 4  # nosem
    baz == 4

    potato == 3
