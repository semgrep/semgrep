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

    b == b # Triage ignored by syntactic_id
    a == a # Triage ignored by match_based_id

    d2 = danger
    sink(d2)
