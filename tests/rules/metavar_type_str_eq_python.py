def test(a: str) -> int:
    # ruleid: no-string-is
    if a is "hello":
        return 1
    # ruleid: no-string-is
    if "hello" is a:
        return 2
    # ok: no-string-is
    if b is 2:
        return -1
    # ok: no-string-is
    if null is "hello":
        return 12
    # ok: no-string-is
    if "hello" is null:
        return 0
