# https://github.com/returntocorp/semgrep/issues/6079
def test(x):
    a = x >= 80300 and "typarray" or "NULL"
    #ruleid: test
    foo(a)

    b = x >= 80300 and v or "NULL"
    #ok: test
    foo(b)

    c = x >= 80300 and "typarray" or w
    #ok: test
    foo(c)
