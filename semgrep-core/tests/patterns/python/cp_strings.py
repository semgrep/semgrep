# https://github.com/returntocorp/semgrep/issues/3881

def test1():
    #ERROR:
    return (
      r"__________________/\\\\\\\\\___________________        " + "\n"
    )

def test2():
    #ERROR:
    return (
      b"__________________/\\\\\\\\\___________________        " + "\n"
    )

def test3():
    #ERROR:
    return (
      u"__________________/\\\\\\\\\___________________        " + "\n"
    )
