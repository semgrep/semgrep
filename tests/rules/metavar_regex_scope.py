# see https://github.com/returntocorp/semgrep/issues/2664
# fixed by https://github.com/returntocorp/semgrep/pull/3157

def foo():
    #ruleid: should match here
    return 1

def bar():
    #ruleid: should match here
    return 2

def baz():
    return 3
