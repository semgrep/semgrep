# see https://github.com/returntocorp/semgrep/issues/2664
# fixed by https://github.com/returntocorp/semgrep/pull/3157

def foo():
    #ruleid: my_pattern_id
    return 1

def bar():
    #ruleid: my_pattern_id
    return 2

def baz():
    return 3
