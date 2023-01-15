def ok():
    x = source
    # `x` is sanitized by side effect!
    sanitize(x)
    #ok: test
    sink(x)

def bad():
    x = source
    #ruleid: test
    sink(x)

def also_bad():
    x = source
    sanitize(x)
    x = source
    #ruleid: test
    sink(x)
