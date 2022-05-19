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
