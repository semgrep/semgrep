def f(ctx):
    match ctx:
        case {K.KEY: v}:
            return v


def g(ctx):
    match ctx:
        case {"a" "b": v}:
            return v
