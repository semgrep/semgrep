CONST_GLOBAL = "secret"

# this is modifed somewhere, so not inferred as a constant
GLOBAL = "secret"

def foo():
    # ERROR: match
    bar(CONST_GLOBAL)


def bar():
    global GLOBAL
    GLOBAL="x"
