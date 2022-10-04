from x import some_fetch

#ERROR: match
GLOBAL_CONST = "SOME_CONST"

def fetch_global_const():
    # The semantic of Python is that this below reference
    # the global because it's used in an rvalue context.
    # In an lvalue context however a new local is created
    # (unless the user used the 'global' directive before to
    # "bring in scope" a global that can then be assigned.
    return some_fetch(GLOBAL_CONST)
