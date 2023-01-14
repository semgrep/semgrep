#ERROR: match, actually we want to check it matches the whole file
from foo import (
    bar,
    baz,
    qux
)
# TODO this was originally badly converted in a
# Pr [Block (import1; import2; import3); Def]
# Then when the pattern was import1; ..., it was
# matched only inside the Block

def func():
    pass

