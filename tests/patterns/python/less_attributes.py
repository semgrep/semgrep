# ERROR: matching
@alpha
@beta
@charlie("xyz")
def foo():
    pass


# ERROR: matching
@beta
@alpha
@charlie("xyz")
def foo():
    pass
