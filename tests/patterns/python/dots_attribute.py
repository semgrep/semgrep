# ERROR: matching
@alpha
@beta
@charlie("xyz")
def foo():
    pass


# ERROR: matching
@beta
@alpha
@charlie("bla")
def foo():
    pass
