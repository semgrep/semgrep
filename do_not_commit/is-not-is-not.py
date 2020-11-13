x = 'foo'

# ruleid: is-not-is-not
if x is (not 'hello there'):
    pass

# ruleid: is-not-is-not
if x is (not None):
    pass

# OK
if x is not None:
    pass
