#ERROR: matching
@alpha
@beta 
@charlie('xyz')
def foo():
    x = 42


#ERROR: matching
@beta
@alpha 
@charlie('bla')
def foo():
    x = 42
