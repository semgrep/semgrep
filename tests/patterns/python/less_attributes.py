#ERROR: matching
@alpha
@beta 
@charlie('xyz')
def foo():
    x = 42


#ERROR: matching
@beta
@alpha 
@charlie('xyz')
def foo():
    x = 42
