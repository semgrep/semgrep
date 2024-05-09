# OK:
@anno1
def f1(foo: 'foo'):
    """
    no `route` suffix
    """
    x = 2

# OK:
@route('/bar')
def f2():
    """
    not fqn, no metavar matching
    """
    x = 3

# ERROR:
@app.route('/foo')
def f3():
    """
    true positive target
    """
    x = 1

# ERROR:
@app.route
def f4():
    """
    implicit parenthesis assumed
    """
    x = 1

def f5():
    x = 2


