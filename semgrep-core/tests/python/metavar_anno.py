# ERROR:
@anno1
def bar(foo: 'foo'):
    x = 2

# ERROR:
@anno1
@anno2
def foobar():
    x = 3

@app.route('/foo')
def foo():
    x = 1

def no_anno():
    x = 2


