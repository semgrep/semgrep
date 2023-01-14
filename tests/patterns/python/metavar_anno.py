# ERROR:
@anno1
def bar(foo: "foo"):
    pass


# ERROR:
@anno1
@anno2
def foobar():
    pass


@app.route("/foo")
def foo():
    pass


def no_anno():
    pass
