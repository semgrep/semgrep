def foo():
    f = open()
    f.write("bar")
    f.close()
    # ruleid:
    f.close()
    # f = open()
    # f.write("baz")
