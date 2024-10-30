def test():
    y = x
    z = bar(y)
    #ruleid: test
    run(lambda: foo(z))
