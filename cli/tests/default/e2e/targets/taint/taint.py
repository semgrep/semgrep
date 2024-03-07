def foo():
    x = 'test string format {}'.format('foo')
    a = source1()
    if True:
        b = a
        b = sanitize1()
    else:
        b = a
    sink1(b)
