def f():
    i = 0
    while (i < 0):
        if i == 4:
            #ERROR:
            sink(source)
            break
            #OK:
            sink(source)
        i += 1