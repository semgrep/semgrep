def wrapper():
    return source1()

a = wrapper()
if True:
    b = a
    b = sanitize1()
else:
    b = a
#ERROR:
sink1(b)
