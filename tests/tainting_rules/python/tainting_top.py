a = source1()
if True:
    b = a
    b = sanitize1()
else:
    b = a
#ruleid: tainting
sink1(b)
