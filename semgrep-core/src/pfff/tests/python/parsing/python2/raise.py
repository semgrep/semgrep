def foo():
    pass

try:
    foo()
except Exception, ex:
    print ex

try:
    foo()
except Exception as ex:
    print ex

try:
    foo()
except:
    raise Exception, "message", sys.exc_info()[2]
