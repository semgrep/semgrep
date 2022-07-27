def foo():
    f = open()
    f.write("a")
    f.close()
    #ruleid: test
    f.write("b")

def bar():
    f = open()
    f.write("a")
    f.close()
    f = open()
    #ok: test
    f.write("b")
