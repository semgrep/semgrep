def test1():
    # ruleid:hardcoded-tmp-path
    f = open("/tmp/blah.txt", 'w')
    f.write("hello world")
    f.close()

def test2():
    # ruleid:hardcoded-tmp-path
    f = open("/tmp/blah/blahblah/blah.txt", 'r')
    data = f.read()
    f.close()

def test3():
    # ok
    f = open("./tmp/blah.txt", 'w')
    f.write("hello world")
    f.close()

def test3a():
    # ok
    f = open("/var/log/something/else/tmp/blah.txt", 'w')
    f.write("hello world")
    f.close()

def test4():
    # ruleid:hardcoded-tmp-path
    with open("/tmp/blah.txt", 'r') as fin:
        data = fin.read()

def test5():
    # ok
    with open("./tmp/blah.txt", 'w') as fout:
        fout.write("hello world")
