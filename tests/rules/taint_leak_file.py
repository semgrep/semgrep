store = dict([])

def bad():
    #ruleid: test
    file = open("test.txt")
    content = file.read()
    print(content)

def ok1():
    #ok: test
    file = open("test.txt")
    content = file.read()
    print(content)
    file.close()

def bad():
    #ok: test
    file = open("test.txt")
    content = file.read()
    store['file'] = file
    print(content)
