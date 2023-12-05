def test1(l):
    lock(l)
    unlock(l)
    #ok: test
    lock(l)
    #ruleid: test
    lock(l)

def test2(obj):
    lock(obj.l)
    unlock(obj.l)
    # ok: test
    lock(obj.l)
    # ruleid: test
    lock(obj.l)

def test3(obj):
    lock(obj[0])
    unlock(obj[0])
    # ok: test
    lock(obj[0])
    # ruleid: test
    lock(obj[0])