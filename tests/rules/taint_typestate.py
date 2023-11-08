def test():
    lock(l)
    unlock(l)
    #ok: test
    lock(l)
    #ruleid: test
    lock(l)
