import pdb as db


def foo():
    # ruleid:pdb-remove
    db.set_trace()
    # ok
    a = "apple"
    #ok
    db = "the string, not the library"
    #ok
    pdb = "also a string"
    # ruleid:pdb-remove
    pdb.Pdb.set_trace()
    # ruleid:pdb-remove
    db.Pdb.set_trace(...)