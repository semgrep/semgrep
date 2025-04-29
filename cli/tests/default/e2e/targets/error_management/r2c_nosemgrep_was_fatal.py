def foo1():
    stuff()

def foo2():
    # nosemgrep: stuff
    stuff()
    
def foo3():
    # nosemgrep: nosemgrep_r_2_c_was_fatal
    stuff()
