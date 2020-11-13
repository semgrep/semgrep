import time as t


def a():
    return 10


# OK
t.sleep

# ruleid:arbitrary-sleep
t.sleep(5)
# ruleid:arbitrary-sleep
t.sleep(0.1)
# todoruleid:arbitrary-sleep
time.sleep("bad")

# OK
t.sleep(a())

# ok
t.sleep(some_var)
