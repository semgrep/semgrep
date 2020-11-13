import subprocess
import subprocess as sub

# ok
x = sub.call("foo")

# ruleid: unchecked-subprocess-call
sub.call("foo")

# OK
sub.check_call("foo")

# OK
sub.check_call("foo ") == 0


def foo():
    # ruleid: unchecked-subprocess-call
    return subprocess.call(["ls", "--no"])


def foo2():
    return subprocess.call(["ls", "--no"]) == 0
