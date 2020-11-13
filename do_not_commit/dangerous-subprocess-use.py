# cf. https://github.com/returntocorp/semgrep/blob/develop/docs/writing_rules/examples.md#auditing-dangerous-function-use

import subprocess
import sys

# ok
subprocess.call("echo 'hello'")

# ok
subprocess.call(["echo", "a", ";", "rm", "-rf", "/"])

# ruleid:dangerous-subprocess-use
subprocess.call("grep -R {} .".format(sys.argv[1]))

# ruleid:dangerous-subprocess-use
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True)

# ruleid:dangerous-subprocess-use
subprocess.call("grep -R {} .".format(sys.argv[1]), shell=True, cwd="/home/user")

# ruleid:dangerous-subprocess-use
subprocess.run("grep -R {} .".format(sys.argv[1]), shell=True)
