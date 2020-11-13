import os

# ok
os.execl("ls")

# ok
os.spawnlp(os.P_WAIT, "ls")

# ok
os.spawnlpe(os.P_WAIT, "ls")

# ok
os.spawnv(os.P_WAIT, "/bin/ls")

# ok
os.spawnve(os.P_WAIT, "/bin/ls", ["-a"], os.environ)

from somewhere import something

# ruleid:dangerous-spawn-process
os.execl(something())

# ruleid:dangerous-spawn-process
os.spawnlp(os.P_WAIT, something())

# ruleid:dangerous-spawn-process
os.spawnlpe(os.P_WAIT, something())

# ruleid:dangerous-spawn-process
os.spawnv(os.P_WAIT, something())

# ruleid:dangerous-spawn-process
os.spawnve(os.P_WAIT, something(), ["-a"], os.environ)