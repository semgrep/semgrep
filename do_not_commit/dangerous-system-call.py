import os

# ok
os.system("ls -al")

# ok
os.popen("cat contents.txt")

from somewhere import something

# ruleid:dangerous-system-call
os.system(something())

# ruleid:dangerous-system-call
os.popen(something())

# ruleid:dangerous-system-call
os.popen2(something())