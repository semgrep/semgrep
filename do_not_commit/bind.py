#import socket as s0
#import socket.socket as s1
#from socket import socket as s1

import socket

mys = s1(doesnt, matter)
# todoruleid:avoid-bind-to-all-interfaces
s1.bind(('', 1337))

theirs = s1(doesnt, matter)

# todoruleid:avoid-bind-to-all-interfaces
theirs.bind(('0.0.0.0', 1337))

# ok
theirs.bind(('8.8.8.8', 1337))

# won't work, shouldn't fire
theirs.bind('', 1337)

# easy

#  (TODO-false positive)
# ruleid:avoid-bind-to-all-interfaces
easy_s = socket.socket(doesnt, matter)
easy_s.bind()
# todoruleid:avoid-bind-to-all-interfaces
easy_s.bind(("", 1337))
