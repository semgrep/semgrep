# cf. https://github.com/PyCQA/bandit/blob/d5f8fa0d89d7b11442fc6ec80ca42953974354c8/examples/telnetlib.py

import telnetlib
import getpass

host = sys.argv[1]

username = raw_input('Username:')
password = getpass.getpass()
# ruleid:telnetlib
tn = telnetlib.Telnet(host)

tn.read_until("login: ")
tn.write(username + "\n")
if password:
    tn.read_until("Password: ")
    tn.write(password + "\n")

tn.write("ls\n")
tn.write("exit\n")

print(tn.read_all())