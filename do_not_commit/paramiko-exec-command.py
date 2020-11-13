# cf. https://github.com/PyCQA/bandit/blob/d5f8fa0d89d7b11442fc6ec80ca42953974354c8/examples/paramiko_injection.py

import paramiko
from paramiko import client


client = paramiko.client.SSHClient()
# this is safe
# ok
client.connect('somehost')

# this is not safe
# ruleid:paramiko-exec-command
client.exec_command('something; really; unsafe')





client2 = client.SSHClient()
client2.connect("somewhere-out-there")
# ruleid:paramiko-exec-command
client2.exec_command("ls -r /")