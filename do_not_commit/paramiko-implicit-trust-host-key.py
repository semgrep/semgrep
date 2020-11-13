from paramiko import client

ssh_client = client.SSHClient()
# ruleid:paramiko-implicit-trust-host-key
ssh_client.set_missing_host_key_policy(client.AutoAddPolicy())
# ruleid:paramiko-implicit-trust-host-key
ssh_client.set_missing_host_key_policy(client.WarningPolicy())
# ok
ssh_client.set_missing_host_key_policy(client.RejectPolicy())