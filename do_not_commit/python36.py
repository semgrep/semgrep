import ssl as s2
import subprocess as s1

# ruleid:python36-compatibility-ssl
s2.get_ciphers()

def main():
    # ruleid:python36-compatibility-Popen2
    subprocess.Popen(cmd, encoding="utf-8")
    # ruleid:python36-compatibility-Popen1
    subprocess.Popen(cmd, errors=None)
