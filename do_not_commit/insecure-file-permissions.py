import os

# ruleid:insecure-file-permissions
os.chmod("file", 0o777)
# ruleid:insecure-file-permissions
os.chmod("file", 511)
# ruleid:insecure-file-permissions
os.chmod("file", 0x1ff)
# ruleid:insecure-file-permissions
os.chmod("file", 0o776)
# ruleid:insecure-file-permissions
os.chmod("file", 0o775)
# ruleid:insecure-file-permissions
os.chmod("file", 0o774)
# ruleid:insecure-file-permissions
os.chmod("file", 0o767)
# ruleid:insecure-file-permissions
os.chmod("file", 0o757)
# ruleid:insecure-file-permissions
os.chmod("file", 0o747)
# ruleid:insecure-file-permissions
os.chmod("file", 0o654)
# ruleid:insecure-file-permissions
os.chmod("file", 0o100777)
# ruleid:insecure-file-permissions
os.chmod("file", 0o100775)
# ruleid:insecure-file-permissions
os.chmod("file", 0o100774)
# ruleid:insecure-file-permissions
os.chmod("file", 0o100767)
# ruleid:insecure-file-permissions
os.lchmod("file", 0o747)
# ruleid:insecure-file-permissions
os.lchmod("file", 0o100777)
f = open("file", 'w')
# ruleid:insecure-file-permissions
os.fchmod(f, 0o654)
# ruleid:insecure-file-permissions
os.fchmod(f, 0o100775)

import stat
# ruleid:insecure-file-permissions
os.chmod("file", stat.S_IRWXU | stat.S_IRGRP | stat.S_IROTH | stat.S_IWOTH | stat.S_IXOTH)

# Try to inject Semgrep. Perms end up OK.
# ok
os.chmod("file", stat.S_IRWXU | print("GOTCHA"))

# Try to inject Semgrep.
# ruleid:insecure-file-permissions
os.chmod("file", stat.S_IRWXO | print("GOTCHA"))

def ensure_exec_perms(file_):
    st = os.stat(file_)
    # ruleid:insecure-file-permissions
    os.chmod(file_, st.st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return file_

def ensure_exec_perms2(file_):
    st = os.stat(file_)
    # ruleid:insecure-file-permissions
    os.chmod(file_, st.st_mode | 0o111)
    return file_

# ok
os.chmod("file", 0o644)
# ok
os.chmod("file", 0o444)
# ok
os.chmod("file", stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH)
# ok
os.chmod("file", stat.S_IRWXU)