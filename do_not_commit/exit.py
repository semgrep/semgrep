import sys

if False:
    # ok
    sys.exit(2)

if True:
    # ruleid: use-sys-exit
    exit(3)

def check_db(user):
    if user is None:
        # ruleid: use-sys-exit
        exit(4)
    else:
        print(user)
        # ok
        sys.exit(0)