import os

# ruleid:python37-compatability-os-module
os.pwrite('a')

if hasattr(os, 'pwrite'):
    # OK
    os.pwrite('a')


if hasattr(os, 'pwritev'):
    # OK
    os.pwritev('a')


# ruleid:python37-compatibility-os2-ok2
os.pwritev('b')
