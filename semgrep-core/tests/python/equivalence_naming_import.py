import subprocess.open
from subprocess import open as sub_open

import subprocess as sub


# TODO
# import os as subprocess


def foo():
    # ERROR:
    result = subprocess.open("ls")
    # ERROR:
    result = sub_open("ls")
    # ERROR:
    result = sub.open("ls")

    result = sub.not_open("ls")
