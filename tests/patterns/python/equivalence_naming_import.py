import subprocess as sub
import subprocess.open
from subprocess import open as sub_open


# TODO
# import os as subprocess


def foo():
    # ERROR:
    subprocess.open("ls")
    # ERROR:
    sub_open("ls")
    # ERROR:
    sub.open("ls")

    sub.not_open("ls")
