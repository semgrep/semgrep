#! /bin/env python

"""Print the "directory blah" statements necessary for ocamldebug
   to find all of the source files under a given root directory.
   We could feasibly just generate the commandline ('-I') parameters,
   but if we have many directories, we might hit the max command
   length for the shell.

   One relatively pain-free way to use this is to add something
   like the following to your .emacs:

      (global-set-key "\C-ci"
        (lambda () (interactive)
          (kill-new (shell-command-to-string
            "/path/to/get_includes.py /path/to/pfff"))))
"""

import os
import sys

def checkargs():
    usage = "Usage: get_includes.py debug|toplevel root_dir\n"
    if len(sys.argv) <> 3:
        print usage
        exit(-1)

    if sys.argv[1] not in [ "debug", "toplevel" ]:
        print usage
        exit(-1)

def removenothrow(l, item):
    """Remove a specified item from a list; don't do anything
       if the item doesn't exist. """
    try:
        l.remove(item)
    except:
        pass


def getdirs(rootdir):
    for root, dirs, files in os.walk(rootdir):
        # add excludes here as needed
        removenothrow(dirs, ".git")
        removenothrow(dirs, "mini_www")
        removenothrow(dirs, "docs")

        for d in dirs:
            yield "%s" % (os.path.join(root, d),)


if __name__ == "__main__":
    checkargs()
    mode = sys.argv[1]
    rootdir = sys.argv[2]
    if mode == "debug":
        print "".join(["directory %s\n" % (d,) for d in getdirs(rootdir)])
    else:
        print "".join(["#directory \"%s\";;\n" % (d,) for d in getdirs(rootdir)])
