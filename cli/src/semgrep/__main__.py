#!/usr/bin/env python3
import sys

from semgrep.main import main

# NOTE: This is the entrypoint for the `pysemgrep` command.
# To match the program usage help between pysemgrep (legacy)
# and osemgrep (new) – and to hide complexity for our users -
# here we specify `semgrep` as the program name for pysemgrep.
#
# This entrypoint is deprecated and may be removed in the future.
#

if __name__ == "__main__":
    sys.exit(main())
