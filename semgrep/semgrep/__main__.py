#!/usr/bin/env python3
import sys

from semgrep.cli import cli
from semgrep.error import OK_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.util import print_error


def main() -> int:
    try:
        cli()
    # Catch custom exceptions, output the right message and exit.
    # Note: this doesn't catch all Exceptions and lets them bubble up.
    except SemgrepError as e:
        print_error(str(e))
        return e.code
    else:
        return OK_EXIT_CODE


if __name__ == "__main__":
    sys.exit(main())
