#!/usr/bin/env python3
import sys

from semgrep.cli import cli
from semgrep.error import SemgrepException
from semgrep.util import print_error


def main() -> None:
    try:
        cli()
    # Catch custom exceptions, output the right message and exit.
    # Note: this doesn't catch all Exceptions and lets them bubble up.
    except SemgrepException as e:
        if e.msg:
            print_error(e.msg)
        sys.exit(e.code)


if __name__ == "__main__":
    main()
