#!/usr/bin/env python3
import sys

from semgrep.cli import cli
from semgrep.error import SemgrepError
from semgrep.util import print_error


def main() -> None:
    try:
        cli()
    # Catch custom exceptions, output the right message and exit.
    # Note: this doesn't catch all Exceptions and lets them bubble up.
    except SemgrepError as e:
        print_error(str(e))
        sys.exit(e.code)


if __name__ == "__main__":
    main()
