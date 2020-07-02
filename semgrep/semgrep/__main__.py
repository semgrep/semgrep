#!/usr/bin/env python3
import logging.handlers
import sys

from semgrep.cli import cli
from semgrep.error import OK_EXIT_CODE
from semgrep.error import SemgrepError

formatter = logging.Formatter("%(message)")
handler = logging.StreamHandler()
handler.setLevel(logging.INFO)
logger = logging.getLogger("semgrep")
logger.addHandler(handler)
logger.setLevel(logging.INFO)


def main() -> int:
    try:
        cli()
    # Catch custom exceptions, output the right message and exit.
    # Note: this doesn't catch all Exceptions and lets them bubble up.
    except SemgrepError as e:
        return e.code
    else:
        return OK_EXIT_CODE


if __name__ == "__main__":
    sys.exit(main())
