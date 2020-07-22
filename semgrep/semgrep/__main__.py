#!/usr/bin/env python3
import logging.handlers
import sys

from semgrep.cli import cli
from semgrep.error import OK_EXIT_CODE
from semgrep.error import SemgrepError


def main() -> int:
    # When running semgrep as a command line tool
    # silence root level logger otherwise logs higher
    # than warning are handled twice
    logger = logging.getLogger("semgrep")
    logger.propagate = False
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
