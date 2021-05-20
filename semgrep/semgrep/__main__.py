#!/usr/bin/env python3
import sys

from semgrep import __VERSION__
from semgrep.cli import cli
from semgrep.error import OK_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.metric_manager import metric_manager
from semgrep.verbose_logging import getLogger


def main() -> int:
    # When running semgrep as a command line tool
    # silence root level logger otherwise logs higher
    # than warning are handled twice
    logger = getLogger("semgrep")
    logger.propagate = False
    metric_manager.set_version(__VERSION__)
    try:
        cli()
    # Catch custom exceptions, output the right message and exit.
    # Note: this doesn't catch all Exceptions and lets them bubble up.
    except SemgrepError as e:
        metric_manager.set_return_code(e.code)
        return e.code
    else:
        metric_manager.set_return_code(OK_EXIT_CODE)
        return OK_EXIT_CODE
    finally:
        metric_manager.send()


if __name__ == "__main__":
    sys.exit(main())
