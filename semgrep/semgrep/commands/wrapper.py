import sys
from functools import wraps
from typing import Any
from typing import Callable

from semgrep import __VERSION__
from semgrep.app.metrics import metric_manager
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import OK_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.verbose_logging import getLogger


def handle_command_errors(func: Callable) -> Callable:
    """
    Adds the following functionality to our subcommands:
    - Enforces that exit code 1 is only for findings
    - Sets global logging level
    - Handles metric sending before exit

    This needed to be done in a decorator so testing
    using click.CliRunner would include this functionality.
    In particular putting this in __main__ (outside click),
    would have been harder to test
    """

    @wraps(func)
    def wrapper(*args: Any, **kwargs: Any) -> None:
        # When running semgrep as a command line tool
        # silence root level logger otherwise logs higher
        # than warning are handled twice
        logger = getLogger("semgrep")
        logger.propagate = False
        metric_manager.set_version(__VERSION__)

        try:
            func(*args, **kwargs)
        # Catch custom exception, output the right message and exit
        except SemgrepError as e:
            metric_manager.set_return_code(e.code)
            sys.exit(e.code)
        except Exception as e:
            logger.exception(e)
            metric_manager.set_return_code(FATAL_EXIT_CODE)
            sys.exit(FATAL_EXIT_CODE)
        else:
            metric_manager.set_return_code(OK_EXIT_CODE)
            sys.exit(OK_EXIT_CODE)
        finally:
            metric_manager.send()

    return wrapper
