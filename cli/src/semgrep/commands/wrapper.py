import sys
from functools import wraps
from typing import Any
from typing import Callable
from typing import NoReturn

from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.state import get_state
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
    def wrapper(*args: Any, **kwargs: Any) -> NoReturn:
        # When running semgrep as a command line tool
        # silence root level logger otherwise logs higher
        # than warning are handled twice
        logger = getLogger("semgrep")
        logger.propagate = False

        try:
            func(*args, **kwargs)
        # Catch custom exception, output the right message and exit
        except SemgrepError as e:
            exit_code = e.code
        except Exception as e:
            logger.exception(e)
            exit_code = FATAL_EXIT_CODE
        except SystemExit as e:
            exit_code = e.code
        except:  # noqa: B001
            exit_code = FATAL_EXIT_CODE
        else:
            exit_code = 0
        finally:
            metrics = get_state().metrics
            metrics.add_exit_code(exit_code)
            metrics.send()

            error_handler = get_state().error_handler
            exit_code = error_handler.send(exit_code)

        # not inside the finally block to avoid overriding other sys.exits
        sys.exit(exit_code)

    return wrapper
