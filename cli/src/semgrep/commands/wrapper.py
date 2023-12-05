import sys
from functools import wraps
from typing import Any
from typing import Callable
from typing import NoReturn
from typing import Optional

from semgrep import state
from semgrep.error import FATAL_EXIT_CODE
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
    def wrapper(*args: Any, **kwargs: Any) -> NoReturn:
        # When running semgrep as a command line tool
        # silence root level logger otherwise logs higher
        # than warning are handled twice
        logger = getLogger("semgrep")
        logger.propagate = False
        exc: Optional[BaseException] = None

        try:
            func(*args, **kwargs)
        # Catch custom exception, output the right message and exit
        except SemgrepError as e:
            exit_code = e.code
            exc = e
        except Exception as e:  # noqa: W0718
            logger.exception(e)
            exit_code = FATAL_EXIT_CODE
            exc = e
        except SystemExit as e:
            if e.code is None:
                exit_code = 0
            elif isinstance(e.code, str):
                exit_code = FATAL_EXIT_CODE
            else:
                exit_code = e.code
            exc = e
        except BaseException as e:  # noqa: W0718
            exit_code = FATAL_EXIT_CODE
            exc = e
        else:
            exit_code = 0
        finally:
            metrics = state.get_state().metrics
            metrics.add_exit_code(exit_code)
            metrics.send()

            error_handler = state.get_state().error_handler
            error_handler.capture_error(exc)
            exit_code = error_handler.send(exit_code)

        # not inside the finally block to avoid overriding other sys.exits
        sys.exit(exit_code)

    return wrapper
