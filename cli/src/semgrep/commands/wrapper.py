#
# Copyright (c) 2022-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import logging
import sys
import time
import uuid
from functools import wraps
from typing import Any
from typing import Callable
from typing import NoReturn
from typing import Optional

from structlog.contextvars import bind_contextvars
from structlog.contextvars import clear_contextvars
from structlog.contextvars import get_contextvars

from semgrep import state
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import is_semgrep_error_already_reported
from semgrep.error import mark_semgrep_error_as_reported
from semgrep.error import SemgrepError
from semgrep.structured_logging import emit_error
from semgrep.structured_logging import log
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
        previous_context = get_contextvars()
        try:
            # When running semgrep as a command line tool
            # semgrep logger propagates to root logger where handlers are attached
            logger = getLogger("semgrep")
            exc: Optional[BaseException] = None
            started = time.monotonic()
            bind_contextvars(
                **{
                    "command.name": func.__name__,
                    "command.id": str(uuid.uuid4()),
                    "scan.phase": "current",
                }
            )
            log.info("command started")

            try:
                func(*args, **kwargs)
            # Catch custom exception, output the right message and exit
            except SemgrepError as e:
                if not is_semgrep_error_already_reported(e):
                    logger.error(
                        e.format_for_terminal(), extra={"structured_skip": True}
                    )
                    emit_error(e, message="command failed")
                    mark_semgrep_error_as_reported(e)
                exit_code = e.code
                exc = e
            except Exception as e:  # noqa: W0718
                logger.exception(e, extra={"structured_skip": True})
                log.exception(
                    "command failed",
                    **{
                        "error.type": type(e).__name__,
                        "error.code": FATAL_EXIT_CODE,
                        "error.message": str(e),
                    },
                )
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
            except BaseException as e:  # noqa: W0718, B036
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

            log.log(
                logging.ERROR if exit_code not in (0, 1) else logging.INFO,
                "command completed",
                **{
                    "exit.code": exit_code,
                    "duration.ms": round((time.monotonic() - started) * 1000),
                },
            )
            # not inside the finally block to avoid overriding other sys.exits
            sys.exit(exit_code)
        finally:
            clear_contextvars()
            bind_contextvars(**previous_context)

    return wrapper
