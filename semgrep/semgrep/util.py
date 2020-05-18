import itertools
import os
import re
import sys
import typing
from typing import Any
from typing import Callable
from typing import Iterable
from typing import List
from typing import Tuple
from urllib.parse import urlparse

global DEBUG
global QUIET
DEBUG = False
QUIET = False

# Exit codes
FINDINGS_EXIT_CODE = 1
FATAL_EXIT_CODE = 2
INVALID_CODE_EXIT_CODE = 3
INVALID_PATTERN_EXIT_CODE = 4
UNPARSEABLE_YAML_EXIT_CODE = 5
NEED_ARBITRARY_CODE_EXEC_EXIT_CODE = 6
MISSING_CONFIG_EXIT_CODE = 7


def is_url(url: str) -> bool:
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False


def tty_sensitive_print(msg: str, file: typing.IO, **kwargs: Any) -> None:
    """
    Strip ANSI escape sequences before printing, if `file` is not a TTY
    """
    if not file.isatty():
        ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
        msg = ansi_escape.sub("", msg)
    print(msg, file=file, **kwargs)


def print_error(e: str) -> None:
    if not QUIET:
        tty_sensitive_print(e, file=sys.stderr)


def print_error_exit(msg: str, exit_code: int = FATAL_EXIT_CODE) -> None:
    if not QUIET:
        tty_sensitive_print(msg, file=sys.stderr)
    sys.exit(exit_code)


def print_msg(msg: str, **kwargs: Any) -> None:
    if not QUIET:
        tty_sensitive_print(msg, file=sys.stderr, **kwargs)


def debug_print(msg: str) -> None:
    if DEBUG:
        print(msg, file=sys.stderr)


def flatten(L: Iterable[Iterable[Any]]) -> Iterable[Any]:
    for list in L:
        for item in list:
            yield item


def set_flags(debug: bool, quiet: bool) -> None:
    """Set the global DEBUG and QUIET flags"""
    # TODO move to a proper logging framework
    global DEBUG
    global QUIET
    if debug:
        DEBUG = True
        debug_print("DEBUG is on")
    if quiet:
        QUIET = True
        debug_print("QUIET is on")


def partition(pred: Callable, iterable: Iterable) -> Tuple[List, List]:
    """E.g. partition(is_odd, range(10)) -> 1 3 5 7 9  and  0 2 4 6 8"""
    i1, i2 = itertools.tee(iterable)
    return list(filter(pred, i1)), list(itertools.filterfalse(pred, i2))
