import sys
from urllib.parse import urlparse
from typing import Iterable, Any, Optional

global DEBUG
global QUIET
DEBUG = False
QUIET = False

FATAL_EXIT_CODE = 2


def is_url(url: str) -> bool:
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False


def print_error(e):
    if not QUIET:
        print(str(e), file=sys.stderr)


def print_error_exit(msg: str, exit_code: int = FATAL_EXIT_CODE) -> None:
    if not QUIET:
        print(msg, file=sys.stderr)
    sys.exit(exit_code)


def print_msg(msg: str):
    if not QUIET:
        print(msg, file=sys.stderr)


def debug_print(msg: str):
    if DEBUG:
        print(msg, file=sys.stderr)


def flatten(L: Iterable[Iterable[Any]]) -> Iterable[Any]:
    for list in L:
        for item in list:
            yield item
