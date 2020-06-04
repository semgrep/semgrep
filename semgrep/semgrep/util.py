import itertools
import sys
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


def is_url(url: str) -> bool:
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False


def print_error(e: str) -> None:
    if not QUIET:
        print(e, file=sys.stderr)


def print_msg(msg: str, **kwargs: Any) -> None:
    if not QUIET:
        print(msg, file=sys.stderr, **kwargs)


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
