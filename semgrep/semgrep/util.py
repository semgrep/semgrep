import itertools
import re
import sys
import typing
from typing import Any
from typing import Callable
from typing import IO
from typing import Iterable
from typing import List
from typing import Set
from typing import Tuple
from typing import TypeVar
from urllib.parse import urlparse

from colorama import Fore
from tqdm import tqdm

T = TypeVar("T")

global DEBUG
global QUIET
global FORCE_COLOR
DEBUG = False
QUIET = False
FORCE_COLOR = False


def is_url(url: str) -> bool:
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False


ANSI_ESCAPE = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")


def tty_sensitive_print(msg: str, file: typing.IO, **kwargs: Any) -> None:
    """
    Strip ANSI escape sequences before printing, if `file` is not a TTY
    """
    if not file.isatty() and not FORCE_COLOR:
        msg = ANSI_ESCAPE.sub("", msg)
    print(msg, file=file, **kwargs)


def print_error(e: str) -> None:
    if not QUIET:
        tty_sensitive_print(e, file=sys.stderr)


def print_msg(msg: str, **kwargs: Any) -> None:
    if not QUIET:
        tty_sensitive_print(msg, file=sys.stderr, **kwargs)


def debug_print(msg: str) -> None:
    if DEBUG:
        tty_sensitive_print(msg, file=sys.stderr)


def debug_tqdm_write(msg: str, file: IO = sys.stderr) -> None:
    if DEBUG:
        tqdm.write(msg, file=file)


def flatten(L: Iterable[Iterable[Any]]) -> Iterable[Any]:
    for list in L:
        for item in list:
            yield item


def set_flags(debug: bool, quiet: bool, force_color: bool) -> None:
    """Set the global DEBUG and QUIET flags"""
    # TODO move to a proper logging framework
    global DEBUG
    global QUIET
    global FORCE_COLOR
    if debug:
        DEBUG = True
        debug_print("DEBUG is on")
    if quiet:
        QUIET = True
        debug_print("QUIET is on")

    if force_color:
        FORCE_COLOR = True
        debug_print("Output will use ANSI escapes, even if output is not a TTY")


def partition(pred: Callable, iterable: Iterable) -> Tuple[List, List]:
    """E.g. partition(is_odd, range(10)) -> 1 3 5 7 9  and  0 2 4 6 8"""
    i1, i2 = itertools.tee(iterable)
    return list(filter(pred, i1)), list(itertools.filterfalse(pred, i2))


def partition_set(pred: Callable, iterable: Iterable) -> Tuple[Set, Set]:
    """E.g. partition(is_odd, range(10)) -> 1 3 5 7 9  and  0 2 4 6 8"""
    i1, i2 = itertools.tee(iterable)
    return set(filter(pred, i1)), set(itertools.filterfalse(pred, i2))


def with_color(color: str, text: str, bold: bool = False) -> str:
    """
    Wrap text in color & reset
    """
    reset = Fore.RESET
    if bold:
        color = color + "\033[1m"
        reset += "\033[0m"
    return f"{color}{text}{reset}"


def progress_bar(
    iterable: Iterable[T], file: IO = sys.stderr, **kwargs: Any
) -> Iterable[T]:
    """
    Return tqdm-wrapped iterable if output stream is a tty;
    else return iterable without tqdm.
    """
    # Conditions:
    # file.isatty() - only show bar if this is an interactive terminal
    # len(iterable) > 1 - don't show progress bar when using -e on command-line. This
    #   is a hack, so it will only show the progress bar if there is more than 1 rule to run.
    # not DEBUG - don't show progress bar with debug
    # not QUIET - don't show progress bar with quiet
    listified = list(
        iterable
    )  # Consume iterable once so we can check length and then use in tqdm.
    if file.isatty() and len(listified) > 1 and not DEBUG and not QUIET:
        # mypy doesn't seem to want to follow tqdm imports. Do this to placate.
        wrapped: Iterable[T] = tqdm(listified, file=file, **kwargs)
        return wrapped
    return listified
