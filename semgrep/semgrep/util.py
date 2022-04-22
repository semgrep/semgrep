import functools
import itertools
import logging
import operator
import os
import subprocess
import sys
from pathlib import Path
from typing import Any
from typing import Callable
from typing import FrozenSet
from typing import Iterable
from typing import List
from typing import Optional
from typing import Tuple
from typing import TypeVar
from urllib.parse import urlparse

import click

from semgrep.constants import Colors
from semgrep.constants import USER_LOG_FILE
from semgrep.constants import YML_SUFFIXES
from semgrep.constants import YML_TEST_SUFFIXES

T = TypeVar("T")

global FORCE_COLOR
FORCE_COLOR = False
global FORCE_NO_COLOR
FORCE_NO_COLOR = False

global VERBOSITY
VERBOSITY = logging.INFO


MAX_TEXT_WIDTH = 120


def is_quiet() -> bool:
    """
    Returns true if logging level is quiet or quieter (higher)
    (i.e. only critical logs surfaced)
    """
    return VERBOSITY >= logging.CRITICAL


def is_debug() -> bool:
    """
    Returns true if logging level is debug or noisier (lower)
    (i.e. want more logs)
    """
    return VERBOSITY <= logging.DEBUG


def is_url(url: str) -> bool:
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False


def set_flags(*, verbose: bool, debug: bool, quiet: bool, force_color: bool) -> None:
    """Set the relevant logging levels"""
    # Assumes only one of verbose, debug, quiet is True
    logger = logging.getLogger("semgrep")
    logger.handlers = []  # Reset to no handlers

    stdout_level = logging.INFO
    if verbose:
        stdout_level = logging.VERBOSE  # type: ignore[attr-defined]
    elif debug:
        stdout_level = logging.DEBUG
    elif quiet:
        stdout_level = logging.CRITICAL

    # Setup stdout logging
    stdout_handler = logging.StreamHandler()
    stdout_formatter = logging.Formatter("%(message)s")
    stdout_handler.setFormatter(stdout_formatter)
    stdout_handler.setLevel(stdout_level)
    logger.addHandler(stdout_handler)

    # Setup file logging
    # USER_LOG_FILE dir must exist
    USER_LOG_FILE.parent.mkdir(parents=True, exist_ok=True)
    file_handler = logging.FileHandler(USER_LOG_FILE, "w")
    file_formatter = logging.Formatter(
        "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    )
    file_handler.setLevel(logging.DEBUG)
    file_handler.setFormatter(file_formatter)
    logger.addHandler(file_handler)

    # Needs to be DEBUG otherwise will filter before sending to handlers
    logger.setLevel(logging.DEBUG)

    global VERBOSITY
    VERBOSITY = stdout_level

    global FORCE_COLOR
    if force_color or os.environ.get("SEMGREP_FORCE_COLOR") is not None:
        FORCE_COLOR = True

    global FORCE_NO_COLOR
    if (
        os.environ.get("NO_COLOR") is not None  # https://no-color.org/
        or os.environ.get("SEMGREP_FORCE_NO_COLOR") is not None
    ):
        FORCE_NO_COLOR = True


def partition(
    pred: Callable[[T], Any], iterable: Iterable[T]
) -> Tuple[List[T], List[T]]:
    """E.g. partition(is_odd, range(10)) -> 1 3 5 7 9  and  0 2 4 6 8"""
    i1, i2 = itertools.tee(iterable)
    return list(filter(pred, i1)), list(itertools.filterfalse(pred, i2))


def partition_set(
    pred: Callable[[T], Any], iterable: Iterable[T]
) -> Tuple[FrozenSet[T], FrozenSet[T]]:
    """E.g. partition(is_odd, range(10)) -> 1 3 5 7 9  and  0 2 4 6 8"""
    i1, i2 = itertools.tee(iterable)
    return frozenset(filter(pred, i1)), frozenset(itertools.filterfalse(pred, i2))


def abort(message: str) -> None:
    click.secho(message, fg="red", err=True)
    sys.exit(2)


def with_color(
    color: Colors,
    text: str,
    bgcolor: Optional[Colors] = None,
    bold: bool = False,
    underline: bool = False,
) -> str:
    """
    Wrap text in color & reset

    Use ANSI color names or 8 bit colors (24-bit is not well supported by terminals)
    In click bold always switches colors to their bright variant (if there is one)
    """
    if FORCE_NO_COLOR and not FORCE_COLOR:
        # for 'no color' there is a global env var (https://no-color.org/)
        # while the env var to 'force color' is semgrep-specific
        # so we let the more specific setting override the broader one
        return text
    if not sys.stderr.isatty() and not FORCE_COLOR:
        return text
    return click.style(
        text,
        fg=color.value,
        bg=(bgcolor.value if bgcolor is not None else None),
        underline=underline,
        bold=bold,
    )


def terminal_wrap(text: str) -> str:
    from shutil import get_terminal_size
    import textwrap

    paras = text.split("\n")
    terminal_size = get_terminal_size((MAX_TEXT_WIDTH, 1))[0]
    if terminal_size <= 0:
        terminal_size = MAX_TEXT_WIDTH
    width = min(MAX_TEXT_WIDTH, terminal_size)
    wrapped_paras = ["\n".join(textwrap.wrap(p, width)) for p in paras]
    return "\n".join(wrapped_paras)


def sub_check_output(cmd: List[str], **kwargs: Any) -> Any:
    """A simple proxy function to minimize and centralize subprocess usage."""
    if is_quiet():
        kwargs = {**kwargs, "stderr": subprocess.DEVNULL}

    # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
    return subprocess.check_output(cmd, **kwargs)


def manually_search_file(path: str, search_term: str, suffix: str) -> Optional[str]:
    """
    Searches a file for the given search term and, if found,
    returns the first word that contains that search term
    """
    if not os.path.isfile(path):
        return None
    with open(path) as fd:
        contents = fd.read()
        words = contents.split()
    # Find all of the individual words that contain the search_term
    matches = [w for w in words if search_term in w]
    return matches[0] + suffix if len(matches) > 0 else None


# TODO: seems dead
def listendswith(l: List[T], tail: List[T]) -> bool:
    """
    E.g.
        - listendswith([1, 2, 3, 4], [3, 4]) -> True
        - listendswith([1, 2, 3, 4], [1, 4]) -> False
    """
    if len(tail) > len(l):
        return False

    return all(l[len(l) - len(tail) + i] == tail[i] for i in range(len(tail)))


def is_config_suffix(path: Path) -> bool:
    return any(
        listendswith(path.suffixes, suffixes) for suffixes in YML_SUFFIXES
    ) and not is_config_test_suffix(path)


def is_config_test_suffix(path: Path) -> bool:
    return any(listendswith(path.suffixes, suffixes) for suffixes in YML_TEST_SUFFIXES)


def format_bytes(num: float) -> str:
    for unit in ["", "K", "M", "G", "T", "P", "E", "Z"]:
        if abs(num) < 1024.0:
            return "%3d%sB" % (num, unit)
        num /= 1024.0
    return "{:.1f}{}B".format(num, "Y")


def truncate(file_name: str, col_lim: int) -> str:
    name_len = len(file_name)
    prefix = "..."
    if name_len > col_lim:
        file_name = prefix + file_name[name_len - col_lim + len(prefix) :]
    return file_name


def flatten(some_list: List[List[T]]) -> List[T]:
    return functools.reduce(operator.iconcat, some_list, [])


PathFilterCallable = Callable[..., FrozenSet[Path]]


def unit_str(count: int, unit: str, pad: bool = False) -> str:
    if count != 1:
        unit += "s"
    elif pad:
        unit += " "

    return f"{count} {unit}"
