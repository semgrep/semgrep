import itertools
import logging
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any
from typing import Callable
from typing import IO
from typing import Iterable
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple
from typing import TypeVar
from urllib.parse import urlparse

import pkg_resources
from colorama import Fore
from tqdm import tqdm

from semgrep.constants import YML_SUFFIXES
from semgrep.constants import YML_TEST_SUFFIXES

T = TypeVar("T")

global FORCE_COLOR
FORCE_COLOR = False


def is_quiet() -> bool:
    """
    Returns true if logging level is quiet or quieter (higher)
    (i.e. only critical logs surfaced)
    """
    return logging.getLogger("semgrep").getEffectiveLevel() >= logging.CRITICAL


def is_debug() -> bool:
    """
    Returns true if logging level is debug or noisier (lower)
    (i.e. want more logs)
    """
    return logging.getLogger("semgrep").getEffectiveLevel() <= logging.DEBUG


def is_url(url: str) -> bool:
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False


def debug_tqdm_write(msg: str, file: IO = sys.stderr) -> None:
    if is_debug():
        tqdm.write(msg, file=file)


def set_flags(verbose: bool, debug: bool, quiet: bool, force_color: bool) -> None:
    """Set the relevant logging levels"""
    # Assumes only one of verbose, debug, quiet is True

    logger = logging.getLogger("semgrep")
    logger.handlers = []
    handler = logging.StreamHandler()
    formatter = logging.Formatter("%(message)s")
    handler.setFormatter(formatter)

    level = logging.INFO
    if verbose:
        level = logging.VERBOSE  # type: ignore[attr-defined]
    elif debug:
        level = logging.DEBUG
    elif quiet:
        level = logging.CRITICAL

    handler.setLevel(level)
    logger.addHandler(handler)
    logger.setLevel(level)

    global FORCE_COLOR
    if force_color:
        FORCE_COLOR = True


def partition(pred: Callable, iterable: Iterable) -> Tuple[List, List]:
    """E.g. partition(is_odd, range(10)) -> 1 3 5 7 9  and  0 2 4 6 8"""
    i1, i2 = itertools.tee(iterable)
    return list(filter(pred, i1)), list(itertools.filterfalse(pred, i2))


def partition_set(pred: Callable, iterable: Iterable) -> Tuple[Set, Set]:
    """E.g. partition(is_odd, range(10)) -> 1 3 5 7 9  and  0 2 4 6 8"""
    i1, i2 = itertools.tee(iterable)
    return set(filter(pred, i1)), set(itertools.filterfalse(pred, i2))


# cf. https://docs.python.org/3/library/itertools.html#itertools-recipes
def powerset(iterable: Iterable) -> Iterable[Tuple[Any, ...]]:
    """powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"""
    s = list(iterable)
    return itertools.chain.from_iterable(
        itertools.combinations(s, r) for r in range(len(s) + 1)
    )


def with_color(color: str, text: str, bold: bool = False) -> str:
    """
    Wrap text in color & reset
    """
    if not sys.stderr.isatty() and not FORCE_COLOR:
        return text

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
    # not QUIET - don't show progress bar with quiet
    listified = list(
        iterable
    )  # Consume iterable once so we can check length and then use in tqdm.
    if file.isatty() and len(listified) > 1 and not is_quiet() and not is_debug():
        # mypy doesn't seem to want to follow tqdm imports. Do this to placate.
        wrapped: Iterable[T] = tqdm(listified, file=file, **kwargs)
        return wrapped
    return listified


def sub_run(cmd: List[str], **kwargs: Any) -> Any:
    """A simple proxy function to minimize and centralize subprocess usage."""
    # fmt: off
    result = subprocess.run(cmd, **kwargs)  # nosem: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
    # fmt: on
    return result


def sub_check_output(cmd: List[str], **kwargs: Any) -> Any:
    """A simple proxy function to minimize and centralize subprocess usage."""
    # fmt: off
    if is_quiet():
        kwargs = {**kwargs, "stderr": subprocess.DEVNULL}
    result = subprocess.check_output(cmd, **kwargs)  # nosem: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
    # fmt: on
    return result


def manually_search_file(path: str, search_term: str, suffix: str) -> Optional[str]:
    """
    Searches a file for the given search term and, if found,
    returns the first word that contains that search term
    """
    if not os.path.isfile(path):
        return None
    with open(path, mode="r") as fd:
        contents = fd.read()
        words = contents.split()
    # Find all of the individual words that contain the search_term
    matches = [w for w in words if search_term in w]
    return matches[0] + suffix if len(matches) > 0 else None


def compute_executable_path(exec_name: str) -> str:
    """Determine full executable path if full path is needed to run it."""
    # First, try packaged binaries
    pkg_exec = pkg_resources.resource_filename("semgrep.bin", exec_name)
    if os.path.isfile(pkg_exec):
        return pkg_exec

    # Second, try system binaries
    which_exec = shutil.which(exec_name)
    if which_exec is not None:
        return which_exec

    # Third, look for something in the same dir as the Python interpreter
    relative_path = os.path.join(os.path.dirname(sys.executable), exec_name)
    if os.path.isfile(relative_path):
        return relative_path

    raise Exception(f"Could not locate '{exec_name}' binary")


def compute_semgrep_path() -> str:
    return compute_executable_path("semgrep-core")


SEMGREP_PATH = compute_semgrep_path()


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
    return "%.1f%sB" % (num, "Y")


def truncate(file_name: str, col_lim: int) -> str:
    name_len = len(file_name)
    prefix = "..."
    if name_len > col_lim:
        file_name = prefix + file_name[name_len - col_lim + len(prefix) :]
    return file_name
