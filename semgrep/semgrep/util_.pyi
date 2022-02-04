from typing import (
    Any,
    Dict,
    FrozenSet,
    List,
    Sequence,
    Set,
    Tuple,
    Optional,
    Collection,
)
from typing import Iterable, Callable
from typing import TypeVar
from pathlib import Path
import semgrep.constants as constants

T = TypeVar("T")

def is_quiet() -> bool:
    """
    Returns true if logging level is quiet or quieter (higher)
    (i.e. only critical logs surfaced)
    """
    ...

def is_debug() -> bool:
    """
    Returns true if logging level is debug or noisier (lower)
    (i.e. want more logs)
    """
    ...

def is_url(url: str) -> bool: ...
def partition(
    pred: Callable[[T], Any], iterable: Iterable[T]
) -> Tuple[List[T], List[T]]:
    """E.g. partition(is_odd, range(10)) -> 1 3 5 7 9  and  0 2 4 6 8"""
    ...

def partition_set(
    pred: Callable[[T], Any], iterable: Iterable[T]
) -> Tuple[Set[T], Set[T]]:
    """E.g. partition(is_odd, range(10)) -> 1 3 5 7 9  and  0 2 4 6 8"""
    ...

def abort(message: str) -> None: ...
def with_color(
    color: constants.Colors,
    text: str,
    bgcolor: Optional[constants.Colors] = None,
    bold: bool = False,
    underline: bool = False,
) -> str:
    """
    Wrap text in color & reset

    Use ANSI color names or 8 bit colors (24-bit is not well supported by terminals)
    In click bold always switches colors to their bright variant (if there is one)
    """
    ...

def terminal_wrap(text: str) -> str: ...
def sub_run(cmd: List[str], **kwargs: Any) -> Any:
    """A simple proxy function to minimize and centralize subprocess usage."""
    ...

def sub_check_output(cmd: List[str], **kwargs: Any) -> Any:
    """A simple proxy function to minimize and centralize subprocess usage."""
    ...

def manually_search_file(path: str, search_term: str, suffix: str) -> Optional[str]:
    """
    Searches a file for the given search term and, if found,
    returns the first word that contains that search term
    """
    ...

def is_config_suffix(path: Path) -> bool: ...
def is_config_test_suffix(path: Path) -> bool: ...
def format_bytes(num: float) -> str: ...
def truncate(file_name: str, col_lim: int) -> str: ...
def flatten(some_list: List[List[T]]) -> List[T]: ...

PathFilterCallable = Callable[..., FrozenSet[Path]]

def log_removed_paths(function: PathFilterCallable) -> PathFilterCallable:
    """A decorator you can apply to functions that filter paths, to keep track of what they filtered.

    It assumes your filter function takes a set of candidate paths as its first parameter,
    and returns a set of remaining paths.

    It adds the keyword argument `removal_log` to the filtering function's signature.
    When this keyword argument is passed, all removed paths are added to this set.
    """
    ...
