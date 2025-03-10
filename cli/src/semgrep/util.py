import functools
import itertools
import operator
import os
import platform
import subprocess
import sys
from io import TextIOWrapper
from pathlib import Path
from typing import Any
from typing import Callable
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Sequence
from typing import TypeVar
from typing import Union
from urllib.parse import urlparse

import click

from semgrep.constants import Colors
from semgrep.constants import FIXTEST_SUFFIX
from semgrep.constants import YML_SUFFIXES
from semgrep.constants import YML_TEST_SUFFIXES
from semgrep.semgrep_interfaces.semgrep_output_v1 import Sha1
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

T = TypeVar("T")

# The character which is used to mask out findings content which we don't want
# to appear in logs. For instance, secrets findings and metavariable content
# from a findings match is masked using this.
MASK_CHAR = "*"
# The amount in [0, 1] to show of matches which are subject to masking. If this
# is 0.2, then 20% of the match (rounded down) is shown.
MASK_SHOW_PCT = 0.2

MAX_TEXT_WIDTH = 120

IS_WINDOWS = platform.system() == 'Windows'

def is_url(url: str) -> bool:
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False


def is_rules(rules: str) -> bool:
    return rules[:6] == "rules:" or rules[:8] == '{"rules"'


def is_truthy(value: Any) -> bool:
    """
    Returns True if the value is a truthy value, False otherwise.
    """
    val = str(value).lower()
    return value is not None and val in ["true", "1", "y", "on", "yes"]


def path_exists(path: Path, follow_symlinks: bool = True) -> bool:
    """
    path.exists(follow_links=False) is only supported with Python >= 12.
    This provides the same functionality for older versions of Python.
    Also, it returns False instead of raising exceptions in some situations.
    """
    try:
        if follow_symlinks:
            return path.exists()
        else:
            if path.is_symlink():
                return True
            else:
                return path.exists()
    except Exception:
        return False


def path_has_permissions(
    path: Path, permissions: int, follow_symlinks: bool = True
) -> bool:
    return (
        # path.stat(follow_symlinks=follow_symlinks): requires python >= 3.10
        path_exists(path, follow_symlinks=follow_symlinks)
        and (path.stat() if follow_symlinks else path.lstat()).st_mode & permissions
        == permissions
    )


def abort(message: str) -> None:
    click.secho(message, fg="red", err=True)
    sys.exit(2)


def has_color() -> bool:
    """
    Determine if color should be used in the output.

    NOTE: This method shares logic with the `configure` method in `terminal.py`
    and is intended to mimic the behavior without relying on internal state.
    This could be a candidate TODO for refactoring to avoid duplication.
    """
    force_color = is_truthy(os.environ.get("SEMGREP_FORCE_COLOR"))
    # See https://no-color.org/
    no_color = is_truthy(os.environ.get("NO_COLOR")) or is_truthy(
        os.environ.get("SEMGREP_FORCE_NO_COLOR")
    )
    # If both are force_color and no_color are set, force_color wins
    if force_color and no_color:
        return True
    if no_color:
        return False
    # If force_color is set, use it. Otherwise, use color if stdout is a tty.
    return force_color or sys.stdout.isatty()


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
    from semgrep.state import get_state  # avoiding circular imports

    terminal = get_state().terminal
    if not terminal.is_color:
        return text
    return click.style(
        text,
        fg=color.value,
        bg=(bgcolor.value if bgcolor is not None else None),
        underline=underline,
        bold=bold,
    )


def with_logo_color(text: str) -> str:
    """
    Wrap text with our brand color if color is enabled.
    Does not rely on internal state.
    """
    if has_color():
        return click.style(text, fg=Colors.green.value)
    return text


def welcome() -> None:
    """
    Print a welcome message with the Semgrep logo.
    """
    logo = with_logo_color("○○○")
    click.echo(
        f"""
┌──── {logo} ────┐
│ Semgrep CLI │
└─────────────┘
""",
        err=True,
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


def sub_check_output(cmd: Sequence[Union[str, Path]], **kwargs: Any) -> Any:
    """A simple proxy function to minimize and centralize subprocess usage."""
    from semgrep.state import get_state  # avoiding circular imports

    terminal = get_state().terminal
    if terminal.is_quiet:
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
    return (
        any(listendswith(path.suffixes, suffixes) for suffixes in YML_SUFFIXES)
        and not is_config_test_suffix(path)
        and not is_config_fixtest_suffix(path)
    )


def is_config_test_suffix(path: Path) -> bool:
    return any(
        listendswith(path.suffixes, suffixes) for suffixes in YML_TEST_SUFFIXES
    ) and not is_config_fixtest_suffix(path)


def is_config_fixtest_suffix(path: Path) -> bool:
    return FIXTEST_SUFFIX in path.suffixes


def final_suffix_matches(path: Path, path2: Path) -> bool:
    return (is_config_test_suffix(path) and is_config_test_suffix(path2)) or (
        path.suffixes[-1] == path2.suffixes[-1]
    )


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


def read_range(fd: TextIOWrapper, start_offset: int, end_offset: int) -> str:
    """
    Takes a file descriptor and returns the text between the offsets. Start
    inclusive, end exclusive.

    It is recommended to open the fd with `open(path, errors="replace"). See
    https://stackoverflow.com/a/56441652.
    """
    # Offsets are start inclusive and end exclusive
    length = end_offset - start_offset
    fd.seek(start_offset)
    return fd.read(length)


def get_lines_from_file(
    path: Path,
    start_line: int,
    end_line: int,
) -> List[str]:
    """
    Return lines in the given file.

    Assumes file exists.
    """
    # Start and end line are one-indexed, but the subsequent slice call is
    # inclusive for start and exclusive for end, so only subtract from start
    start_line = start_line - 1

    if start_line == -1 and end_line == 0:
        # Completely empty file
        return []

    # buffering=1 turns on line-level reads
    with path.open(buffering=1, errors="replace") as fd:
        result = list(itertools.islice(fd, start_line, end_line))

    return result


def get_lines_from_git_blob(
    blob_sha: Sha1,
    start_line: int,
    end_line: int,
) -> List[str]:
    """
    Return lines in the given git blob. Result is cached since calling git
    multiple times may be expensive and the contents of a blob are stable
    (addressed by sha), since (among other reasons) the sha is directly related
    to the content.

    Assumes blob exists.
    """
    # Avoid circular import
    from semgrep.git import git_check_output

    # Start and end line are one-indexed, but the subsequent slice call is
    # inclusive for start and exclusive for end, so only subtract from start
    start_line = start_line - 1

    if start_line == -1 and end_line == 0:
        # Completely empty file
        return []

    contents = git_check_output(["git", "cat-file", "blob", blob_sha.value])
    return list(itertools.islice(contents.splitlines(), start_line, end_line))


def with_feature_status(*, enabled: bool = False) -> str:
    """
    Returns the status of a feature with an icon indicator
      - enabled:   a green checkmark (✔)
      - otherwise: a red (x)
    """
    # NOTE: we could use something simple like `click.secho("✔", fg="green", nl=False)`
    # but we have a custom flag for forcing color off (i.e. `force_color_off`)
    # that we need to respect.
    return with_color(Colors.green, "✔") if enabled else with_color(Colors.red, "✘")


@functools.lru_cache(maxsize=None)
def line_count_of_path(path: Path) -> int:
    try:
        with open(path, "rb") as f:
            return sum(1 for _ in f)
    except Exception as e:
        logger.debug(f"Failed to open {path} to count lines: {e}")
        return 0


def pretty_print_percentage(numerator: int, denominator: int) -> str:
    if denominator == 0:
        return "an unknown percentage"
    percentage = numerator / denominator
    # round to 1 decimal place
    precision = 1
    percentage = round(percentage * 100, precision)
    if percentage < 0.1 and numerator != 0:
        return "<0.1%"
    if percentage > 99.9 and numerator != denominator:
        percentage = 99.9
    return f"~{percentage}%"
