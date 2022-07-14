import functools
import operator
import os
import subprocess
import sys
from pathlib import Path
from textwrap import dedent
from typing import Any
from typing import Callable
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Sequence
from typing import TypeVar
from urllib.parse import urlparse

import click

from semgrep.constants import Colors
from semgrep.constants import FIXTEST_SUFFIX
from semgrep.constants import YML_SUFFIXES
from semgrep.constants import YML_TEST_SUFFIXES


T = TypeVar("T")


MAX_TEXT_WIDTH = 120


def is_url(url: str) -> bool:
    try:
        result = urlparse(url)
        return all([result.scheme, result.netloc])
    except ValueError:
        return False


def path_has_permissions(path: Path, permissions: int) -> bool:
    return path.exists() and path.stat().st_mode & permissions == permissions


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


def git_check_output(command: Sequence[str], cwd: Optional[str] = None) -> str:
    """
    Helper function to run a GIT command that prints out helpful debugging information
    """
    # Avoiding circular imports
    from semgrep.error import SemgrepError
    from semgrep.state import get_state

    env = get_state().env

    cwd = cwd if cwd is not None else os.getcwd()
    try:
        # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
        return subprocess.check_output(
            command,
            stderr=subprocess.PIPE,
            encoding="utf-8",
            timeout=env.git_command_timeout,
            cwd=cwd,
        ).strip()
    except subprocess.CalledProcessError as e:
        command_str = " ".join(command)
        raise SemgrepError(
            dedent(
                f"""
                Command failed with exit code: {e.returncode}
                -----
                Command failed with output:
                {e.stderr}

                Failed to run '{command_str}'. Possible reasons:

                - the git binary is not available
                - the current working directory is not a git repository
                - the current working directory is not marked as safe
                    (fix with `git config --global --add safe.directory $(pwd)`)

                Try running the command yourself to debug the issue.
                """
            ).strip()
        )
