import sys
from typing import Any
from typing import IO
from typing import Iterable

from tqdm import tqdm

from semgrep.util import is_debug
from semgrep.util import is_quiet
from semgrep.util import T


def debug_tqdm_write(msg: str, file: IO = sys.stderr) -> None:
    if is_debug():
        tqdm.write(msg, file=file)


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
