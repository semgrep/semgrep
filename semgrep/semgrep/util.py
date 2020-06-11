import itertools
import re
import sys
import threading
import time
import typing
from typing import Any
from typing import Callable
from typing import Iterable
from typing import List
from typing import Optional
from typing import TextIO
from typing import Tuple
from urllib.parse import urlparse

from colorama import Fore

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


class StoppableProgressWriter(threading.Thread):
    # cf. https://code.activestate.com/recipes/535141-console-progress-dots-using-threads-and-a-context-/

    def __init__(
        self,
        stream: TextIO = sys.stderr,
        interval: float = 1.0,
        spinner: Optional[List[str]] = None,
        done_msg: str = "finished!",
        **kwargs: Any,
    ):
        super(StoppableProgressWriter, self).__init__(**kwargs)
        self.stream = stream
        self.interval = interval
        if not spinner:
            spinner = ["\\", "|", "/", "-"]
        self.spinner = itertools.cycle(spinner)
        self.done_msg = done_msg
        self.event = threading.Event()

    def _clear_line(self) -> None:
        self.stream.write("\r")  # Put cursor at beginning of line
        self.stream.write("\033[K")  # Clear to end of line
        self.stream.flush()

    def stop(self, fail: bool = False) -> None:
        if self.stream.isatty():
            self._clear_line()
            if not fail:
                self.stream.write(self.done_msg + "\n")
                self.stream.flush()
        self.event.set()

    def run(self) -> None:
        if self.stream.isatty():
            start = time.time()
            while not self.event.is_set():
                self._clear_line()
                message = f" scanning... {round(time.time() - start, 3)}s elapsed"
                self.stream.write(next(self.spinner))
                self.stream.write(message)
                self.stream.flush()
                time.sleep(self.interval)


def with_color(color: str, text: str, bold: bool = False) -> str:
    """
    Wrap text in color & reset
    """
    reset = Fore.RESET
    if bold:
        color = color + "\033[1m"
        reset += "\033[0m"
    return f"{color}{text}{reset}"
