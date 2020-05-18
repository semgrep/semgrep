from typing import List
from typing import NamedTuple
from typing import Optional

from colorama import Fore

from semgrep.semgrep_types import Span


class SemgrepException(Exception):
    """
    Parent class of all exceptions we anticipate in Semgrep commands

    All Semgrep Exceptions are caught and their error messages
    are displayed to the user.
    """

    def __init__(self) -> None:
        self.msg: Optional[str] = None
        self.code = 1


class OutdatedPythonException(SemgrepException):
    def __init__(self) -> None:
        super().__init__()
        self.msg = "Semgrep requires Python 3.5+. Please ensure you are using Python3.5+ to run semgrep."


class SemgrepLangError(NamedTuple):
    short_msg: str
    long_msg: Optional[str]
    level: str
    spans: List[Span]
    help: Optional[str] = None

    def __str__(self) -> str:
        return self.emit()

    def emit(self) -> str:
        # NOTE: this isn't perfect -- eg. alignment will be broken crossing from 2 digit numbers to 3 digit numbers
        header = f"{self.level}: {self.short_msg}"
        snippets = []
        for span in self.spans:
            location_hint = f"  --> {span.file}:{span.start_line + 1}"
            snippet = [location_hint]
            for line in range(span.context_start, span.start_line):
                snippet.append(f"{line + 1} | {span.raw[line]}")
            for line in range(span.start_line, span.end_line):
                snippet.append(f"{line + 1} | {span.raw[line]}")

            snippets.append("\n".join(snippet))
        snippet_str = "\n".join(snippets)
        if self.help:
            help = f"= help: {self.help}"
        else:
            help = ""
        return f"{header}\n{snippet_str}\n{Fore.BLUE}{help}{Fore.RESET}\n{Fore.RED}{self.long_msg}{Fore.RESET}\n"
