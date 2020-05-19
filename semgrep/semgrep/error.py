from typing import Optional


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
