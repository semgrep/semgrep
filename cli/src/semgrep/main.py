import sys
from os import environ

from colorama import Fore
from colorama import Style

from semgrep.cli import cli


# Note that it is pretty standard in Python to have a __main__.py file
# (see https://docs.python.org/3/library/__main__.html#main-py-in-python-packages)
# to provide a command-line interface for a package. One can then run
# your program/package simply with 'python -m <package> ...'.
# However for pysemgrep we don't want that anymore! We want to force people
# to call semgrep via cli/bin/semgrep because the Python Semgrep package
# will soon disappear, hence the use of main.py here, not __main__.py


def has_color() -> bool:
    """
    Determine if color should be used in the output.
    """
    force_color = environ.get("SEMGREP_FORCE_COLOR") is not None
    # See https://no-color.org/
    no_color = (
        environ.get("NO_COLOR") is not None
        or environ.get("SEMGREP_FORCE_NO_COLOR") is not None
    )
    # Should not have both force color and no color set
    # If both are set, force color should take precedence
    if force_color and no_color:
        return True
    if no_color:
        return False
    return force_color or sys.stdout.isatty()


def with_logo_color(text: str) -> str:
    """
    Wrap text with our brand color if color is enabled.
    """
    if has_color():
        return f"{Fore.GREEN}{text}{Style.RESET_ALL}"
    return text


def welcome() -> None:
    """
    Print a welcome message with the Semgrep logo.
    """
    logo = with_logo_color("○○○")
    print(
        f"""
┌──── {logo} ────┐
│ Semgrep CLI │
└─────────────┘

""",
        file=sys.stderr,
    )


# This is the entrypoint for the `pysemgrep` command.
def main() -> int:
    # Print the welcome message with the Semgrep logo,
    # intending to so as quickly as possible to give a sense of
    # a smooth startup experience.
    welcome()
    # To match the program usage help between pysemgrep (legacy)
    # and osemgrep (new) - and to hide complexity for our users -
    # here we specify `semgrep` as the program name for pysemgrep.
    cli(prog_name="semgrep")
    return 0
