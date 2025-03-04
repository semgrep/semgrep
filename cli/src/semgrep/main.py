##############################################################################
# Prelude
##############################################################################
# Entry point of pysemgrep (called from console_scripts/entrypoint.py)
#
# Note that it is pretty standard in Python to have a __main__.py file
# (see https://docs.python.org/3/library/__main__.html#main-py-in-python-packages)
# to provide a command-line interface for a package. One can then run
# your program/package simply with 'python -m <package> ...'.
# However for pysemgrep we don't want that anymore! We want to force people
# to call semgrep via /usr/bin/semgrep because the Python Semgrep package
# will soon disappear, hence the use of main.py here, not __main__.py
#
import sys

from semgrep.util import welcome

##############################################################################
# Printing Welcome ASAP
##############################################################################

# NOTE: When experimenting with reducing startup times as part of a performance
# investigation, we found no distinct difference between running semgrep as a
# module (`python -m semgrep`) versus running it via the main `semgrep`
# entrypoint which has a longer call chain of
#    `semgrep -> exec_osemgrep() -> Main.ml -> pysemgrep -> semgrep.main.main()`.
#
# The biggest latency contributor was the import of `semgrep.cli` itself.
#
# While slow imports are a known issue in Python and there are some mitigations
# available, we have not invested much time in optimizing imports beyond
# ensuring we prefer narrow imports via `module.package import method` instead
# of `import module` to reduce the import load.
# There is an ongoing effort in the Python community to improve the startup time
# of imports, see https://peps.python.org/pep-0690/ which might help point us
# to some short-term solutions.
#
#
# For reproducibility, we used the following code snippet to measure the time
# taken between import and actual execution of the CLI with `--version` command.
#
#    def end_trace(operation: str, start: int):
#        end = time.monotonic_ns()
#        milliseconds = (end - start) / 1_000_000.0
#        sys.stderr.write(f"{operation} took {milliseconds:.2f} milliseconds\n")
#
#    @contextmanager
#    def log_timing(operation: str):
#        start = time.monotonic_ns()
#        yield
#        end_trace(operation, start)
#
#    if __name__ == "__main__":
#        with log_timing("Importing Semgrep CLI"):
#            from semgrep.cli import cli
#        start = time.monotonic_ns()
#        atexit.register(lambda: end_trace("CLI execution", start))
#        cli(prog_name="semgrep")
#
# Example output of `python semgrep --version`:
#    Importing Semgrep CLI took 615.74 milliseconds
#    1.85.0
#    CLI execution took 181.69 milliseconds
#    python -m semgrep --version  0.55s user 0.25s system 79% cpu 1.002 total
#
# Thus extra care should be taken before importing the CLI to ensure that
# the user is provided with some feedback that the program is running.


def conditional_welcome() -> None:
    """
    Print a welcome message as fast as possible under the right conditions.
    """
    # See CLI.ml for the list of pysemgrep commands
    exclusionary_pysemgrep_commands = {
        "publish",
        "login",
        "ci",
        "install-semgrep-pro",
        "lsp",
        "logout",
    }
    # Check if any of the exclusionary commands are provided
    args = set(sys.argv[1:])
    # We only allow `scan` or no subcommand to print the welcome message
    if args.intersection(exclusionary_pysemgrep_commands):
        return
    # Exclude alternate modes for the scan subcommand,
    # e.g. validation, version check, test, pattern mode.
    exclusionary_options = {
        # Validation
        "--validate",
        # Version check
        "--version",
        # Print targets
        "--x-ls",
        # Test mode
        "--test",
        # Pattern mode
        "-e",
        "--pattern",
        "-l",
        "--lang",
    }
    if args.intersection(exclusionary_options):
        return
    # NOTE: While we should unconditionally print the welcome message,
    # we will only print if stdout is a tty until we are ready to update
    # all the test snapshots.
    if not sys.stdout.isatty():
        return
    # Finally, print the welcome message
    welcome()


##############################################################################
# Entry point
##############################################################################


def main() -> None:
    """
    The entrypoint for `pysemgrep` and `semgrep` when `exec_pysemgrep` is called.
    """
    # We conditionally print the welcome message for the `scan` command
    # to give immediate feedback to the user.
    conditional_welcome()
    # This import is very slow (generally takes ~500ms)
    from semgrep.cli import cli

    # To match the program usage help between pysemgrep (legacy)
    # and osemgrep (new) - and to hide complexity for our users -
    # here we specify `semgrep` as the program name for pysemgrep.
    cli(prog_name="semgrep")
