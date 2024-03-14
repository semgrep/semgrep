from semgrep.cli import cli

# Note that it is pretty standard in Python to have a __main__.py file
# (see https://docs.python.org/3/library/__main__.html#main-py-in-python-packages)
# to provide a command-line interface for a package. One can then run
# your program/package simply with 'python -m <package> ...'.
# However for pysemgrep we don't want that anymore! We want to force people
# to call semgrep via cli/bin/semgrep because the Python Semgrep package
# will soon disappear, hence the use of main.py here, not __main__.py


# This is the entrypoint for the `pysemgrep` command.
def main() -> int:
    # To match the program usage help between pysemgrep (legacy)
    # and osemgrep (new) - and to hide complexity for our users -
    # here we specify `semgrep` as the program name for pysemgrep.
    cli(prog_name="semgrep")
    return 0
