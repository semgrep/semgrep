#!/usr/bin/env python3
import sys

from semgrep.cli import cli

# NOTE: This is the entrypoint for the `pysemgrep` command.
# To match the program usage help between pysemgrep (legacy)
# and osemgrep (new) – and to hide complexity for our users -
# here we specify `semgrep` as the program name for pysemgrep.
def main() -> int:
    cli(prog_name="semgrep")
    return 0


if __name__ == "__main__":
    sys.exit(main())
