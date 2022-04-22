#!/usr/bin/env python3
import sys

from semgrep.cli import cli


def main() -> int:
    cli()
    return 0


if __name__ == "__main__":
    sys.exit(main())
