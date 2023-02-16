#!/usr/bin/env python3
from __future__ import annotations

import sys

from semgrep.cli import cli


def main() -> int:
    cli()
    return 0


if __name__ == "__main__":
    sys.exit(main())
