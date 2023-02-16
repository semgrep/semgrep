# semgrep_bridge_python.pyi
# Interface stubs for 'semgrep_bridge_python' module.

# For usage information, see the module help text:
#
#   $ python3 -c "import semgrep_bridge_python; help(semgrep_bridge_python)"
from __future__ import annotations

from typing import Callable

def startup() -> None: ...
def shutdown() -> None: ...
def semgrep_analyze(
    argv: list[str], read_file: Callable[[str], tuple[bytes, int]]
) -> str | None: ...

# EOF
