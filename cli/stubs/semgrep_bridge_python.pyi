# semgrep_bridge_python.pyi
# Interface stubs for 'semgrep_bridge_python' module.

# For usage information, see the module help text:
#
#   $ python3 -c "import semgrep_bridge_python; help(semgrep_bridge_python)"

from typing import Callable
from typing import List
from typing import Optional
from typing import Tuple

def startup() -> None: ...
def shutdown() -> None: ...
def semgrep_analyze(
    argv: List[str], read_file: Callable[[str], Tuple[bytes, int]]
) -> Optional[str]: ...

# EOF
