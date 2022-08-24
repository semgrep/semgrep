# find lockfiles
from pathlib import Path
from typing import List
from typing import Optional
from typing import Tuple

from semdep.parse_lockfile import parse_lockfile_str
from semgrep.semgrep_interfaces.semgrep_output_v0 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v0 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v0 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v0 import Gem
from semgrep.semgrep_interfaces.semgrep_output_v0 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v0 import Gradle
from semgrep.semgrep_interfaces.semgrep_output_v0 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v0 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v0 import Pypi

ECOSYSTEM_TO_LOCKFILES = {
    Ecosystem(Pypi()): ["Pipfile.lock", "poetry.lock"],
    Ecosystem(Npm()): ["package-lock.json", "yarn.lock"],
    Ecosystem(Gem()): ["Gemfile.lock"],
    Ecosystem(Gomod()): ["go.sum"],
    Ecosystem(Cargo()): ["Cargo.lock"],
    Ecosystem(Maven()): ["pom.xml"],
    Ecosystem(Gradle()): ["gradle.lockfile"],
}


def find_single_lockfile(
    p: Path, ecosystem: Ecosystem
) -> Optional[Tuple[Path, List[FoundDependency]]]:
    """
    Find the nearest lockfile in a given ecosystem to P
    Searches only up the directory tree
    """
    for path in p.parents:
        for lockfile_pattern in ECOSYSTEM_TO_LOCKFILES[ecosystem]:
            lockfile_path = path / lockfile_pattern
            if lockfile_path.exists():
                return lockfile_path, parse_lockfile_str(
                    lockfile_path.read_text(encoding="utf8"), lockfile_path
                )
            else:
                continue
    return None
