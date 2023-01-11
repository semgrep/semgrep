from functools import lru_cache
from pathlib import Path
from typing import List
from typing import Optional

from semdep.parsers.gem import parse_gemfile
from semdep.parsers.go_sum import parse_go_sum
from semdep.parsers.gradle import parse_gradle
from semdep.parsers.package_lock import parse_package_lock
from semdep.parsers.pipfile import parse_pipfile
from semdep.parsers.poetry import parse_poetry
from semdep.parsers.pom_tree import parse_pom_tree
from semdep.parsers.requirements import parse_requirements
from semdep.parsers.yarn import parse_yarn
from semgrep.error import SemgrepError
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

LOCKFILE_PARSERS = {
    "pipfile.lock": parse_pipfile,  # Python
    "yarn.lock": parse_yarn,  # JavaScript
    "package-lock.json": parse_package_lock,  # JavaScript
    "gemfile.lock": parse_gemfile,  # Ruby
    "go.sum": parse_go_sum,  # Go
    "maven_dep_tree.txt": parse_pom_tree,  # Java
    "gradle.lockfile": parse_gradle,  # Java
    "poetry.lock": parse_poetry,  # Python
    "requirements.txt": parse_requirements,
}


@lru_cache(maxsize=1000)
def parse_lockfile_path(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    lockfile_name = lockfile_path.name.lower()
    # coupling with the github action, which decides to send files with these names back to us
    if lockfile_name in LOCKFILE_PARSERS:
        return LOCKFILE_PARSERS[lockfile_name](lockfile_path, manifest_path)
    else:
        raise SemgrepError(f"don't know how to parse this filename: {lockfile_path}")
