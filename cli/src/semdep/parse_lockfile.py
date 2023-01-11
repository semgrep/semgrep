from functools import lru_cache
from pathlib import Path
from typing import List
from typing import Optional

from parsy import ParseError

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
def parse_lockfile_str(
    lockfile_text: str, filepath_for_reference: Path, manifest_text: Optional[str]
) -> List[FoundDependency]:
    # coupling with the github action, which decides to send files with these names back to us
    filepath = filepath_for_reference.name.lower()
    if filepath in LOCKFILE_PARSERS:
        try:
            return list(LOCKFILE_PARSERS[filepath](lockfile_text, manifest_text))
        except ParseError as e:
            logger.error(f"Failed to parse {filepath_for_reference} with exception {e}")
            return []
    else:
        raise SemgrepError(
            f"don't know how to parse this filename: {filepath_for_reference}"
        )
