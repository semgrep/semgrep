from functools import lru_cache
from pathlib import Path
from typing import Generator
from typing import List
from typing import Optional

from semgrep.error import SemgrepError
from semgrep.verbose_logging import getLogger

# NOTE: Defused XML doesn't export types :(


logger = getLogger(__name__)

from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown

from semdep.parsers.requirements import parse_requirements
from semdep.parsers.pom_tree import parse_pom_tree
from semdep.parsers.gem import parse_gemfile
from semdep.parsers.go_sum import parse_go_sum
from semdep.parsers.gradle import parse_gradle
from semdep.parsers.pipfile import parse_pipfile
from semdep.parsers.poetry import parse_poetry
from semdep.parsers.pom_tree import parse_pom_tree
from semdep.parsers.yarn import parse_yarn
from semdep.parsers.package_lock import parse_package_lock

from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown


def parse_cargo(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    def parse_dep(s: str) -> FoundDependency:
        lines = s.split("\n")[1:]
        dep = lines[0].split("=")[1].strip()[1:-1]
        version = lines[1].split("=")[1].strip()[1:-1]
        if len(lines) >= 3 and lines[3].startswith("checksum"):
            hash = {"sha256": [lines[3].split("=")[1].strip()[1:-1]]}
        else:
            hash = {}
        return FoundDependency(
            package=dep,
            version=version,
            ecosystem=Ecosystem(Cargo()),
            resolved_url=None,
            allowed_hashes=hash,
            transitivity=Transitivity(Unknown()),
        )

    deps = lockfile_text.split("[[package]]")[1:]
    yield from (parse_dep(dep) for dep in deps)


OLD_LOCKFILE_PARSERS = {
    "cargo.lock": parse_cargo,  # Rust
}

NEW_LOCKFILE_PARSERS = {
    "requirements.txt": parse_requirements,  # Python
    "maven_dep_tree.txt": parse_pom_tree,  # Java
    "yarn.lock": parse_yarn,  # JavaScript
    "gradle.lockfile": parse_gradle,  # Java
    "pipfile.lock": parse_pipfile,  # Python
    "package-lock.json": parse_package_lock,  # JavaScript
    "gemfile.lock": parse_gemfile,  # Ruby
    "poetry.lock": parse_poetry,  # Python
    "go.sum": parse_go_sum,  # Go
}


@lru_cache(maxsize=1000)
def parse_lockfile_path(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    # coupling with the github action, which decides to send files with these names back to us
    lockfile_name = lockfile_path.name.lower()
    if lockfile_name in NEW_LOCKFILE_PARSERS:
        parse_lockfile = NEW_LOCKFILE_PARSERS[lockfile_name]
        return parse_lockfile(lockfile_path, manifest_path)

    if lockfile_name in OLD_LOCKFILE_PARSERS:
        lockfile_text = lockfile_path.read_text()
        if manifest_path:
            manifest_text = manifest_path.read_text()
        else:
            manifest_text = None

        try:
            return list(
                OLD_LOCKFILE_PARSERS[lockfile_name](lockfile_text, manifest_text)
            )
        # Such a general except clause is suspect, but the parsing error could be any number of
        # python errors, since our parsers are just using stdlib string processing functions
        # This will avoid catching dangerous to catch things like KeyboardInterrupt and SystemExit
        except Exception as e:
            logger.error(f"Failed to parse {lockfile_path} with exception {e}")
            return []
    else:
        raise SemgrepError(f"don't know how to parse this filename: {lockfile_path}")
