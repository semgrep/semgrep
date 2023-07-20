import os
from functools import lru_cache
from pathlib import Path
from typing import Callable
from typing import Dict
from typing import Generator
from typing import List
from typing import Optional
from typing import Tuple

from semdep.parsers.composer import parse_composer_lock
from semdep.parsers.gem import parse_gemfile
from semdep.parsers.go_mod import parse_go_mod
from semdep.parsers.gradle import parse_gradle
from semdep.parsers.package_lock import parse_package_lock
from semdep.parsers.pipfile import parse_pipfile
from semdep.parsers.pnpm import parse_pnpm
from semdep.parsers.poetry import parse_poetry
from semdep.parsers.pom_tree import parse_pom_tree
from semdep.parsers.requirements import parse_requirements
from semdep.parsers.util import DependencyParserError
from semdep.parsers.yarn import parse_yarn
from semgrep.console import console
from semgrep.error import SemgrepError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v1 import CargoParser
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


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

NEW_LOCKFILE_PARSERS: Dict[
    str,
    Callable[
        [Path, Optional[Path]],
        Tuple[List[FoundDependency], List[DependencyParserError]],
    ],
] = {
    "requirements.txt": parse_requirements,  # Python
    "requirements3.txt": parse_requirements,  # Python
    "maven_dep_tree.txt": parse_pom_tree,  # Java
    "yarn.lock": parse_yarn,  # JavaScript
    "gradle.lockfile": parse_gradle,  # Java
    "pipfile.lock": parse_pipfile,  # Python
    "package-lock.json": parse_package_lock,  # JavaScript
    "gemfile.lock": parse_gemfile,  # Ruby
    "poetry.lock": parse_poetry,  # Python
    "go.mod": parse_go_mod,  # Go
    "pnpm-lock.yaml": parse_pnpm,  # JavaScript
    "composer.lock": parse_composer_lock,  # PHP
}

LOCKFILE_TO_MANIFEST: Dict[str, Optional[str]] = {
    "Pipfile.lock": "Pipfile",
    "poetry.lock": "pyproject.toml",
    "requirements.txt": "requirements.in",
    "requirements3.txt": "requirements.in",
    "package-lock.json": "package.json",
    "yarn.lock": "package.json",
    "composer.lock": "composer.json",
    "Gemfile.lock": None,
    "go.mod": None,
    "Cargo.lock": None,
    "maven_dep_tree.txt": None,
    "gradle.lockfile": "build.gradle",
    "pnpm-lock.yaml": None,
}


def lockfile_path_to_manifest_path(lockfile_path: Path) -> Optional[Path]:
    """
    Given full lockfile path, return the path to the manifest file if it exists

    Return None if file doesnt exist
    """
    path, lockfile_pattern = lockfile_path.parent, lockfile_path.parts[-1]
    manifest_pattern = LOCKFILE_TO_MANIFEST[lockfile_pattern]

    # some lockfiles don't have a manifest
    if not manifest_pattern:
        return None

    manifest_path = path / manifest_pattern
    if not manifest_path.exists():
        return None

    return manifest_path


def parse_lockfile_path(
    lockfile_path: Path,
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    """
    Parse a lockfile and return it as a list of dependency objects

    Also returns Optional DependencyParseError as second return value if there was a problem
    parsing the lockfile

    Raises SemgrepError if the lockfile is not supported
    """
    file_changed_timestamp = os.stat(lockfile_path).st_mtime
    return _parse_lockfile_path_helper(lockfile_path, file_changed_timestamp)


@lru_cache(maxsize=1000)
def _parse_lockfile_path_helper(
    lockfile_path: Path, file_changed_timestamp: float
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    """
    Parse a lockfile and return it as a list of dependency objects

    Takes file_changed_timestamp to help invalidate the cache in case the file has changed
    which can happen between a head <-> baseline scan transition
    """
    manifest_path = lockfile_path_to_manifest_path(lockfile_path)
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
            return (
                list(OLD_LOCKFILE_PARSERS[lockfile_name](lockfile_text, manifest_text)),
                [],
            )
        # Such a general except clause is suspect, but the parsing error could be any number of
        # python errors, since our parsers are just using stdlib string processing functions
        # This will avoid catching dangerous to catch things like KeyboardInterrupt and SystemExit
        except Exception as e:
            console.print(f"Failed to parse {lockfile_path} with exception {e}")
            return (
                [],
                [
                    DependencyParserError(
                        str(lockfile_path), ScaParserName(CargoParser()), str(e)
                    )
                ],
            )
    else:
        raise SemgrepError(f"don't know how to parse this filename: {lockfile_path}")
