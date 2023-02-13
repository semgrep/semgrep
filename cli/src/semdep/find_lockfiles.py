# find lockfiles
from pathlib import Path
from typing import Optional

from semgrep.semgrep_interfaces.semgrep_output_v1 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi

ECOSYSTEM_TO_LOCKFILES = {
    Ecosystem(Pypi()): ["Pipfile.lock", "poetry.lock", "requirements.txt"],
    Ecosystem(Npm()): ["package-lock.json", "yarn.lock"],
    Ecosystem(Gem()): ["Gemfile.lock"],
    Ecosystem(Gomod()): ["go.sum"],
    Ecosystem(Cargo()): ["Cargo.lock"],
    Ecosystem(Maven()): ["maven_dep_tree.txt", "gradle.lockfile"],
}

LOCKFILE_TO_MANIFEST = {
    "Pipfile.lock": "Pipfile",
    "poetry.lock": "pyproject.toml",
    "requirements.txt": "requirements.in",
    "package-lock.json": "package.json",
    "yarn.lock": "package.json",
    "Gemfile.lock": None,
    "go.sum": None,
    "Cargo.lock": None,
    "maven_dep_tree.txt": None,
    "gradle.lockfile": None,
}


def lockfile_path_to_manfiest_path(lockfile_path: Path) -> Optional[Path]:
    """
    Given full lockfile path, return the path to the manifest file if it exists
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


def find_single_lockfile(p: Path, ecosystem: Ecosystem) -> Optional[Path]:
    """
    Find the nearest lockfile in a given ecosystem to P
    Searches only up the directory tree
    """
    for path in p.parents:
        for lockfile_pattern in ECOSYSTEM_TO_LOCKFILES[ecosystem]:
            lockfile_path = path / lockfile_pattern
            if lockfile_path.exists():
                return lockfile_path
            else:
                continue
    return None
