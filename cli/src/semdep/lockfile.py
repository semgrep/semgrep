from abc import ABC
from abc import abstractmethod
from dataclasses import dataclass
from fnmatch import fnmatch
from pathlib import Path
from typing import Dict
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Tuple

from semdep.parsers.cargo import parse_cargo
from semdep.parsers.composer import parse_composer_lock
from semdep.parsers.gem import parse_gemfile
from semdep.parsers.go_mod import parse_go_mod
from semdep.parsers.gradle import parse_gradle
from semdep.parsers.mix import parse_mix
from semdep.parsers.package_lock import parse_package_lock
from semdep.parsers.packages_lock_c_sharp import (
    parse_packages_lock as parse_packages_lock_c_sharp,
)
from semdep.parsers.pipfile import parse_pipfile
from semdep.parsers.pnpm import parse_pnpm
from semdep.parsers.poetry import parse_poetry
from semdep.parsers.pom_tree import parse_pom_tree
from semdep.parsers.pubspec_lock import parse_pubspec_lock
from semdep.parsers.requirements import parse_requirements
from semdep.parsers.swiftpm import parse_package_resolved
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import SemgrepParser
from semdep.parsers.util import to_parser
from semdep.parsers.yarn import parse_yarn
from semgrep.semgrep_interfaces.semgrep_output_v1 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v1 import CargoParser
from semgrep.semgrep_interfaces.semgrep_output_v1 import Composer
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Hex
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Nuget
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pub
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import SwiftPM


@dataclass(frozen=True)
class LockfileMatcher(ABC):
    """
    Abstract base class for matching and parsing lockfiles.
    Attributes:
        manifest (Optional[str]): Represents the name of the manifest file associated with the lockfile.
            For example, in the case of Python's Pipfile.lock, the manifest is 'Pipfile'.
        parser (Parser): A function that parses the lockfile and potentially a manifest file into
            a list of FoundDependency objects and a list of DependencyParserError.
        ecosystem (Ecosystem): The ecosystem associated with the lockfile, such as Pypi, Npm, etc.
    """

    manifest: Optional[str]
    parser: SemgrepParser
    ecosystem: Ecosystem

    @abstractmethod
    def is_match(self, path: Path) -> bool:
        """
        Determines whether the given path matches the lockfile criteria for this matcher.
        Args:
            path (Path): The path to the lockfile.
        Returns:
            bool: True if the path matches, False otherwise.
        """

    @abstractmethod
    def get_manifest_path(self, lockfile_path: Path) -> Optional[Path]:
        """
        Retrieves the corresponding manifest path based on the lockfile path.
        Args:
            lockfile_path (Path): The path to the lockfile.
        Returns:
            Optional[Path]: The path to the manifest file if it exists, otherwise None.
        """

    def get_subproject_root(self, lockfile_path: Path) -> Path:
        """
        Retrieves the subproject root path based on the lockfile path.
        Args:
            lockfile_path (Path): The path to the lockfile.
        Returns:
            Path: The parent path of the lockfile.
        """
        return lockfile_path.parent


@dataclass(frozen=True)
class ExactLockfileMatcher(LockfileMatcher):
    """
    Matcher for lockfiles that have an exact filename.
    Attributes:
        lockfile (str): The exact name of the lockfile.
    Example:
        For Pipfile.lock, the manifest is Pipfile.
    """

    lockfile: str

    def is_match(self, path: Path) -> bool:
        return path.name == self.lockfile

    def get_manifest_path(self, lockfile_path: Path) -> Optional[Path]:
        if self.manifest:
            manifest_path = lockfile_path.parent / self.manifest
            return manifest_path if manifest_path.exists() else None
        return None


@dataclass(frozen=True)
class PatternLockfileMatcher(LockfileMatcher):
    """
    Matcher for lockfiles that follow a specific pattern.
    Attributes:
        pattern (str): The pattern that the lockfile name should match.
    Example:
        For a pattern of "*requirements*.txt", the manifest could be "*.in".
    """

    pattern: str

    def is_match(self, path: Path) -> bool:
        return fnmatch(str(path), self.pattern)

    def get_manifest_path(self, lockfile_path: Path) -> Optional[Path]:
        if self.manifest:
            manifest_path = self.get_subproject_root(lockfile_path) / self.manifest
            if manifest_path.exists():
                return manifest_path

        return None


@dataclass(frozen=True)
class RequirementsLockfileMatcher(PatternLockfileMatcher):
    def get_manifest_path(self, lockfile_path: Path) -> Optional[Path]:
        if self.manifest:
            # First check the lockfile's parent directory for the manifest
            manifest_path = self.get_subproject_root(lockfile_path) / self.manifest
            if manifest_path.exists():
                return manifest_path

            # If manifest not found, check for a manifest with the same stem as the lockfile
            lockfile_stem = lockfile_path.stem
            manifest_name = f"{lockfile_stem}{Path(self.manifest).suffix}"
            manifest_path = lockfile_path.with_name(manifest_name)
            return manifest_path if manifest_path.exists() else None
        return None

    def get_subproject_root(self, lockfile_path: Path) -> Path:
        """
        Retrieves the parent path of the lockfile.
        """

        # Not pretty but we need to handle the case where the lockfile is in a subdirectory
        # Ex. for requirements/base.txt, the parent path should be the directory containing the
        # requirements directory. For requirements.txt, the parent path should be the directory
        # containing the lockfile.

        # Check if the lockfile is in a 'requirements' directory
        if "requirements" in lockfile_path.parts:
            # Find the index of 'requirements' in the path
            req_index = lockfile_path.parts.index("requirements")
            # Return the parent of the 'requirements' directory
            return Path(*lockfile_path.parts[:req_index])
        else:
            # If not in a 'requirements' directory, return the immediate parent
            return lockfile_path.parent


NEW_REQUIREMENTS_MATCHERS: List[LockfileMatcher] = [
    ExactLockfileMatcher(
        lockfile="Pipfile.lock",
        manifest="Pipfile",
        parser=parse_pipfile,
        ecosystem=Ecosystem(Pypi()),
    ),
    ExactLockfileMatcher(
        lockfile="poetry.lock",
        manifest="pyproject.toml",
        parser=parse_poetry,
        ecosystem=Ecosystem(Pypi()),
    ),
    RequirementsLockfileMatcher(
        pattern="*requirements*.txt",
        manifest="requirements.in",
        parser=parse_requirements,
        ecosystem=Ecosystem(Pypi()),
    ),
]

OLD_REQUIREMENTS_MATCHERS: List[LockfileMatcher] = [
    ExactLockfileMatcher(
        lockfile="Pipfile.lock",
        manifest="Pipfile",
        parser=parse_pipfile,
        ecosystem=Ecosystem(Pypi()),
    ),
    ExactLockfileMatcher(
        lockfile="poetry.lock",
        manifest="pyproject.toml",
        parser=parse_poetry,
        ecosystem=Ecosystem(Pypi()),
    ),
    ExactLockfileMatcher(
        lockfile="requirements.txt",
        manifest="requirements.in",
        parser=parse_requirements,
        ecosystem=Ecosystem(Pypi()),
    ),
    ExactLockfileMatcher(
        lockfile="requirements3.txt",
        manifest="requirements.in",
        parser=parse_requirements,
        ecosystem=Ecosystem(Pypi()),
    ),
]


ECOSYSTEM_TO_LOCKFILES: Dict[Ecosystem, List[LockfileMatcher]] = {
    Ecosystem(Npm()): [
        ExactLockfileMatcher(
            lockfile="package-lock.json",
            manifest="package.json",
            parser=parse_package_lock,
            ecosystem=Ecosystem(Npm()),
        ),
        ExactLockfileMatcher(
            lockfile="yarn.lock",
            manifest="package.json",
            parser=parse_yarn,
            ecosystem=Ecosystem(Npm()),
        ),
        ExactLockfileMatcher(
            lockfile="pnpm-lock.yaml",
            manifest=None,
            parser=parse_pnpm,
            ecosystem=Ecosystem(Npm()),
        ),
    ],
    Ecosystem(Gem()): [
        ExactLockfileMatcher(
            lockfile="Gemfile.lock",
            manifest=None,
            parser=parse_gemfile,
            ecosystem=Ecosystem(Gem()),
        ),
    ],
    Ecosystem(Gomod()): [
        ExactLockfileMatcher(
            lockfile="go.mod",
            manifest=None,
            parser=parse_go_mod,
            ecosystem=Ecosystem(Gomod()),
        ),
    ],
    Ecosystem(Cargo()): [
        ExactLockfileMatcher(
            lockfile="Cargo.lock",
            manifest=None,
            parser=to_parser(parse_cargo, ScaParserName(CargoParser())),
            ecosystem=Ecosystem(Cargo()),
        ),
    ],
    Ecosystem(Maven()): [
        ExactLockfileMatcher(
            lockfile="maven_dep_tree.txt",
            manifest=None,
            parser=parse_pom_tree,
            ecosystem=Ecosystem(Maven()),
        ),
        ExactLockfileMatcher(
            lockfile="gradle.lockfile",
            manifest="build.gradle",
            parser=parse_gradle,
            ecosystem=Ecosystem(Maven()),
        ),
    ],
    Ecosystem(Composer()): [
        ExactLockfileMatcher(
            lockfile="composer.lock",
            manifest="composer.json",
            parser=parse_composer_lock,
            ecosystem=Ecosystem(Composer()),
        ),
    ],
    Ecosystem(Nuget()): [
        ExactLockfileMatcher(
            lockfile="packages.lock.json",
            manifest=None,
            parser=parse_packages_lock_c_sharp,
            ecosystem=Ecosystem(Nuget()),
        ),
    ],
    Ecosystem(Pub()): [
        ExactLockfileMatcher(
            lockfile="pubspec.lock",
            manifest="pubspec.yaml",
            parser=parse_pubspec_lock,
            ecosystem=Ecosystem(Pub()),
        ),
    ],
    Ecosystem(SwiftPM()): [
        ExactLockfileMatcher(
            lockfile="Package.resolved",
            manifest="Package.swift",
            parser=parse_package_resolved,
            ecosystem=Ecosystem(SwiftPM()),
        ),
    ],
    Ecosystem(Hex()): [
        ExactLockfileMatcher(
            lockfile="mix.lock",
            manifest="mix.exs",
            parser=parse_mix,
            ecosystem=Ecosystem(Hex()),
        ),
    ],
}


class EcosystemLockfiles:
    ecosystem_to_lockfiles: Dict[Ecosystem, List[LockfileMatcher]] = {
        **ECOSYSTEM_TO_LOCKFILES,
        # By default, use the old requirements lockfile matchers for Python
        **{Ecosystem(Pypi()): OLD_REQUIREMENTS_MATCHERS},
    }

    @classmethod
    def init(cls, use_new_requirements_matchers: bool = False) -> None:
        cls.ecosystem_to_lockfiles[Ecosystem(Pypi())] = (
            NEW_REQUIREMENTS_MATCHERS
            if use_new_requirements_matchers
            else OLD_REQUIREMENTS_MATCHERS
        )


def is_valid_lockfile(ecosystem: Ecosystem, path: Path) -> bool:
    """
    Check if a path is a valid lockfile for the given ecosystem.
    """
    lockfile_matchers = EcosystemLockfiles.ecosystem_to_lockfiles.get(ecosystem, [])
    return any(matcher.is_match(path) for matcher in lockfile_matchers)


def filter_lockfile_paths(
    ecosystem: Ecosystem, candidates: FrozenSet[Path]
) -> FrozenSet[Path]:
    """
    Filter out paths that are not lockfiles for the given ecosystem
    """
    return frozenset(path for path in candidates if is_valid_lockfile(ecosystem, path))


def create_matcher(path: Path) -> LockfileMatcher:
    """
    Method for creating the appropriate LockfileMatcher instance based on
    the lockfile path.
    """
    for _, lockfile_matchers in EcosystemLockfiles.ecosystem_to_lockfiles.items():
        for lockfile_matcher in lockfile_matchers:
            if lockfile_matcher.is_match(path):
                return lockfile_matcher

    raise ValueError(f"Unknown lockfile: {path.name}")


@dataclass(frozen=True)
class Lockfile:
    """
    Class for representing a lockfile and its metadata
    """

    path: Path
    parent_path: Path
    manifest_path: Optional[Path]
    ecosystem: Ecosystem
    matcher: LockfileMatcher

    @staticmethod
    def from_path(path: Path) -> "Lockfile":
        """
        Create a Lockfile instance from a path
        """
        matcher = create_matcher(path)
        manifest_path = matcher.get_manifest_path(path)
        parent_path = matcher.get_subproject_root(path)
        return Lockfile(
            path=path,
            parent_path=parent_path,
            manifest_path=manifest_path,
            ecosystem=matcher.ecosystem,
            matcher=matcher,
        )

    def parse(self) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
        """
        Parse a lockfile and return it as a list of dependency objects
        Also returns Optional DependencyParseError as second return value if there was a problem
        parsing the lockfile
        """
        return self.matcher.parser(self.path, self.manifest_path)
