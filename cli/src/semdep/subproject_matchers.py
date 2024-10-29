from abc import ABC
from abc import abstractmethod
from dataclasses import dataclass
from dataclasses import field
from fnmatch import fnmatch
from pathlib import Path
from typing import Dict
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple
from typing import Union

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.subproject import DependencySource
from semgrep.subproject import LockfileDependencySource
from semgrep.subproject import ManifestOnlyDependencySource
from semgrep.subproject import MultiLockfileDependencySource
from semgrep.subproject import PackageManagerType
from semgrep.subproject import Subproject


@dataclass(frozen=True)
class SubprojectMatcher(ABC):
    """
    This is a base class for "matchers" that create "subprojects" from a list of potential
    candidates. Subclasses must implement `is_match` and `make_subprojects`.
    """

    @abstractmethod
    def is_match(self, path: Path) -> bool:
        """
        Whether the given path has a relevant filename for this type of matcher.
        """
        raise NotImplementedError

    @abstractmethod
    def make_subprojects(
        self, dep_source_files: FrozenSet[Path]
    ) -> Tuple[List[Subproject], FrozenSet[Path]]:
        """
        Use the files given in `dep_source_files` to make as many subprojects as possible.
        This may not use all the files in `dep_source_files`.

        Returns:
        - the list of created subprojects
        - the set of files that were used to construct the returned subprojects.
        """
        raise NotImplementedError


@dataclass(frozen=True)
class LockfileManifestMatcher(SubprojectMatcher):
    """
    An abstract class for matchers that look for a single lockfile and a single manifest.

    Subprojects are a match only when the lockfile is found, and are created whether the manifest
    is found or not.

    Child classes must implement _is_manifest_match, _is_lockfile_match, _lockfile_to_manifest, and _get_subproject_root
    """

    manifest_kind: out.ManifestKind
    package_manager_type: PackageManagerType

    @abstractmethod
    def _is_manifest_match(self, path: Path) -> bool:
        raise NotImplementedError

    @abstractmethod
    def _is_lockfile_match(self, path: Path) -> bool:
        raise NotImplementedError

    @abstractmethod
    def _lockfile_to_manifest(
        self, lockfile_path: Path, candidates: FrozenSet[Path]
    ) -> Optional[Path]:
        """
        Given a lockfile path, return the path to the corresponding manifest
        if it exists in `candidates`.
        """
        raise NotImplementedError

    @abstractmethod
    def _get_subproject_root(
        self,
        manifest_lockfile_paths: Union[
            Tuple[Path, None], Tuple[None, Path], Tuple[Path, Path]
        ],
    ) -> Path:
        """
        Get the root of the subproject. The arguments are passed in a tuple of (manifest_path, lockfile_path)
        rather than individually to allow verifying that at least one of the arguments is not None.
        """
        raise NotImplementedError

    def is_match(self, path: Path) -> bool:
        return self._is_manifest_match(path) or self._is_lockfile_match(path)

    def _filter_manifest_lockfiles(
        self, dep_source_files: FrozenSet[Path]
    ) -> Tuple[Set[Path], Set[Path]]:
        """
        Classifies the provided source files as lockfiles, manifests, or neither.

        Returns a tuple of (manifest_paths, lockfile_paths)
        """
        lockfiles: Set[Path] = set()
        manifests: Set[Path] = set()
        for path in dep_source_files:
            if self._is_lockfile_match(path):
                lockfiles.add(path)
            if self._is_manifest_match(path):
                manifests.add(path)
        return (manifests, lockfiles)

    def make_subprojects(
        self, dep_source_files: FrozenSet[Path]
    ) -> Tuple[List[Subproject], FrozenSet[Path]]:
        """
        Use the files given in `dep_source_files` to make as many subprojects as possible.
        This may not use all the files in `dep_source_files`.
        """
        # grab all lockfiles and all manifests matching the pattern for this matcher.
        # we will use these to construct subprojects
        _manifests, lockfiles = self._filter_manifest_lockfiles(dep_source_files)

        # track the manifests that we use in the first lockfile-based step. These manifests
        # should be skipped in the second manifest-based step.
        paired_manifests: Set[Path] = set()

        subprojects: List[Subproject] = []

        # first, handle cases where the lockfile exists and manifest may or may not
        for lockfile_path in lockfiles:
            matching_manifest_path = self._lockfile_to_manifest(
                lockfile_path, dep_source_files
            )
            if matching_manifest_path is not None:
                root_dir = self._get_subproject_root(
                    (matching_manifest_path, lockfile_path)
                )
                paired_manifests.add(matching_manifest_path)
            else:
                # mypy is not smart enough to allow us to pull root_dir out of the if/else,
                # so we do it in each branch...
                root_dir = self._get_subproject_root(
                    (matching_manifest_path, lockfile_path)
                )
            dep_source = LockfileDependencySource(
                manifest=(self.manifest_kind, matching_manifest_path),
                package_manager_type=self.package_manager_type,
                lockfile_path=lockfile_path,
            )
            subprojects.append(
                Subproject(root_dir=root_dir, dependency_source=dep_source)
            )

        return subprojects, frozenset(paired_manifests | lockfiles)


@dataclass(frozen=True)
class ExactLockfileManifestMatcher(LockfileManifestMatcher):
    """
    Matcher for lockfiles and manifests that have an exact filename.
    Both manifest and lockfile name must be defined, but a subproject
    is generated if at least one of the two is present.

    Attributes:
        lockfile_name (str): The exact name of the lockfile.
        manifest_name (str): The exact name of the manifest
    Example:
        For Pipfile.lock, the manifest is Pipfile.
    """

    lockfile_name: str
    manifest_name: str  # might want to make this optional and remove the manifests that weren't there before in the config

    def _is_manifest_match(self, path: Path) -> bool:
        return path.name == self.manifest_name

    def _is_lockfile_match(self, path: Path) -> bool:
        return path.name == self.lockfile_name

    def _lockfile_to_manifest(
        self, lockfile_path: Path, candidates: FrozenSet[Path]
    ) -> Optional[Path]:
        if self.manifest_name:
            manifest_path = lockfile_path.parent / self.manifest_name
            return manifest_path if manifest_path in candidates else None
        return None

    def _get_subproject_root(
        self,
        manifest_lockfile_paths: Union[
            Tuple[Path, None], Tuple[None, Path], Tuple[Path, Path]
        ],
    ) -> Path:
        # the subproject root is just the direct parent of the manifest or of the lockfile,
        # with priority to the manifest if it exists (for no particular reason)
        return (
            manifest_lockfile_paths[0].parent
            if manifest_lockfile_paths[0] is not None
            else manifest_lockfile_paths[1].parent
        )


@dataclass(frozen=True)
class PatternLockfileMatcher(LockfileManifestMatcher):
    """
    Matcher for lockfiles and manifests that follow a specific pattern
    Attributes:
        lockfile_pattern: The pattern that the lockfile name should match.
            This lockfile name must be at the subproject root.
        manifest_name: The exact name that the corresponding manifest should match.
            The manifest is not required, so a match will still
            be generated if no manifest is found.
    """

    lockfile_pattern: str
    manifest_name: str  # we might want to let this be None sometimes, if we have lockfile-only package managers

    def _is_lockfile_match(self, path: Path) -> bool:
        return fnmatch(str(path), self.lockfile_pattern)

    def _is_manifest_match(self, path: Path) -> bool:
        return path.name == self.manifest_name

    def _lockfile_to_manifest(
        self, lockfile_path: Path, candidates: FrozenSet[Path]
    ) -> Optional[Path]:
        manifest_path = lockfile_path.parent / self.manifest_name
        if manifest_path in candidates:
            return manifest_path
        return None

    def _get_subproject_root(
        self,
        manifest_lockfile_paths: Union[
            Tuple[Path, None], Tuple[None, Path], Tuple[Path, Path]
        ],
    ) -> Path:
        return (
            manifest_lockfile_paths[0].parent
            if manifest_lockfile_paths[0] is not None
            else manifest_lockfile_paths[1].parent
        )


@dataclass(frozen=True)
class ManifestOnlyMatcher(SubprojectMatcher):
    """
    A matcher for lone manifests. These matchers must be placed after matchers
    that look for both a manifest and a lockfile.

    Implementors must implement _is_manifest_match and _get_subproject_root
    """

    manifest_kind: out.ManifestKind

    @abstractmethod
    def _is_manifest_match(self, path: Path) -> bool:
        raise NotImplementedError

    @abstractmethod
    def _get_subproject_root(self, manifest_path: Path) -> Path:
        raise NotImplementedError

    def is_match(self, path: Path) -> bool:
        return self._is_manifest_match(path)

    def _filter_matching_manifests(
        self, dep_source_files: FrozenSet[Path]
    ) -> FrozenSet[Path]:
        """
        Return only the matching manifests from the set of dependency source files.
        """
        return frozenset(p for p in dep_source_files if self._is_manifest_match(p))

    def make_subprojects(
        self, dep_source_files: FrozenSet[Path]
    ) -> Tuple[List[Subproject], FrozenSet[Path]]:
        manifests = self._filter_matching_manifests(dep_source_files)

        subprojects: List[Subproject] = []
        for manifest_path in manifests:
            root_dir = self._get_subproject_root(manifest_path)
            manifest_dep_source = ManifestOnlyDependencySource(
                manifest_kind=self.manifest_kind,
                manifest_path=manifest_path,
            )
            subprojects.append(
                Subproject(root_dir=root_dir, dependency_source=manifest_dep_source)
            )

        return subprojects, manifests


@dataclass(frozen=True)
class ExactManifestOnlyMatcher(ManifestOnlyMatcher):
    manifest_name: str

    def _get_subproject_root(self, manifest_path: Path) -> Path:
        return manifest_path.parent

    def _is_manifest_match(self, path: Path) -> bool:
        return path.name == self.manifest_name


@dataclass(frozen=True)
class PipRequirementsMatcher(SubprojectMatcher):
    """
    Matcher for requirements lockfiles specifically, which may be nested inside
    a folder called "requirements" instead of being directly at the subproject
    root.

    Also supports multiple lockfiles in a single root directory.
    """

    # base file pattern is everything except for the last extension. Applies to both
    # requirements and manifests
    base_file_pattern: str  # without extension

    requirements_file_extensions: List[str]
    manifest_file_extension: str

    default_manifest_file_base: str  # without extension
    manifest_kind: out.ManifestKind = field(
        default_factory=lambda: out.ManifestKind(value=out.RequirementsIn())
    )

    def _is_requirements_match(self, path: Path) -> bool:
        for ext in self.requirements_file_extensions:
            if fnmatch(str(path), f"{self.base_file_pattern}.{ext}"):
                return True
        return False

    def _is_manifest_match(self, path: Path) -> bool:
        return (
            fnmatch(
                str(path), f"{self.base_file_pattern}.{self.manifest_file_extension}"
            )
            or str(path)
            == f"{self.default_manifest_file_base}.{self.manifest_file_extension}"
        )

    def is_match(self, path: Path) -> bool:
        return self._is_manifest_match(path) or self._is_requirements_match(path)

    def _lockfile_to_manifest(
        self, requirements_path: Path, candidates: FrozenSet[Path]
    ) -> Optional[Path]:
        """
        Finds the corresponding manifest for the given lockfile_path, if it exists.

        To be recognized, manifests must either names must either:
        - be exactly the default_manifest_file_name and be located in the root
            directory of the subproject
        - match the _stem_ of the requirements path and be located alongside the
            requirements file
        """
        # First check for a manifest with the same stem as the lockfile
        # e.g. requirements-dev.txt -> requirements-dev.in
        requirements_stem = requirements_path.stem
        manifest_name = f"{requirements_stem}.{self.manifest_file_extension}"
        manifest_path = requirements_path.with_name(manifest_name)
        if manifest_path in candidates:
            return manifest_path

        # If that didn't find anything, look in the subproject root directory
        manifest_path = (
            self._get_requirements_root(requirements_path)
            / f"{self.default_manifest_file_base}.{self.manifest_file_extension}"
        )
        if manifest_path in candidates:
            return manifest_path
        else:
            return None

    def _get_requirements_root(self, requirements_path: Path) -> Path:
        # We need to handle the case where the lockfile is in a subdirectory that we recognize.
        # E.g. for requirements/base.txt, the subproject root directory should be the directory including the
        # requirements directory. For requirements.txt, the subproject root should be the directory
        # containing the lockfile.

        # Check if the lockfile is in a 'requirements' directory
        if "requirements" in requirements_path.parts:
            # Find the index of 'requirements' in the path
            req_index = requirements_path.parts.index("requirements")
            # Return the parent of the 'requirements' directory
            return Path(*requirements_path.parts[:req_index])
        else:
            # If not in a 'requirements' directory, return the immediate parent
            return requirements_path.parent

    def _filter_manifest_requirements(
        self, dep_source_files: FrozenSet[Path]
    ) -> Tuple[Set[Path], Set[Path]]:
        """
        Classifies the provided source files as requirements files, manifests, or neither.

        Returns a tuple of (manifest_paths, requirements_paths)
        """
        requirements_files: Set[Path] = set()
        manifests: Set[Path] = set()
        for path in dep_source_files:
            if self._is_requirements_match(path):
                requirements_files.add(path)
            if self._is_manifest_match(path):
                manifests.add(path)
        return (manifests, requirements_files)

    def make_subprojects(
        self, dep_source_files: FrozenSet[Path]
    ) -> Tuple[List[Subproject], FrozenSet[Path]]:
        # find all manifests and requirements files that we will use to build subprojects
        manifests, requirements_files = self._filter_manifest_requirements(
            dep_source_files
        )

        subprojects: List[Subproject] = []

        # tracks manifests that were accounted for in the first (requirements-based) phase.
        # These manifests should not be used in the second (manifest-only) phase.
        paired_manifests: Set[Path] = set()

        # Because multiple lockfiles can exist in a single subproject, we first need to group the
        # requirements files by root directory
        requirements_files_by_root_dir: Dict[Path, List[Path]] = {}
        for requirements_path in requirements_files:
            root_dir = self._get_requirements_root(requirements_path)
            if root_dir not in requirements_files_by_root_dir:
                requirements_files_by_root_dir[root_dir] = []
            requirements_files_by_root_dir[root_dir].append(requirements_path)

        # Create a subproject from each group of requirements files and the corresponding manifest
        # for each, if it exists.
        for (
            root_dir,
            local_requirements_paths,
        ) in requirements_files_by_root_dir.items():
            lockfile_sources: List[LockfileDependencySource] = []
            for req_path in sorted(
                local_requirements_paths
            ):  # sorting so that there is a deterministic order in tests
                matching_manifest_path = self._lockfile_to_manifest(
                    req_path, dep_source_files
                )
                if matching_manifest_path is not None:
                    paired_manifests.add(matching_manifest_path)
                lockfile_sources.append(
                    LockfileDependencySource(
                        manifest=(self.manifest_kind, matching_manifest_path),
                        package_manager_type=PackageManagerType.PIP,
                        lockfile_path=req_path,
                    )
                )

            # use the correct dependency source type depending on the number
            # of lockfiles
            dep_source: DependencySource
            if len(lockfile_sources) == 1:
                dep_source = lockfile_sources[0]
            else:
                dep_source = MultiLockfileDependencySource(tuple(lockfile_sources))

            subprojects.append(Subproject(root_dir, dep_source))

        # TODO: (bk) handle lone manifests.
        # there could be lone manifests remaining (manifests - paired_manifests)
        # and this code currently does not handle them. For lockfileless and for
        # ecosystem reporting, we will need to create ManifestOnlyDependencySources
        # from these.

        return subprojects, frozenset(manifests | requirements_files)


# NOTE: the order that these matchers are defined in matters. In find_subprojects, we
# use each dependency source file for at most one matcher, running the matchers in the
# order that they are defined here. This means that if a catch-all matcher were placed
# first, the rest of the matchers would have no chance of matching any subprojects.
MATCHERS: List[SubprojectMatcher] = [
    PipRequirementsMatcher(
        base_file_pattern="*requirement*",
        requirements_file_extensions=["txt", "pip"],
        manifest_file_extension="in",
        default_manifest_file_base="requirements",
    ),
    # Npm
    ExactLockfileManifestMatcher(
        lockfile_name="package-lock.json",
        manifest_name="package.json",
        package_manager_type=PackageManagerType.NPM,
        manifest_kind=out.ManifestKind(out.PackageJson()),
    ),
    ExactLockfileManifestMatcher(
        lockfile_name="yarn.lock",
        manifest_name="package.json",
        package_manager_type=PackageManagerType.YARN,
        manifest_kind=out.ManifestKind(out.PackageJson()),
    ),
    ExactLockfileManifestMatcher(
        lockfile_name="pnpm-lock.yaml",
        manifest_name="package.json",
        package_manager_type=PackageManagerType.PNPM,
        manifest_kind=out.ManifestKind(out.PackageJson()),
    ),
    # Gem
    ExactLockfileManifestMatcher(
        lockfile_name="Gemfile.lock",
        manifest_name="Gemfile",
        package_manager_type=PackageManagerType.RUBY_GEM,
        manifest_kind=out.ManifestKind(out.Gemfile()),
    ),
    # Go modules
    ExactLockfileManifestMatcher(
        lockfile_name="go.mod",
        manifest_name="go.mod",
        package_manager_type=PackageManagerType.GO_MOD,
        manifest_kind=out.ManifestKind(out.GoMod_()),
    ),
    # Cargo
    ExactLockfileManifestMatcher(
        lockfile_name="Cargo.lock",
        manifest_name="Cargo.toml",
        package_manager_type=PackageManagerType.CARGO,
        manifest_kind=out.ManifestKind(out.CargoToml()),
    ),
    # Maven
    ExactLockfileManifestMatcher(
        lockfile_name="maven_dep_tree.txt",
        manifest_name="pom.xml",
        package_manager_type=PackageManagerType.MAVEN,
        manifest_kind=out.ManifestKind(out.PomXml()),
    ),
    ExactLockfileManifestMatcher(
        lockfile_name="gradle.lockfile",
        manifest_name="build.gradle",
        package_manager_type=PackageManagerType.GRADLE,
        manifest_kind=out.ManifestKind(out.BuildGradle()),
    ),
    ExactManifestOnlyMatcher(
        manifest_kind=out.ManifestKind(out.PomXml()),
        manifest_name="pom.xml",
    ),
    ExactManifestOnlyMatcher(
        manifest_kind=out.ManifestKind(out.BuildGradle()),
        manifest_name="build.gradle",
    ),
    # Composer
    ExactLockfileManifestMatcher(
        lockfile_name="composer.lock",
        manifest_name="composer.json",
        package_manager_type=PackageManagerType.COMPOSER,
        manifest_kind=out.ManifestKind(out.ComposerJson()),
    ),
    # Nuget
    ExactLockfileManifestMatcher(
        lockfile_name="packages.lock.json",
        manifest_name="nuget.manifest.json",
        package_manager_type=PackageManagerType.NUGET,
        manifest_kind=out.ManifestKind(out.NugetManifestJson()),
    ),
    # Pub
    ExactLockfileManifestMatcher(
        lockfile_name="pubspec.lock",
        manifest_name="pubspec.yaml",
        package_manager_type=PackageManagerType.DART_PUB,
        manifest_kind=out.ManifestKind(out.PubspecYaml()),
    ),
    # Swift PM
    ExactLockfileManifestMatcher(
        lockfile_name="Package.resolved",
        manifest_name="Package.swift",
        package_manager_type=PackageManagerType.SWIFT_PM,
        manifest_kind=out.ManifestKind(out.PackageSwift_()),
    ),
    # Hex
    ExactLockfileManifestMatcher(
        lockfile_name="mix.lock",
        manifest_name="mix.exs",
        package_manager_type=PackageManagerType.ELIXIR_HEX,
        manifest_kind=out.ManifestKind(out.MixExs()),
    ),
    # Pipenv
    ExactLockfileManifestMatcher(
        lockfile_name="Pipfile.lock",
        manifest_name="Pipfile",
        package_manager_type=PackageManagerType.PIPENV,
        manifest_kind=out.ManifestKind(out.Pipfile_()),
    ),
    # Poetry
    ExactLockfileManifestMatcher(
        lockfile_name="poetry.lock",
        manifest_name="pyproject.toml",
        package_manager_type=PackageManagerType.POETRY,
        manifest_kind=out.ManifestKind(out.PyprojectToml_()),
    ),
]


def filter_dependency_source_files(candidates: FrozenSet[Path]) -> FrozenSet[Path]:
    """
    Returns the paths in `candidates` that are dependency source files.
    """
    return frozenset(path for path in candidates if _is_dependency_source_file(path))


def _is_dependency_source_file(path: Path) -> bool:
    """
    Check if a path is a valid dependency source file (lockfile, manifest, SBOM, etc)
    """
    return any(matcher.is_match(path) for matcher in MATCHERS)
