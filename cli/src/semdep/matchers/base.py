from abc import ABC
from abc import abstractmethod
from dataclasses import dataclass
from fnmatch import fnmatch
from pathlib import Path
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple
from typing import Union

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.subproject import LockfileOnlyDependencySource
from semgrep.subproject import ManifestLockfileDependencySource
from semgrep.subproject import ManifestOnlyDependencySource
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
    lockfile_kind: out.LockfileKind

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

            lockfile = out.Lockfile(self.lockfile_kind, out.Fpath(str(lockfile_path)))
            dep_source: Union[
                ManifestLockfileDependencySource, LockfileOnlyDependencySource
            ]
            if matching_manifest_path:
                dep_source = ManifestLockfileDependencySource(
                    manifest=out.Manifest(
                        kind=self.manifest_kind,
                        path=out.Fpath(str(matching_manifest_path)),
                    ),
                    lockfile=lockfile,
                )
            else:
                dep_source = LockfileOnlyDependencySource(lockfile)

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
    manifest_name: Optional[str]

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
                manifest=out.Manifest(
                    kind=self.manifest_kind, path=out.Fpath(str(manifest_path))
                ),
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
