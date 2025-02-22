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
from semdep.matchers.base import SubprojectMatcher


@dataclass(frozen=True)
class PipRequirementsMatcher(SubprojectMatcher):
    """
    Matcher for requirements lockfiles specifically, which may be nested inside
    a folder called "requirements" instead of being directly at the subproject
    root.

    Also supports multiple lockfiles in a single root directory.
    """

    ECOSYSTEM = out.Ecosystem(out.Pypi())

    # base file pattern is everything except for the last extension. Applies to both
    # requirements and manifests
    base_file_pattern: str  # without extension

    requirements_file_extensions: List[str]
    manifest_file_extension: str

    default_manifest_file_base: str  # without extension
    manifest_kind: out.ManifestKind = field(
        default_factory=lambda: out.ManifestKind(value=out.RequirementsIn())
    )
    lockfile_kind: out.LockfileKind = field(
        default_factory=lambda: out.LockfileKind(value=out.PipRequirementsTxt())
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
    ) -> Tuple[List[out.Subproject], FrozenSet[Path]]:
        # find all manifests and requirements files that we will use to build subprojects
        manifests, requirements_files = self._filter_manifest_requirements(
            dep_source_files
        )

        subprojects: List[out.Subproject] = []

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
            lockfile_sources: List[
                Union[
                    out.LockfileOnlyDependencySource,
                    out.ManifestLockfileDependencySource,
                ]
            ] = []
            for req_path in sorted(
                local_requirements_paths
            ):  # sorting so that there is a deterministic order in tests
                matching_manifest_path = self._lockfile_to_manifest(
                    req_path, dep_source_files
                )
                if matching_manifest_path is not None:
                    paired_manifests.add(matching_manifest_path)
                    manifest = out.Manifest(
                        kind=self.manifest_kind,
                        path=out.Fpath(str(matching_manifest_path)),
                    )
                else:
                    manifest = None

                lockfile = out.Lockfile(
                    kind=self.lockfile_kind, path=out.Fpath(str(req_path))
                )

                if manifest is not None:
                    lockfile_sources.append(
                        out.ManifestLockfileDependencySource((manifest, lockfile))
                    )
                else:
                    lockfile_sources.append(out.LockfileOnlyDependencySource(lockfile))

            # use the correct dependency source type depending on the number
            # of lockfiles
            dep_source: out.DependencySource
            if len(lockfile_sources) == 1:
                dep_source = out.DependencySource(lockfile_sources[0])
            else:
                dep_source = out.DependencySource(
                    out.MultiLockfileDependencySource(
                        [out.DependencySource(x) for x in lockfile_sources]
                    )
                )

            subprojects.append(
                out.Subproject(
                    root_dir=out.Fpath(str(root_dir)),
                    dependency_source=dep_source,
                    ecosystem=self.ECOSYSTEM,
                )
            )

        # TODO: (bk) handle lone manifests.
        # there could be lone manifests remaining (manifests - paired_manifests)
        # and this code currently does not handle them. For lockfileless and for
        # ecosystem reporting, we will need to create ManifestOnlyDependencySources
        # from these.

        return subprojects, frozenset(manifests | requirements_files)
