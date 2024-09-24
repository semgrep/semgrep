from abc import ABC
from abc import abstractmethod
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

from semdep.lockfile import EcosystemLockfiles
from semdep.lockfile import Lockfile
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.target_manager import TargetManager


class DependencySource(ABC):
    @abstractmethod
    def get_lockfile_paths(self) -> List[Path]:
        pass


@dataclass(frozen=True)
class LockfileDependencySource(DependencySource):
    manifest_path: Optional[Path]
    lockfile_path: Path  # required for now, but this will change as we start to support lockfileless scanning

    def get_lockfile_paths(self) -> List[Path]:
        return [self.lockfile_path]


@dataclass(frozen=True)
class MultiLockfileDependencySource(DependencySource):
    sources: List[LockfileDependencySource]

    def get_lockfile_paths(self) -> List[Path]:
        return [source.lockfile_path for source in self.sources]


@dataclass(frozen=True)
class Subproject:
    """
    A subproject, defined by some kind of manifest file (e.g. pyproject.toml, package.json, ...).
    This may be at the root of the repo being scanned or may be some other folder.

    Used as the unit of analysis for supply chain.
    """

    # the root of the subproject
    root_dir: Path

    ecosystem: Ecosystem

    # the dependency source is how we resolved the dependencies. This might be a lockfile/manifest pair (the only current one),
    # but in the future it might also be dynamic resolution based on a manifest, an SBOM, or something else
    dependency_source: DependencySource
    found_dependencies: List[FoundDependency]

    def map_lockfile_to_dependencies(self) -> Dict[str, List[FoundDependency]]:
        """
        Returns a mapping of lockfile paths to dependencies found in that lockfile
        """
        lockfile_to_deps = defaultdict(list)

        for dep in self.found_dependencies:
            if dep.lockfile_path is not None:
                lockfile_to_deps[str(dep.lockfile_path.value)].append(dep)
            else:
                # if the dependency doesn't have a lockfile path, we just put it in the root directory
                lockfile_to_deps[
                    str(self.root_dir.joinpath(Path("unknown_lockfile")))
                ].append(dep)

        return dict(lockfile_to_deps)

    def get_lockfile_paths(self) -> List[Path]:
        """
        Returns a list of lockfile paths for this subproject
        """
        return self.dependency_source.get_lockfile_paths()


def find_closest_subproject(
    path: Path, ecosystem: Ecosystem, candidates: List[Subproject]
) -> Optional[Subproject]:
    """
    Find the best SCA project for the given match by looking at the parent path of the match
    and comparing it to the root directories of the provided candidates. The best SCA project is
    the one with the closest root directory to the match.

    ! All provided candidates must have the same ecosystem.

    We also order the candidates by root directory length so that we prefer
    more specific subprojects over more general ones.

    Args:
        path (Path): The path to search for the closest subproject.
        ecosystem (Ecosystem): The ecosystem to search lockfiles for.
        candidates (List[Subproject]): List of candidate subprojects.
    """

    sorted_candidates = sorted(
        candidates, key=lambda x: len(x.root_dir.parts), reverse=True
    )

    for candidate in sorted_candidates:
        for parent in [path, *path.parents]:
            if candidate.root_dir == parent and candidate.ecosystem == ecosystem:
                return candidate

    return None


def _get_lockfiles_by_root_dir(
    target_manager: TargetManager, ecosystem: Ecosystem
) -> Dict[Path, List[Lockfile]]:
    """
    This function returns a mapping of parent paths to lockfiles for the given ecosystem.

    Here we assume if there are multiple lockfiles in the same directory, we should combine the
    dependencies from all of them. This is a simplification that may not be correct in all cases.
    """
    lockfiles = target_manager.get_lockfiles(ecosystem, ignore_baseline_handler=True)

    # group by parent
    lockfiles_by_parent_path: Dict[Path, List[Lockfile]] = {}
    for lockfile in lockfiles:
        parent = lockfile.parent_path
        if parent not in lockfiles_by_parent_path:
            lockfiles_by_parent_path[parent] = []
        lockfiles_by_parent_path[parent].append(lockfile)

    return lockfiles_by_parent_path


def _parse_lockfiles(
    lockfiles: List[Lockfile],
) -> Tuple[List[FoundDependency], List[DependencyParserError], List[Path]]:
    """
    Parse a list of lockfiles and aggregate the results.

    Args:
        lockfiles (List[Lockfile]): A list of Lockfile objects to parse.

    Returns:
        Tuple[List[FoundDependency], List[DependencyParserError], List[Path]]:
        - A list of all found dependencies
        - A list of all parsing errors encountered
        - A list of paths to the parsed lockfiles
    """
    all_deps: List[FoundDependency] = []
    all_parse_errors: List[DependencyParserError] = []
    sca_dependency_targets: List[Path] = []

    for lockfile in lockfiles:
        deps, parse_errors = lockfile.parse()
        all_deps.extend(deps)
        all_parse_errors.extend(parse_errors)
        sca_dependency_targets.append(lockfile.path)

    return all_deps, all_parse_errors, sca_dependency_targets


def _create_dependency_source(lockfiles: List[Lockfile]) -> DependencySource:
    """
    Create a DependencySource based on the number of lockfiles.

    Args:
        lockfiles (List[Lockfile]): A list of Lockfile objects.

    Returns:
        DependencySource: Either a LockfileDependencySource or a MultiLockfileDependencySource.

    Raises:
        ValueError: If no lockfiles are provided.
    """
    if not lockfiles:
        raise ValueError("No lockfiles found")

    if len(lockfiles) == 1:
        return LockfileDependencySource(
            manifest_path=lockfiles[0].manifest_path,
            lockfile_path=lockfiles[0].path,
        )

    return MultiLockfileDependencySource(
        sources=[
            LockfileDependencySource(
                manifest_path=lockfile.manifest_path,
                lockfile_path=lockfile.path,
            )
            for lockfile in lockfiles
        ]
    )


def _process_ecosystem(
    target_manager: TargetManager, ecosystem: Ecosystem
) -> Tuple[List[Subproject], List[DependencyParserError], List[Path]]:
    """
    Process a specific ecosystem to extract dependencies and create subprojects.

    Args:
        target_manager: An object managing the targets for dependency resolution.
        ecosystem (Ecosystem): The ecosystem to process.

    Returns:
        Tuple[List[Subproject], List[DependencyParserError], List[Path]]:
        - A list of resolved Subprojects
        - A list of all parsing errors encountered
        - A list of all dependency target paths
    """
    resolved_deps: List[Subproject] = []
    all_parse_errors: List[DependencyParserError] = []
    all_sca_dependency_targets: List[Path] = []

    for parent_path, lockfiles in _get_lockfiles_by_root_dir(
        target_manager, ecosystem
    ).items():
        all_deps, parse_errors, sca_dependency_targets = _parse_lockfiles(lockfiles)
        dependency_source = _create_dependency_source(lockfiles)

        subproject = Subproject(
            root_dir=parent_path,
            dependency_source=dependency_source,
            ecosystem=ecosystem,
            found_dependencies=all_deps,
        )

        resolved_deps.append(subproject)
        all_parse_errors.extend(parse_errors)
        all_sca_dependency_targets.extend(sca_dependency_targets)

    return resolved_deps, all_parse_errors, all_sca_dependency_targets


def resolve_subprojects(
    target_manager: TargetManager,
    allow_dynamic_resolution: bool = False,
    enable_experimental_requirements: bool = False,
) -> Tuple[Dict[Ecosystem, List[Subproject]], List[DependencyParserError], List[Path]]:
    """
    Identify lockfiles and manifest files to resolve dependency information from the environment

    If `allow_dynamic_resolution` is True, this function may cause projects that are scanned to be built. This may involve:
    - Downloading packages from the internet
    - Executing code that is included in the scanned project or in downloaded packages

    When `allow_dynamic_resolution` is False, dependencies are resolved only by parsing existing files (lockfiles and manifests).
    """
    resolved_deps: Dict[Ecosystem, List[Subproject]] = {}
    dependency_parser_errors: List[DependencyParserError] = []

    # targets that were considered in generating the dependency tree
    sca_dependency_targets: List[Path] = []
    if allow_dynamic_resolution:
        raise NotImplementedError
    else:
        # for safety, when `allow_dynamic_resolution` is disabled we follow
        # a lockfile-first approach to resolve projects that matches previous behavior
        if enable_experimental_requirements:
            for ecosystem in EcosystemLockfiles.ecosystem_to_lockfiles.keys():
                (
                    ecosystem_subprojects,
                    parse_errors,
                    dependency_targets,
                ) = _process_ecosystem(target_manager, ecosystem)

                resolved_deps.setdefault(ecosystem, []).extend(ecosystem_subprojects)
                dependency_parser_errors.extend(parse_errors)
                sca_dependency_targets.extend(dependency_targets)
        else:
            for ecosystem in EcosystemLockfiles.ecosystem_to_lockfiles.keys():
                for lockfile in target_manager.get_lockfiles(
                    ecosystem, ignore_baseline_handler=True
                ):
                    deps, parse_errors = lockfile.parse()
                    sca_dependency_targets.append(lockfile.path)
                    subproject = Subproject(
                        root_dir=lockfile.path.parent,
                        dependency_source=LockfileDependencySource(
                            manifest_path=lockfile.manifest_path,
                            lockfile_path=lockfile.path,
                        ),
                        ecosystem=ecosystem,
                        found_dependencies=deps,
                    )

                    if ecosystem not in resolved_deps:
                        resolved_deps[ecosystem] = []
                    resolved_deps[ecosystem].append(subproject)
                    dependency_parser_errors.extend(parse_errors)

    return resolved_deps, dependency_parser_errors, sca_dependency_targets
