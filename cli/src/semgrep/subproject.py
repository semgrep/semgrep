from dataclasses import dataclass
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

from semdep.lockfile import ECOSYSTEM_TO_LOCKFILES
from semdep.lockfile import ExactLockfileMatcher
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.target_manager import TargetManager


@dataclass(frozen=True)
class LockfileDependencySource:
    manifest_path: Optional[Path]
    lockfile_path: Path  # required for now, but this will change as we start to support lockfileless scanning


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
    dependency_source: (
        LockfileDependencySource  # TODO: add more dependency source types
    )
    found_dependencies: List[FoundDependency]


def _find_matching_lockfile(
    current_path: Path,
    ecosystem: Ecosystem,
    candidates_by_lockfile: Dict[Path, Subproject],
) -> Optional[Path]:
    """
    Find a matching lockfile in the given path for the specified ecosystem.

    Args:
        current_path (Path): The path to search for lockfiles.
        ecosystem (Ecosystem): The ecosystem to search lockfiles for.
        candidates_by_lockfile (Dict[Path, Subproject]): Dictionary of candidate lockfiles.

    Returns:
        Optional[Path]: The path of the matching lockfile, or None if no match is found.
    """
    for lockfile_matcher in ECOSYSTEM_TO_LOCKFILES[ecosystem]:
        if isinstance(lockfile_matcher, ExactLockfileMatcher):
            lockfile_path = current_path / lockfile_matcher.lockfile
            if lockfile_path in candidates_by_lockfile:
                return lockfile_path
        else:
            continue
    return None


def find_closest_subproject(
    path: Path, ecosystem: Ecosystem, candidates: List[Subproject]
) -> Optional[Subproject]:
    """
    Find the best SCA project for the given match.

    Determines by lockfile location, matching previous behavior
    of `TargetManager.find_single_lockfile`. Searches up the directory
    tree for files that match lockfile patterns.

    All provided candidates must have the same ecosystem.
    """
    # TODO: (bk) for now, we replicate previous lockfile-based behavior here, since lockfile-sourced
    # dependencies are all that we support and to ensure no behavior change. In the future,
    # we will need to change this logic to be based on root_dir instead of the lockfile location.
    candidates_by_lockfile = {o.dependency_source.lockfile_path: o for o in candidates}

    for path in path.parents:
        matching_lockfile = _find_matching_lockfile(
            path, ecosystem, candidates_by_lockfile
        )
        if matching_lockfile:
            return candidates_by_lockfile[matching_lockfile]
    return None


def resolve_subprojects(
    target_manager: TargetManager, allow_dynamic_resolution: bool = False
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
        for ecosystem in ECOSYSTEM_TO_LOCKFILES.keys():
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
