from pathlib import Path
from typing import Dict
from typing import FrozenSet
from typing import List
from typing import Set
from typing import Tuple

from semdep.subproject_matchers import MATCHERS
from semdep.subproject_matchers import SubprojectMatcher
from semgrep.resolve_dependency_source import resolve_dependency_source
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.subproject import ResolvedSubproject
from semgrep.subproject import Subproject
from semgrep.subproject import UnresolvedSubproject
from semgrep.target_manager import TargetManager
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def find_subprojects(
    dependency_source_files: FrozenSet[Path], matchers: List[SubprojectMatcher]
) -> List[Subproject]:
    """
    Using the given dependency source files and the given list of matchers, return all the subprojects that could be
    created. Note that each dependency source file will be used by at most one matcher, and matching will be attempted
    in the order that the matchers are provided.
    """
    unresolved_subprojects: List[Subproject] = []
    used_files: Set[Path] = set()
    for matcher in matchers:
        # for each matcher, pass only those files that have not yet been used by another matcher.
        new_subprojects, new_used_files = matcher.make_subprojects(
            dependency_source_files - used_files
        )
        used_files |= new_used_files
        unresolved_subprojects.extend(new_subprojects)
    return unresolved_subprojects


def resolve_subprojects(
    target_manager: TargetManager,
    allow_dynamic_resolution: bool = False,
    ptt_enabled: bool = False,
) -> Tuple[
    List[UnresolvedSubproject], Dict[Ecosystem, List[ResolvedSubproject]], List[Path]
]:
    """
    Identify lockfiles and manifest files to resolve dependency information from the environment

    If `allow_dynamic_resolution` is True, this function may cause projects that are scanned to be built. This may involve:
    - Downloading packages from the internet
    - Executing code that is included in the scanned project or in downloaded packages

    When `allow_dynamic_resolution` is False, dependencies are resolved only by parsing existing files (lockfiles and manifests).

    Returns a tuple with the following items:
        1. Unresolved subprojects
        2. Resolved subprojects, grouped by ecosystem
        4. Dependency source paths that were used in the resolution process
    """
    # first, find all the subprojects
    dependency_source_files = target_manager.get_all_dependency_source_files(
        ignore_baseline_handler=True
    )
    found_subprojects = find_subprojects(dependency_source_files, MATCHERS)

    # targets that were considered in generating the dependency tree
    dependency_targets: List[Path] = []

    resolved: Dict[Ecosystem, List[ResolvedSubproject]] = {}
    unresolved: List[UnresolvedSubproject] = []
    # Dispatch each subproject to a resolver for resolution
    for to_resolve in found_subprojects:
        resolved_info, errors, targets = resolve_dependency_source(
            to_resolve.dependency_source,
            allow_dynamic_resolution,
            ptt_enabled,
        )
        dependency_targets.extend(targets)

        if resolved_info is not None:
            # resolved_info is only None when dependency resolution failed in some way
            ecosystem, resolution_method, deps = resolved_info
            resolved_subproject = ResolvedSubproject.from_unresolved(
                to_resolve, resolution_method, errors, deps, ecosystem
            )

            if ecosystem not in resolved:
                resolved[ecosystem] = []
            resolved[ecosystem].append(resolved_subproject)
        else:
            # we were not able to resolve the subproject, so track it as an unresolved subproject
            unresolved.append(UnresolvedSubproject.from_subproject(to_resolve, errors))

    return unresolved, resolved, dependency_targets
