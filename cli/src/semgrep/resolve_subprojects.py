from pathlib import Path
from typing import Dict
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple
from typing import Union

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
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
from semdep.subproject_matchers import MATCHERS
from semdep.subproject_matchers import SubprojectMatcher
from semgrep.error import DependencyResolutionError
from semgrep.rpc_call import resolve_dependencies
from semgrep.semgrep_interfaces.semgrep_output_v1 import CargoParser
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.subproject import DependencySource
from semgrep.subproject import LockfileOnlyDependencySource
from semgrep.subproject import ManifestLockfileDependencySource
from semgrep.subproject import ManifestOnlyDependencySource
from semgrep.subproject import MultiLockfileDependencySource
from semgrep.subproject import ResolutionMethod
from semgrep.subproject import ResolvedSubproject
from semgrep.subproject import Subproject
from semgrep.subproject import UnresolvedSubproject
from semgrep.target_manager import TargetManager
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# argument order is lockfile path, manifest path
PARSERS_BY_LOCKFILE_KIND: Dict[out.LockfileKind, SemgrepParser] = {
    out.LockfileKind(out.PipfileLock()): parse_pipfile,
    out.LockfileKind(out.PipRequirementsTxt()): parse_requirements,
    out.LockfileKind(out.PoetryLock_()): parse_poetry,
    out.LockfileKind(out.NpmPackageLockJson()): parse_package_lock,
    out.LockfileKind(out.YarnLock()): parse_yarn,
    out.LockfileKind(out.PnpmLock_()): parse_pnpm,
    out.LockfileKind(out.GemfileLock_()): parse_gemfile,
    out.LockfileKind(out.ComposerLock_()): parse_composer_lock,
    out.LockfileKind(out.GoMod2()): parse_go_mod,
    out.LockfileKind(out.CargoLock()): to_parser(
        parse_cargo, ScaParserName(CargoParser())
    ),
    out.LockfileKind(out.MavenDepTree()): parse_pom_tree,
    out.LockfileKind(out.GradleLockfile_()): parse_gradle,
    out.LockfileKind(out.NugetPackagesLockJson()): parse_packages_lock_c_sharp,
    out.LockfileKind(out.PubspecLock_()): parse_pubspec_lock,
    out.LockfileKind(out.SwiftPackageResolved()): parse_package_resolved,
    out.LockfileKind(out.MixLock_()): parse_mix,
}

ECOSYSTEM_BY_LOCKFILE_KIND: Dict[out.LockfileKind, Ecosystem] = {
    out.LockfileKind(out.PipfileLock()): Ecosystem(out.Pypi()),
    out.LockfileKind(out.PipRequirementsTxt()): Ecosystem(out.Pypi()),
    out.LockfileKind(out.PoetryLock_()): Ecosystem(out.Pypi()),
    out.LockfileKind(out.NpmPackageLockJson()): Ecosystem(out.Npm()),
    out.LockfileKind(out.YarnLock()): Ecosystem(out.Npm()),
    out.LockfileKind(out.PnpmLock_()): Ecosystem(out.Npm()),
    out.LockfileKind(out.GemfileLock_()): Ecosystem(out.Gem()),
    out.LockfileKind(out.ComposerLock_()): Ecosystem(out.Composer()),
    out.LockfileKind(out.GoMod2()): Ecosystem(out.Gomod()),
    out.LockfileKind(out.CargoLock()): Ecosystem(out.Cargo()),
    out.LockfileKind(out.MavenDepTree()): Ecosystem(out.Maven()),
    out.LockfileKind(out.GradleLockfile_()): Ecosystem(out.Maven()),
    out.LockfileKind(out.NugetPackagesLockJson()): Ecosystem(out.Nuget()),
    out.LockfileKind(out.PubspecLock_()): Ecosystem(out.Pub()),
    out.LockfileKind(out.SwiftPackageResolved()): Ecosystem(out.SwiftPM()),
    out.LockfileKind(out.MixLock_()): Ecosystem(out.Mix()),
}

DEPENDENCY_GRAPH_SUPPORTED_MANIFEST_KINDS = [
    out.ManifestKind(out.PomXml()),
    out.ManifestKind(out.BuildGradle()),
]


def _resolve_dependencies_dynamically(
    dependency_source: Union[
        ManifestOnlyDependencySource, ManifestLockfileDependencySource
    ]
) -> Tuple[
    Optional[Tuple[Ecosystem, List[FoundDependency]]],
    Sequence[Union[DependencyParserError, DependencyResolutionError]],
    List[Path],
]:
    """
    Handle the RPC call to resolve dependencies dynamically.
    """
    response = resolve_dependencies([dependency_source.to_semgrep_output()])
    if response is None:
        # we failed to resolve somehow
        # TODO: handle this and generate an error
        return None, [], []
    if len(response) > 1:
        logger.warning(
            f"Too many responses from dynamic dependency resolution RPC. Expected 1, got {len(response)}"
        )
    result = response[0][1]
    if isinstance(result.value, out.ResolutionOk):
        resolved_deps, errors = result.value.value
        # right now we only support lockfileless for the maven ecosystem, so hardcode that here
        # TODO: move this ecosystem identification into the ocaml code when we redo the interface there
        ecosystem = Ecosystem(out.Maven())
        wrapped_errors = [
            DependencyResolutionError(
                type_=e_type,
                dependency_source_file=Path(dependency_source.manifest.path.value),
            )
            for e_type in errors
        ]
        return (
            (ecosystem, resolved_deps),
            wrapped_errors,
            [Path(dependency_source.manifest.path.value)],
        )
    else:
        # some error occured in resolution, track it
        wrapped_errors = [
            DependencyResolutionError(
                type_=e_type,
                dependency_source_file=Path(dependency_source.manifest.path.value),
            )
            for e_type in result.value.value
        ]
        return (None, wrapped_errors, [])


def _resolve_dependency_source(
    dep_source: DependencySource,
    enable_dynamic_resolution: bool = True,
    prioritize_dependency_graph_generation: bool = False,
) -> Tuple[
    Optional[Ecosystem],
    List[FoundDependency],
    Sequence[Union[DependencyParserError, DependencyResolutionError]],
    List[Path],
]:
    """
    Resolve the dependencies in the dependency source. Returns:
    - The ecosystem the resulting dependencies belong to. If there are no dependencies, this value is None
    - The list of FoundDependency objects that were resolved
    - The list of dependency parser errors encountered
    - The list of paths that should be considered dependency targets
    """
    ecosystem: Optional[Ecosystem] = None

    if isinstance(dep_source, LockfileOnlyDependencySource) or isinstance(
        dep_source, ManifestLockfileDependencySource
    ):
        lockfile_path = Path(dep_source.lockfile.path.value)
        parser = PARSERS_BY_LOCKFILE_KIND[dep_source.lockfile.kind]
        ecosystem = ECOSYSTEM_BY_LOCKFILE_KIND[dep_source.lockfile.kind]
        if (
            enable_dynamic_resolution
            and prioritize_dependency_graph_generation
            and isinstance(dep_source, ManifestLockfileDependencySource)
            and dep_source.manifest.kind in DEPENDENCY_GRAPH_SUPPORTED_MANIFEST_KINDS
        ):
            (
                resolved_info,
                new_errors,
                new_targets,
            ) = _resolve_dependencies_dynamically(dep_source)
            manifest_path = Path(dep_source.manifest.path.value)
            if resolved_info is not None:
                # TODO: Reimplement this once more robust error handling for lockfileless resolution is implemented
                new_ecosystem, new_deps = resolved_info
                return new_ecosystem, new_deps, new_errors, new_targets
            else:
                # dynamic resolution failed, fall back to lockfile parsing
                resolved_deps, parse_errors = parser(lockfile_path, manifest_path)
            return ecosystem, resolved_deps, parse_errors, [lockfile_path]
        else:
            resolved_deps, parse_errors = parser(
                lockfile_path,
                Path(dep_source.manifest.path.value)
                if isinstance(dep_source, ManifestLockfileDependencySource)
                else None,
            )
            return ecosystem, resolved_deps, parse_errors, [lockfile_path]
    elif isinstance(dep_source, MultiLockfileDependencySource):
        all_resolved_deps: List[FoundDependency] = []
        all_parse_errors: List[
            Union[DependencyParserError, DependencyResolutionError]
        ] = []
        all_dep_targets: List[Path] = []

        for lockfile_source in dep_source.sources:
            ecosystem, new_deps, new_errors, new_targets = _resolve_dependency_source(
                lockfile_source
            )
            all_resolved_deps.extend(new_deps)
            all_parse_errors.extend(new_errors)
            all_dep_targets.extend(new_targets)

        return ecosystem, all_resolved_deps, all_parse_errors, all_dep_targets
    elif (
        isinstance(dep_source, ManifestOnlyDependencySource)
        and enable_dynamic_resolution
    ):
        resolved_info, new_errors, new_targets = _resolve_dependencies_dynamically(
            dep_source
        )
        if resolved_info is None:
            return None, [], new_errors, new_targets
        new_ecosystem, new_deps = resolved_info
        return new_ecosystem, new_deps, new_errors, new_targets
    else:
        # dependency source type is not supported, do nothing
        return (None, [], [], [])


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
    prioritize_dependency_graph_generation: bool = False,
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
        ecosystem, deps, errors, targets = _resolve_dependency_source(
            to_resolve.dependency_source,
            allow_dynamic_resolution,
            prioritize_dependency_graph_generation,
        )
        dependency_targets.extend(targets)

        if ecosystem is not None:
            # ecosystem is only None when dependency resolution failed in some way
            resolved_subproject = ResolvedSubproject.from_unresolved(
                to_resolve, ResolutionMethod.LOCKFILE_PARSING, errors, deps, ecosystem
            )

            if ecosystem not in resolved:
                resolved[ecosystem] = []
            resolved[ecosystem].append(resolved_subproject)
        else:
            # we were not able to resolve the subproject, so track it as an unresolved subproject
            unresolved.append(UnresolvedSubproject.from_subproject(to_resolve, errors))

    return unresolved, resolved, dependency_targets
