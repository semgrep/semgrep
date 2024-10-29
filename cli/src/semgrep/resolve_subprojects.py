from pathlib import Path
from typing import Dict
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

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
from semgrep.rpc_call import resolve_dependencies
from semgrep.semgrep_interfaces.semgrep_output_v1 import CargoParser
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.subproject import DependencySource
from semgrep.subproject import LockfileDependencySource
from semgrep.subproject import ManifestOnlyDependencySource
from semgrep.subproject import MultiLockfileDependencySource
from semgrep.subproject import PackageManagerType
from semgrep.subproject import ResolutionMethod
from semgrep.subproject import ResolvedSubproject
from semgrep.subproject import Subproject
from semgrep.target_manager import TargetManager
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# argument order is lockfile path, manifest path
PARSERS_BY_PACKAGE_MANAGER_TYPE: Dict[PackageManagerType, SemgrepParser] = {
    PackageManagerType.PIPENV: parse_pipfile,
    PackageManagerType.PIP: parse_requirements,
    PackageManagerType.POETRY: parse_poetry,
    PackageManagerType.NPM: parse_package_lock,
    PackageManagerType.YARN: parse_yarn,
    PackageManagerType.PNPM: parse_pnpm,
    PackageManagerType.RUBY_GEM: parse_gemfile,
    PackageManagerType.COMPOSER: parse_composer_lock,
    PackageManagerType.GO_MOD: parse_go_mod,
    PackageManagerType.CARGO: to_parser(parse_cargo, ScaParserName(CargoParser())),
    PackageManagerType.MAVEN: parse_pom_tree,
    PackageManagerType.GRADLE: parse_gradle,
    PackageManagerType.NUGET: parse_packages_lock_c_sharp,
    PackageManagerType.DART_PUB: parse_pubspec_lock,
    PackageManagerType.SWIFT_PM: parse_package_resolved,
    PackageManagerType.ELIXIR_HEX: parse_mix,
}

ECOSYSTEM_BY_PACKAGE_MANAGER_TYPE: Dict[PackageManagerType, Ecosystem] = {
    PackageManagerType.PIPENV: Ecosystem(out.Pypi()),
    PackageManagerType.PIP: Ecosystem(out.Pypi()),
    PackageManagerType.POETRY: Ecosystem(out.Pypi()),
    PackageManagerType.NPM: Ecosystem(out.Npm()),
    PackageManagerType.YARN: Ecosystem(out.Npm()),
    PackageManagerType.PNPM: Ecosystem(out.Npm()),
    PackageManagerType.RUBY_GEM: Ecosystem(out.Gem()),
    PackageManagerType.COMPOSER: Ecosystem(out.Composer()),
    PackageManagerType.GO_MOD: Ecosystem(out.Gomod()),
    PackageManagerType.CARGO: Ecosystem(out.Cargo()),
    PackageManagerType.MAVEN: Ecosystem(out.Maven()),
    PackageManagerType.GRADLE: Ecosystem(out.Maven()),
    PackageManagerType.NUGET: Ecosystem(out.Nuget()),
    PackageManagerType.DART_PUB: Ecosystem(out.Pub()),
    PackageManagerType.SWIFT_PM: Ecosystem(out.SwiftPM()),
    PackageManagerType.ELIXIR_HEX: Ecosystem(out.Mix()),
}

DEPENDENCY_GRAPH_SUPPORTED_MANIFEST_KINDS = [
    out.ManifestKind(out.PomXml()),
    out.ManifestKind(out.BuildGradle()),
]


def _resolve_dependencies_dynamically(
    manifest_path: Path, manifest_kind: out.ManifestKind
) -> Tuple[
    Optional[Ecosystem], List[FoundDependency], List[DependencyParserError], List[Path]
]:
    """
    Handle the RPC call to resolve dependencies dynamically.
    """
    manifest_arg = out.Manifest(kind=manifest_kind, path=out.Fpath(str(manifest_path)))
    response = resolve_dependencies([manifest_arg])
    if response is None:
        # we failed to resolve somehow
        # TODO: handle this and generate an error
        return (None, [], [], [])
    if len(response) > 1:
        logger.warning(
            f"Too many responses from dynamic dependency resolution RPC. Expected 1, got {len(response)}"
        )
    result = response[0][1]
    if isinstance(result.value, out.ResolutionOk):
        resolved_deps = result.value.value
        # right now we only support lockfileless for the maven ecosystem, so hardcode that here
        # TODO: move this ecosystem identification into the ocaml code when we redo the interface there
        ecosystem = Ecosystem(out.Maven())
        return ecosystem, resolved_deps, [], [manifest_path]
    else:
        # some error occured in resolution
        # TODO: error handling / bubbling up
        return (None, [], [], [])


def _resolve_dependency_source(
    dep_source: DependencySource,
    enable_dynamic_resolution: bool = True,
    prioritize_dependency_graph_generation: bool = False,
) -> Tuple[
    Optional[Ecosystem], List[FoundDependency], List[DependencyParserError], List[Path]
]:
    """
    Resolve the dependencies in the dependency source. Returns:
    - The ecosystem the resulting dependencies belong to. If there are no dependencies, this value is None
    - The list of FoundDependency objects that were resolved
    - The list of dependency parser errors encountered
    - The list of paths that should be considered dependency targets
    """
    ecosystem: Optional[Ecosystem] = None

    if isinstance(dep_source, LockfileDependencySource):
        parser = PARSERS_BY_PACKAGE_MANAGER_TYPE[dep_source.package_manager_type]
        ecosystem = ECOSYSTEM_BY_PACKAGE_MANAGER_TYPE[dep_source.package_manager_type]
        manifest_kind, manifest_path = (
            dep_source.manifest if dep_source.manifest else (None, None)
        )
        if (
            enable_dynamic_resolution
            and prioritize_dependency_graph_generation
            and manifest_path is not None
            and manifest_kind in DEPENDENCY_GRAPH_SUPPORTED_MANIFEST_KINDS
        ):
            (
                ecosystem,
                new_deps,
                new_errors,
                new_targets,
            ) = _resolve_dependencies_dynamically(manifest_path, manifest_kind)
            if ecosystem is not None:
                # TODO: Reimplement this once more robust error handling for lockfileless resolution is implemented
                return ecosystem, new_deps, new_errors, new_targets
            else:
                # dynamic resolution failed, fall back to lockfile parsing
                resolved_deps, parse_errors = parser(
                    dep_source.lockfile_path, manifest_path
                )
            return ecosystem, resolved_deps, parse_errors, [dep_source.lockfile_path]
        else:
            resolved_deps, parse_errors = parser(
                dep_source.lockfile_path, manifest_path
            )
            return ecosystem, resolved_deps, parse_errors, [dep_source.lockfile_path]
    elif isinstance(dep_source, MultiLockfileDependencySource):
        all_resolved_deps: List[FoundDependency] = []
        all_parse_errors: List[DependencyParserError] = []
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
        return _resolve_dependencies_dynamically(
            dep_source.manifest_path, dep_source.manifest_kind
        )

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
    Dict[Ecosystem, List[ResolvedSubproject]], List[DependencyParserError], List[Path]
]:
    """
    Identify lockfiles and manifest files to resolve dependency information from the environment

    If `allow_dynamic_resolution` is True, this function may cause projects that are scanned to be built. This may involve:
    - Downloading packages from the internet
    - Executing code that is included in the scanned project or in downloaded packages

    When `allow_dynamic_resolution` is False, dependencies are resolved only by parsing existing files (lockfiles and manifests).
    """
    dependency_parser_errors: List[DependencyParserError] = []

    # first, find all the subprojects
    dependency_source_files = target_manager.get_all_dependency_source_files(
        ignore_baseline_handler=True
    )
    unresolved_subprojects = find_subprojects(dependency_source_files, MATCHERS)

    # targets that were considered in generating the dependency tree
    dependency_targets: List[Path] = []

    resolved: Dict[Ecosystem, List[ResolvedSubproject]] = {}
    # Dispatch each subproject to a resolver for resolution
    for to_resolve in unresolved_subprojects:
        ecosystem, deps, errors, targets = _resolve_dependency_source(
            to_resolve.dependency_source,
            allow_dynamic_resolution,
            prioritize_dependency_graph_generation,
        )
        dependency_parser_errors.extend(errors)
        dependency_targets.extend(targets)

        if ecosystem is not None:
            # ecosystem is only None when dependency resolution failed in some way
            resolved_subproject = ResolvedSubproject.from_unresolved(
                to_resolve, ResolutionMethod.LOCKFILE_PARSING, deps, ecosystem
            )

            if ecosystem not in resolved:
                resolved[ecosystem] = []
            resolved[ecosystem].append(resolved_subproject)

    return resolved, dependency_parser_errors, dependency_targets
