from pathlib import Path
from typing import Dict
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
from semdep.parsers.util import SemgrepParser
from semdep.parsers.util import to_parser
from semdep.parsers.yarn import parse_yarn
from semgrep.error import DependencyResolutionError
from semgrep.rpc_call import resolve_dependencies
from semgrep.semgrep_interfaces.semgrep_output_v1 import CargoParser
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import ParseDependenciesFailed
from semgrep.semgrep_interfaces.semgrep_output_v1 import ResolutionError
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.subproject import DependencySource
from semgrep.subproject import LockfileOnlyDependencySource
from semgrep.subproject import ManifestLockfileDependencySource
from semgrep.subproject import ManifestOnlyDependencySource
from semgrep.subproject import MultiLockfileDependencySource
from semgrep.subproject import ResolutionMethod
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


# Maps lockfile kinds to their corresponding parsers. A None value indicates
# we've identified this lockfile format but don't yet have parser support
# for its ecosystem.
#
# argument order is lockfile path, manifest path
PARSERS_BY_LOCKFILE_KIND: Dict[out.LockfileKind, Union[SemgrepParser, None]] = {
    out.LockfileKind(out.PipfileLock()): parse_pipfile,
    out.LockfileKind(out.PipRequirementsTxt()): parse_requirements,
    out.LockfileKind(out.PoetryLock()): parse_poetry,
    out.LockfileKind(out.UvLock()): None,
    out.LockfileKind(out.NpmPackageLockJson()): parse_package_lock,
    out.LockfileKind(out.YarnLock()): parse_yarn,
    out.LockfileKind(out.PnpmLock()): parse_pnpm,
    out.LockfileKind(out.GemfileLock()): parse_gemfile,
    out.LockfileKind(out.ComposerLock()): parse_composer_lock,
    out.LockfileKind(out.GoMod()): parse_go_mod,
    out.LockfileKind(out.CargoLock()): to_parser(
        parse_cargo, ScaParserName(CargoParser())
    ),
    out.LockfileKind(out.MavenDepTree()): parse_pom_tree,
    out.LockfileKind(out.GradleLockfile()): parse_gradle,
    out.LockfileKind(out.NugetPackagesLockJson()): parse_packages_lock_c_sharp,
    out.LockfileKind(out.PubspecLock()): parse_pubspec_lock,
    out.LockfileKind(out.SwiftPackageResolved()): parse_package_resolved,
    out.LockfileKind(out.MixLock()): parse_mix,
    out.LockfileKind(out.ConanLock()): None,  # No parser support yet
}

# Maps lockfile kinds to their corresponding package ecosystems.
#
# A `None` value indicates we've identified this lockfile format but don't yet
# support its ecosystem. If `None`, the dependency source will remain unresolved.
ECOSYSTEM_BY_LOCKFILE_KIND: Dict[out.LockfileKind, Union[Ecosystem, None]] = {
    out.LockfileKind(out.PipfileLock()): Ecosystem(out.Pypi()),
    out.LockfileKind(out.PipRequirementsTxt()): Ecosystem(out.Pypi()),
    out.LockfileKind(out.PoetryLock()): Ecosystem(out.Pypi()),
    out.LockfileKind(out.UvLock()): Ecosystem(out.Pypi()),
    out.LockfileKind(out.NpmPackageLockJson()): Ecosystem(out.Npm()),
    out.LockfileKind(out.YarnLock()): Ecosystem(out.Npm()),
    out.LockfileKind(out.PnpmLock()): Ecosystem(out.Npm()),
    out.LockfileKind(out.GemfileLock()): Ecosystem(out.Gem()),
    out.LockfileKind(out.ComposerLock()): Ecosystem(out.Composer()),
    out.LockfileKind(out.GoMod()): Ecosystem(out.Gomod()),
    out.LockfileKind(out.CargoLock()): Ecosystem(out.Cargo()),
    out.LockfileKind(out.MavenDepTree()): Ecosystem(out.Maven()),
    out.LockfileKind(out.GradleLockfile()): Ecosystem(out.Maven()),
    out.LockfileKind(out.NugetPackagesLockJson()): Ecosystem(out.Nuget()),
    out.LockfileKind(out.PubspecLock()): Ecosystem(out.Pub()),
    out.LockfileKind(out.SwiftPackageResolved()): Ecosystem(out.SwiftPM()),
    out.LockfileKind(out.MixLock()): Ecosystem(out.Mix()),
    out.LockfileKind(out.ConanLock()): None,  # Ecosystem (C++, Conan) not yet supported
}

ECOSYSTEM_BY_MANIFEST_KIND: Dict[out.ManifestKind, Union[Ecosystem, None]] = {
    out.ManifestKind(out.RequirementsIn()): Ecosystem(out.Pypi()),
    out.ManifestKind(out.PackageJson()): Ecosystem(out.Npm()),
    out.ManifestKind(out.Gemfile()): Ecosystem(out.Gem()),
    out.ManifestKind(out.GoMod_()): Ecosystem(out.Gomod()),
    out.ManifestKind(out.CargoToml()): Ecosystem(out.Cargo()),
    out.ManifestKind(out.PomXml()): Ecosystem(out.Maven()),
    out.ManifestKind(out.BuildGradle()): Ecosystem(out.Maven()),
    out.ManifestKind(out.SettingsGradle()): Ecosystem(out.Maven()),
    out.ManifestKind(out.ComposerJson()): Ecosystem(out.Composer()),
    out.ManifestKind(out.NugetManifestJson()): Ecosystem(out.Nuget()),
    out.ManifestKind(out.PubspecYaml()): Ecosystem(out.Pub()),
    out.ManifestKind(out.PackageSwift()): Ecosystem(out.SwiftPM()),
    out.ManifestKind(out.MixExs()): Ecosystem(out.Mix()),
    out.ManifestKind(out.Pipfile()): Ecosystem(out.Pypi()),
    out.ManifestKind(out.PyprojectToml()): Ecosystem(out.Pypi()),
    out.ManifestKind(
        out.ConanFilePy()
    ): None,  # Ecosystem (C++, Conan) not yet supported
}

PTT_OCAML_PARSER_SUBPROJECT_KINDS = [
    (out.ManifestKind(out.PackageJson()), out.LockfileKind(out.NpmPackageLockJson())),
]

PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS = [
    (out.ManifestKind(out.PomXml()), None),
    (out.ManifestKind(out.BuildGradle()), None),
    (out.ManifestKind(out.BuildGradle()), out.LockfileKind(out.GradleLockfile())),
]

DependencyResolutionResult = Tuple[
    Optional[Tuple[Ecosystem, ResolutionMethod, List[FoundDependency]]],
    Sequence[Union[DependencyParserError, DependencyResolutionError]],
    List[Path],
]


def _resolve_dependencies_rpc(
    dependency_source: Union[
        ManifestOnlyDependencySource, ManifestLockfileDependencySource
    ],
) -> Tuple[
    Optional[Tuple[Ecosystem, List[FoundDependency]]],
    Sequence[DependencyResolutionError],
    List[Path],
]:
    """
    Handle the RPC call to resolve dependencies in ocaml
    """
    response = resolve_dependencies([dependency_source.to_semgrep_output()])
    if response is None:
        # we failed to resolve somehow
        # TODO: handle this and generate an error
        return None, [], []
    if len(response) > 1:
        logger.warning(
            f"Too many responses from dependency resolution RPC. Expected 1, got {len(response)}"
        )
    result = response[0][1]
    # TODO: move this ecosystem identification into the ocaml code when we redo the interface there
    ecosystem = (
        ECOSYSTEM_BY_MANIFEST_KIND[dependency_source.manifest.kind]
        if isinstance(dependency_source, ManifestOnlyDependencySource)
        else ECOSYSTEM_BY_LOCKFILE_KIND[dependency_source.lockfile.kind]
    )
    if isinstance(result.value, out.ResolutionOk) and ecosystem is not None:
        resolved_deps, errors = result.value.value

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
            [Path(dependency_source.manifest.path.value)]
            if isinstance(dependency_source, ManifestOnlyDependencySource)
            else [Path(dependency_source.lockfile.path.value)],
        )
    else:
        # some error occured in resolution, track it
        wrapped_errors = (
            [
                DependencyResolutionError(
                    type_=e_type,
                    dependency_source_file=Path(dependency_source.manifest.path.value),
                )
                for e_type in result.value.value
            ]
            if not isinstance(result.value, out.ResolutionOk)
            else [
                # This is here because we have manifest/lockfile kinds for Conan, which we use
                # for data tracking reasons, but SCA doesn't support Conan, and we have no ecosystem
                # for it. Basically this case should never happen, if it does then something went very wrong.
                DependencyResolutionError(
                    type_=ResolutionError(
                        ParseDependenciesFailed(
                            "Trying to use RPC to resolve dependencies from a manifest we don't support"
                        )
                    ),
                    dependency_source_file=Path(dependency_source.manifest.path.value),
                )
            ]
        )
        return (None, wrapped_errors, [])


def _handle_manifest_only_source(
    dep_source: ManifestOnlyDependencySource,
) -> DependencyResolutionResult:
    """Handle dependency resolution for manifest-only sources."""
    resolved_info, new_errors, new_targets = _resolve_dependencies_rpc(dep_source)
    if resolved_info is None:
        return None, new_errors, new_targets
    new_ecosystem, new_deps = resolved_info
    return (
        (new_ecosystem, ResolutionMethod.DYNAMIC, new_deps),
        new_errors,
        new_targets,
    )


def _handle_multi_lockfile_source(
    dep_source: MultiLockfileDependencySource,
) -> DependencyResolutionResult:
    """Handle dependency resolution for sources with multiple lockfiles."""
    all_resolved_deps: List[FoundDependency] = []
    all_parse_errors: List[Union[DependencyParserError, DependencyResolutionError]] = []
    all_dep_targets: List[Path] = []

    resolution_methods: Set[ResolutionMethod] = set()
    ecosystem = None

    for lockfile_source in dep_source.sources:
        new_resolved_info, new_errors, new_targets = resolve_dependency_source(
            lockfile_source
        )
        if new_resolved_info is not None:
            ecosystem, resolution_method, new_deps = new_resolved_info
            resolution_methods.add(resolution_method)
            all_resolved_deps.extend(new_deps)
        all_parse_errors.extend(new_errors)
        all_dep_targets.extend(new_targets)

    if ecosystem is None:
        return None, all_parse_errors, all_dep_targets

    # if any of the files were resolved using dynamic resolution, mark the whole subproject as resolved that way
    resolution_method = (
        ResolutionMethod.DYNAMIC
        if ResolutionMethod.DYNAMIC in resolution_methods
        else ResolutionMethod.LOCKFILE_PARSING
    )

    return (
        (ecosystem, resolution_method, all_resolved_deps),
        all_parse_errors,
        all_dep_targets,
    )


def _handle_lockfile_source(
    dep_source: Union[LockfileOnlyDependencySource, ManifestLockfileDependencySource],
    enable_dynamic_resolution: bool,
    ptt_enabled: bool,
) -> DependencyResolutionResult:
    """Handle dependency resolution for lockfile-based sources."""
    lockfile_path = Path(dep_source.lockfile.path.value)
    parser = PARSERS_BY_LOCKFILE_KIND[dep_source.lockfile.kind]
    ecosystem = ECOSYSTEM_BY_LOCKFILE_KIND[dep_source.lockfile.kind]

    if ptt_enabled and isinstance(dep_source, ManifestLockfileDependencySource):
        use_nondynamic_ocaml_parsing = (
            dep_source.manifest.kind,
            dep_source.lockfile.kind,
        ) in PTT_OCAML_PARSER_SUBPROJECT_KINDS
        use_dynamic_resolution = (
            enable_dynamic_resolution
            and (dep_source.manifest.kind, dep_source.lockfile.kind)
            in PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS
        )
        if use_nondynamic_ocaml_parsing or use_dynamic_resolution:
            (
                resolved_info,
                new_errors,
                new_targets,
            ) = _resolve_dependencies_rpc(dep_source)

            if resolved_info is not None:
                # TODO: Reimplement this once more robust error handling for lockfileless resolution is implemented
                new_ecosystem, new_deps = resolved_info
                return (
                    (new_ecosystem, ResolutionMethod.DYNAMIC, new_deps),
                    new_errors,
                    new_targets,
                )

    # if there is no parser or ecosystem for the lockfile, we can't resolve it
    if parser is None or ecosystem is None:
        return None, [], []

    # Parse lockfile (used for both standard parsing and as fallback for failed dynamic resolution)
    manifest_path = (
        Path(dep_source.manifest.path.value)
        if isinstance(dep_source, ManifestLockfileDependencySource)
        else None
    )

    resolved_deps, parse_errors = parser(lockfile_path, manifest_path)

    return (
        (ecosystem, ResolutionMethod.LOCKFILE_PARSING, resolved_deps),
        parse_errors,
        [lockfile_path],
    )


def resolve_dependency_source(
    dep_source: DependencySource,
    enable_dynamic_resolution: bool = True,
    ptt_enabled: bool = False,
) -> DependencyResolutionResult:
    """
    Resolve the dependencies in the dependency source. Returns:
    - The ecosystem the resulting dependencies belong to. If there are no dependencies, this value is None
    - The list of FoundDependency objects that were resolved
    - The list of dependency parser errors encountered
    - The list of paths that should be considered dependency targets
    """
    if isinstance(dep_source, LockfileOnlyDependencySource) or isinstance(
        dep_source, ManifestLockfileDependencySource
    ):
        return _handle_lockfile_source(
            dep_source,
            enable_dynamic_resolution,
            ptt_enabled,
        )
    elif isinstance(dep_source, MultiLockfileDependencySource):
        return _handle_multi_lockfile_source(dep_source)
    elif (
        isinstance(dep_source, ManifestOnlyDependencySource)
        and enable_dynamic_resolution
        and (dep_source.manifest.kind, None) in PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS
    ):
        return _handle_manifest_only_source(dep_source)
    else:
        # dependency source type is not supported, do nothing
        return (None, [], [])
