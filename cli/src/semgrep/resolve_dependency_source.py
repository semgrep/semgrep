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
from semdep.parsers.util import DependencyParser
from semdep.parsers.util import to_parser
from semdep.parsers.yarn import parse_yarn
from semgrep.error import DependencyResolutionError
from semgrep.rpc_call import resolve_dependencies
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import ParseDependenciesFailed
from semgrep.semgrep_interfaces.semgrep_output_v1 import ResolutionError
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
PARSERS_BY_LOCKFILE_KIND: Dict[out.LockfileKind, Union[DependencyParser, None]] = {
    out.LockfileKind(out.PipfileLock()): DependencyParser(parse_pipfile),
    out.LockfileKind(out.PipRequirementsTxt()): DependencyParser(parse_requirements),
    out.LockfileKind(out.PoetryLock()): DependencyParser(parse_poetry),
    out.LockfileKind(out.UvLock()): None,
    out.LockfileKind(out.NpmPackageLockJson()): DependencyParser(parse_package_lock),
    out.LockfileKind(out.YarnLock()): DependencyParser(parse_yarn),
    out.LockfileKind(out.PnpmLock()): DependencyParser(parse_pnpm),
    out.LockfileKind(out.GemfileLock()): DependencyParser(parse_gemfile),
    out.LockfileKind(out.ComposerLock()): DependencyParser(parse_composer_lock),
    out.LockfileKind(out.GoMod()): DependencyParser(parse_go_mod),
    out.LockfileKind(out.CargoLock()): to_parser(parse_cargo),
    out.LockfileKind(out.MavenDepTree()): DependencyParser(parse_pom_tree),
    out.LockfileKind(out.GradleLockfile()): DependencyParser(parse_gradle),
    out.LockfileKind(out.NugetPackagesLockJson()): DependencyParser(
        parse_packages_lock_c_sharp
    ),
    out.LockfileKind(out.PubspecLock()): DependencyParser(parse_pubspec_lock),
    out.LockfileKind(out.SwiftPackageResolved()): DependencyParser(
        parse_package_resolved
    ),
    out.LockfileKind(out.MixLock()): DependencyParser(parse_mix),
    out.LockfileKind(out.ConanLock()): None,  # No parser support yet
}

PTT_OCAML_PARSER_SUBPROJECT_KINDS = [
    (out.ManifestKind(out.PackageJson()), out.LockfileKind(out.NpmPackageLockJson())),
    (out.ManifestKind(out.Csproj()), out.LockfileKind(out.NugetPackagesLockJson())),
]

PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS = [
    (out.ManifestKind(out.PomXml()), None),
    (out.ManifestKind(out.BuildGradle()), None),
    (out.ManifestKind(out.BuildGradle()), out.LockfileKind(out.GradleLockfile())),
    (out.ManifestKind(out.Csproj()), None),
    (
        out.ManifestKind(out.RequirementsIn()),
        out.LockfileKind(out.PipRequirementsTxt()),
    ),
    (
        None,
        out.LockfileKind(out.PipRequirementsTxt()),
    ),
]

DependencyResolutionResult = Tuple[
    Optional[Tuple[ResolutionMethod, List[FoundDependency]]],
    Sequence[Union[DependencyParserError, DependencyResolutionError]],
    List[Path],
]


def _resolve_dependencies_rpc(
    dependency_source: Union[
        ManifestOnlyDependencySource,
        ManifestLockfileDependencySource,
        LockfileOnlyDependencySource,
    ],
) -> Tuple[
    Optional[List[FoundDependency]],
    Sequence[DependencyResolutionError],
    List[Path],
]:
    """
    Handle the RPC call to resolve dependencies in ocaml
    """
    try:
        response = resolve_dependencies([dependency_source.to_semgrep_output()])
    except Exception as e:
        logger.verbose(f"RPC call failed: {e}")
        return None, [], []

    if response is None:
        # we failed to resolve somehow
        # TODO: handle this and generate an error
        return None, [], []
    if len(response) > 1:
        logger.warning(
            f"Too many responses from dependency resolution RPC. Expected 1, got {len(response)}"
        )
    result = response[0][1]
    if isinstance(result.value, out.ResolutionOk):
        resolved_deps, errors = result.value.value

        wrapped_errors = [
            DependencyResolutionError(
                type_=e_type,
                dependency_source_file=Path(
                    dependency_source.lockfile.path.value
                    if isinstance(dependency_source, LockfileOnlyDependencySource)
                    else dependency_source.manifest.path.value
                ),
            )
            for e_type in errors
        ]
        return (
            resolved_deps,
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
                    dependency_source_file=Path(
                        dependency_source.lockfile.path.value
                        if isinstance(dependency_source, LockfileOnlyDependencySource)
                        else dependency_source.manifest.path.value
                    ),
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
                    dependency_source_file=Path(
                        dependency_source.lockfile.path.value
                        if isinstance(dependency_source, LockfileOnlyDependencySource)
                        else dependency_source.manifest.path.value
                    ),
                )
            ]
        )
        return (None, wrapped_errors, [])


def _handle_manifest_only_source(
    dep_source: ManifestOnlyDependencySource,
) -> DependencyResolutionResult:
    """Handle dependency resolution for manifest-only sources."""
    new_deps, new_errors, new_targets = _resolve_dependencies_rpc(dep_source)
    if new_deps is None:
        return None, new_errors, new_targets
    return (
        (ResolutionMethod.DYNAMIC, new_deps),
        new_errors,
        new_targets,
    )


def _handle_multi_lockfile_source(
    dep_source: MultiLockfileDependencySource,
    enable_dynamic_resolution: bool,
    ptt_enabled: bool,
) -> DependencyResolutionResult:
    """Handle dependency resolution for sources with multiple lockfiles."""
    all_resolved_deps: List[FoundDependency] = []
    all_parse_errors: List[Union[DependencyParserError, DependencyResolutionError]] = []
    all_dep_targets: List[Path] = []

    resolution_methods: Set[ResolutionMethod] = set()

    for lockfile_source in dep_source.sources:
        # We resolve each lockfile source independently.
        #
        # NOTE(sal): In the case of dynamic resolution, we should try to resolve all the lockfiles together,
        #            and then get a single response for all of them. Until then, I explicitly disable
        #            dynamic resolution and path-to-transitivity (PTT) for multi-lockfile sources. They were
        #            never enabled in the first place anyway.
        new_resolved_info, new_errors, new_targets = resolve_dependency_source(
            lockfile_source,
            enable_dynamic_resolution=False,
            ptt_enabled=False,
        )
        if new_resolved_info is not None:
            resolution_method, new_deps = new_resolved_info
            resolution_methods.add(resolution_method)
            all_resolved_deps.extend(new_deps)
        all_parse_errors.extend(new_errors)
        all_dep_targets.extend(new_targets)

    # if any of the files were resolved using dynamic resolution, mark the whole subproject as resolved that way. This is sort of an arbitrary choice.
    resolution_method = (
        ResolutionMethod.DYNAMIC
        if ResolutionMethod.DYNAMIC in resolution_methods
        else ResolutionMethod.LOCKFILE_PARSING
    )

    return (
        (resolution_method, all_resolved_deps),
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

    if ptt_enabled:
        manifest_kind = (
            dep_source.manifest.kind
            if isinstance(dep_source, ManifestLockfileDependencySource)
            else None
        )
        lockfile_kind = dep_source.lockfile.kind

        use_nondynamic_ocaml_parsing = (
            manifest_kind,
            lockfile_kind,
        ) in PTT_OCAML_PARSER_SUBPROJECT_KINDS

        use_dynamic_resolution = (
            enable_dynamic_resolution
            and (manifest_kind, lockfile_kind)
            in PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS
        )

        if use_nondynamic_ocaml_parsing or use_dynamic_resolution:
            logger.verbose(
                f"Dynamically resolving path(s): {[str(path) for path in dep_source.get_display_paths()]}"
            )

            (
                new_deps,
                new_errors,
                new_targets,
            ) = _resolve_dependencies_rpc(dep_source)

            for error in new_errors:
                logger.verbose(f"Dynamic resolution RPC error: '{error}'")

            if new_deps is not None:
                # TODO: Reimplement this once more robust error handling for lockfileless resolution is implemented
                return (
                    (
                        ResolutionMethod.LOCKFILE_PARSING
                        if use_nondynamic_ocaml_parsing
                        else ResolutionMethod.DYNAMIC,
                        new_deps,
                    ),
                    new_errors,
                    new_targets,
                )

    # if there is no parser or ecosystem for the lockfile, we can't resolve it
    if parser is None:
        return None, [], []

    # Parse lockfile (used for both standard parsing and as fallback for failed dynamic resolution)
    manifest_path = (
        Path(dep_source.manifest.path.value)
        if isinstance(dep_source, ManifestLockfileDependencySource)
        else None
    )

    resolved_deps, parse_errors = parser(lockfile_path, manifest_path)

    return (
        (ResolutionMethod.LOCKFILE_PARSING, resolved_deps),
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
        return _handle_multi_lockfile_source(
            dep_source,
            enable_dynamic_resolution,
            ptt_enabled,
        )
    elif (
        isinstance(dep_source, ManifestOnlyDependencySource)
        and enable_dynamic_resolution
        and (dep_source.manifest.kind, None) in PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS
    ):
        return _handle_manifest_only_source(dep_source)
    else:
        # dependency source type is not supported, do nothing
        return (None, [], [])
