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
from semgrep.rpc_call import resolve_dependencies
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.subproject import get_display_paths
from semgrep.subproject import ResolvedDependency
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
    Optional[Tuple[out.ResolutionMethod, List[ResolvedDependency]]],
    Sequence[Union[DependencyParserError, out.ScaResolutionError]],
    List[Path],
]


def manifest_path_unless_lockfile_only(
    ds: Union[
        out.ManifestOnlyDependencySource,
        out.ManifestLockfileDependencySource,
        out.LockfileOnlyDependencySource,
    ]
) -> out.Fpath:
    if isinstance(ds, out.LockfileOnlyDependencySource):
        return ds.value.path
    elif isinstance(ds, out.ManifestOnlyDependencySource):
        return ds.value.path
    elif isinstance(ds, out.ManifestLockfileDependencySource):
        return ds.value[0].path
    else:
        raise TypeError(f"Unexpected dependency_source variant1: {type(ds)}")


def lockfile_path_unless_manifest_only(
    ds: Union[
        out.ManifestOnlyDependencySource,
        out.ManifestLockfileDependencySource,
        out.LockfileOnlyDependencySource,
    ]
) -> out.Fpath:
    if isinstance(ds, out.LockfileOnlyDependencySource):
        return ds.value.path
    elif isinstance(ds, out.ManifestOnlyDependencySource):
        return ds.value.path
    elif isinstance(ds, out.ManifestLockfileDependencySource):
        return ds.value[1].path
    else:
        raise TypeError(f"Unexpected dependency_source variant2: {type(ds)}")


def _resolve_dependencies_rpc(
    dep_src: Union[
        out.ManifestOnlyDependencySource,
        out.ManifestLockfileDependencySource,
        out.LockfileOnlyDependencySource,
    ]
) -> Tuple[
    Optional[List[ResolvedDependency]],
    Sequence[out.ScaResolutionError],
    List[Path],
]:
    """
    Handle the RPC call to resolve dependencies in ocaml
    """
    try:
        response = resolve_dependencies([out.DependencySource(dep_src)])
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
            out.ScaResolutionError(
                type_=e_type,
                dependency_source_file=manifest_path_unless_lockfile_only(dep_src),
            )
            for e_type in errors
        ]
        return (
            resolved_deps,
            wrapped_errors,
            [Path(lockfile_path_unless_manifest_only(dep_src).value)],
        )
    else:
        # some error occured in resolution, track it
        wrapped_errors = (
            [
                out.ScaResolutionError(
                    type_=e_type,
                    dependency_source_file=manifest_path_unless_lockfile_only(dep_src),
                )
                for e_type in result.value.value
            ]
            if not isinstance(result.value, out.ResolutionOk)
            else [
                # This is here because we have manifest/lockfile kinds for Conan, which we use
                # for data tracking reasons, but SCA doesn't support Conan, and we have no ecosystem
                # for it. Basically this case should never happen, if it does then something went very wrong.
                out.ScaResolutionError(
                    type_=out.ResolutionErrorKind(
                        out.ParseDependenciesFailed(
                            "Trying to use RPC to resolve dependencies from a manifest we don't support"
                        )
                    ),
                    dependency_source_file=manifest_path_unless_lockfile_only(dep_src),
                )
            ]
        )
        return (None, wrapped_errors, [])


def _handle_manifest_only_source(
    dep_source: out.ManifestOnlyDependencySource,
) -> DependencyResolutionResult:
    """Handle dependency resolution for manifest-only sources."""
    new_deps, new_errors, new_targets = _resolve_dependencies_rpc(dep_source)
    if new_deps is None:
        return None, new_errors, new_targets
    return (
        (out.ResolutionMethod(out.DynamicResolution()), new_deps),
        new_errors,
        new_targets,
    )


def _handle_multi_lockfile_source(
    dep_source: out.MultiLockfileDependencySource,
    enable_dynamic_resolution: bool,
    ptt_enabled: bool,
) -> DependencyResolutionResult:
    """Handle dependency resolution for sources with multiple lockfiles."""
    all_resolved_deps: List[ResolvedDependency] = []
    all_parse_errors: List[Union[DependencyParserError, out.ScaResolutionError]] = []
    all_dep_targets: List[Path] = []

    resolution_methods: Set[out.ResolutionMethod] = set()

    for lockfile_source in dep_source.value:
        # We resolve each lockfile source independently.
        #
        # NOTE(sal): In the case of dynamic resolution, we should try to resolve
        # all the lockfiles together, and then get a single response for all of
        # them. Until then, I explicitly disable dynamic resolution and
        # path-to-transitivity (PTT) for multi-lockfile sources. They were never
        # enabled in the first place anyway.
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
        out.ResolutionMethod(out.DynamicResolution())
        if out.ResolutionMethod(out.DynamicResolution()) in resolution_methods
        else out.ResolutionMethod(out.LockfileParsing())
    )

    return (
        (resolution_method, all_resolved_deps),
        all_parse_errors,
        all_dep_targets,
    )


def _handle_lockfile_source(
    dep_source: Union[
        out.LockfileOnlyDependencySource, out.ManifestLockfileDependencySource
    ],
    enable_dynamic_resolution: bool,
    ptt_enabled: bool,
) -> DependencyResolutionResult:
    """Handle dependency resolution for lockfile-based sources."""
    lockfile = (
        dep_source.value
        if isinstance(dep_source, out.LockfileOnlyDependencySource)
        else dep_source.value[1]
    )
    lockfile_path = Path(lockfile.path.value)
    parser = PARSERS_BY_LOCKFILE_KIND[lockfile.kind]

    if ptt_enabled:
        manifest_kind = (
            dep_source.value[0].kind
            if isinstance(dep_source, out.ManifestLockfileDependencySource)
            else None
        )
        lockfile_kind = lockfile.kind

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
                f"Dynamically resolving path(s): {[str(path) for path in get_display_paths(out.DependencySource(dep_source))]}"
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
                        out.ResolutionMethod(out.LockfileParsing())
                        if use_nondynamic_ocaml_parsing
                        else out.ResolutionMethod(out.DynamicResolution()),
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
        Path(dep_source.value[0].path.value)
        if isinstance(dep_source, out.ManifestLockfileDependencySource)
        else None
    )

    resolved_deps, parse_errors = parser(lockfile_path, manifest_path)

    return (
        (
            out.ResolutionMethod(out.LockfileParsing()),
            [(fd, None) for fd in resolved_deps],
        ),
        parse_errors,
        [lockfile_path],
    )


def resolve_dependency_source(
    dep_source: out.DependencySource,
    enable_dynamic_resolution: bool = True,
    ptt_enabled: bool = False,
) -> DependencyResolutionResult:
    """
    Resolve the dependencies in the dependency source. Returns:
    - The list of ResolvedDependency objects that were resolved
    - The list of dependency parser errors encountered
    - The list of paths that should be considered dependency targets
    """
    dep_source_ = dep_source.value
    if isinstance(dep_source_, out.LockfileOnlyDependencySource) or isinstance(
        dep_source_, out.ManifestLockfileDependencySource
    ):
        return _handle_lockfile_source(
            dep_source_,
            enable_dynamic_resolution,
            ptt_enabled,
        )
    elif isinstance(dep_source_, out.MultiLockfileDependencySource):
        return _handle_multi_lockfile_source(
            dep_source_,
            enable_dynamic_resolution,
            ptt_enabled,
        )
    elif (
        isinstance(dep_source_, out.ManifestOnlyDependencySource)
        and enable_dynamic_resolution
        and (dep_source_.value.kind, None) in PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS
    ):
        return _handle_manifest_only_source(dep_source_)
    else:
        # dependency source type is not supported, do nothing
        return (None, [], [])
