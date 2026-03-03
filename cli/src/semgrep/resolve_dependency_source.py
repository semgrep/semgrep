#
# Copyright (c) 2024-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
from dataclasses import dataclass
from dataclasses import fields
from difflib import ndiff
from pathlib import Path
from pprint import pformat
from typing import Any
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
from semgrep import telemetry
from semgrep.rpc import RpcSession
from semgrep.rpc_call import resolve_dependencies
from semgrep.sca_subproject_support import ALWAYS_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS
from semgrep.sca_subproject_support import ALWAYS_OCAML_PARSER_SUBPROJECT_KINDS
from semgrep.sca_subproject_support import PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS
from semgrep.sca_subproject_support import PTT_OCAML_PARSER_SUBPROJECT_KINDS
from semgrep.sca_subproject_support import TR_OCAML_RESOLVER_SUBPROJECT_KINDS
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.simple_profiling import simple_profiling
from semgrep.subproject import DependencyResolutionConfig
from semgrep.subproject import get_display_paths
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
    out.LockfileKind(out.GoModLock()): DependencyParser(parse_go_mod),
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
    out.LockfileKind(out.BunLock()): None,  # No parser support yet
}


@dataclass(kw_only=True)
class DependencyResolutionResult:
    deps: Union[
        Tuple[out.ResolutionMethod, List[out.ResolvedDependency]], out.UnresolvedReason
    ]
    errors: Sequence[Union[DependencyParserError, out.ScaResolutionError]]
    targets: List[Path]


def manifest_path_unless_lockfile_only(
    ds: Union[
        out.ManifestOnly,
        out.ManifestLockfile,
        out.LockfileOnly,
    ],
) -> out.Fpath:
    if isinstance(ds, out.LockfileOnly):
        return ds.value.path
    elif isinstance(ds, out.ManifestOnly):
        return ds.value.path
    elif isinstance(ds, out.ManifestLockfile):
        return ds.value[0].path
    else:
        raise TypeError(f"Unexpected dependency_source variant1: {type(ds)}")


def lockfile_path_unless_manifest_only(
    ds: Union[
        out.ManifestOnly,
        out.ManifestLockfile,
        out.LockfileOnly,
    ],
) -> out.Fpath:
    if isinstance(ds, out.LockfileOnly):
        return ds.value.path
    elif isinstance(ds, out.ManifestOnly):
        return ds.value.path
    elif isinstance(ds, out.ManifestLockfile):
        return ds.value[1].path
    else:
        raise TypeError(f"Unexpected dependency_source variant2: {type(ds)}")


def get_lockfile_kind(
    ds: Union[
        out.LockfileOnly,
        out.ManifestLockfile,
        out.ManifestOnly,
        out.MultiLockfile,
    ],
) -> Optional[out.LockfileKind]:
    if isinstance(ds, out.LockfileOnly):
        return ds.value.kind
    elif isinstance(ds, out.ManifestLockfile):
        return ds.value[1].kind
    elif isinstance(ds, out.MultiLockfile):
        kinds: List[Optional[out.LockfileKind]] = list(
            filter(None, [get_lockfile_kind(src.value) for src in ds.value])
        )
        # In practice, there should only be a single lockfile kind.
        return kinds[0] if kinds else None
    elif isinstance(ds, out.ManifestOnly):
        return None


@dataclass(kw_only=True)
class ResolveDependenciesRpcResult:
    """The result of _resolve_dependencies_rpc"""

    new_deps: Optional[List[out.ResolvedDependency]]
    new_errors: Sequence[out.ScaResolutionError]
    new_targets: List[Path]


def _resolve_dependencies_rpc(
    *,
    dep_src: Union[
        out.ManifestOnly,
        out.ManifestLockfile,
        out.LockfileOnly,
    ],
    download_dependency_source_code: bool,
    allow_local_builds: bool,
    package_manager_env: Optional[List[Tuple[str, str]]] = None,
    rpc_session: Optional[RpcSession] = None,
) -> ResolveDependenciesRpcResult:
    """
    Handle the RPC call to resolve dependencies in ocaml
    """
    try:
        if rpc_session:
            ret = rpc_session.call(
                out.FunctionCall(
                    out.CallResolveDependencies(
                        out.ResolveDependenciesParams(
                            [out.DependencySource(dep_src)],
                            download_dependency_source_code,
                            allow_local_builds,
                            package_manager_env=package_manager_env,
                        )
                    )
                ),
                out.RetResolveDependencies,
            )
            response = ret.value if ret else None
        else:
            response = resolve_dependencies(
                [out.DependencySource(dep_src)],
                download_dependency_source_code,
                allow_local_builds,
                package_manager_env=package_manager_env,
            )
    except Exception as e:
        logger.verbose(f"RPC call failed: {e}")
        return ResolveDependenciesRpcResult(
            new_deps=None, new_errors=[], new_targets=[]
        )

    if response is None:
        # we failed to resolve somehow
        # TODO: handle this and generate an error
        return ResolveDependenciesRpcResult(
            new_deps=None, new_errors=[], new_targets=[]
        )
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
        return ResolveDependenciesRpcResult(
            new_deps=resolved_deps,
            new_errors=wrapped_errors,
            new_targets=[Path(lockfile_path_unless_manifest_only(dep_src).value)],
        )
    else:
        # some error occurred in resolution, track it
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
        return ResolveDependenciesRpcResult(
            new_deps=None, new_errors=wrapped_errors, new_targets=[]
        )


def _handle_manifest_only_source(
    dep_source: out.ManifestOnly,
    config: DependencyResolutionConfig,
    rpc_session: Optional[RpcSession] = None,
) -> DependencyResolutionResult:
    """Handle dependency resolution for manifest-only sources."""
    logger.verbose(
        f"Dynamically resolving manifest only path(s): {[str(path) for path in get_display_paths(out.DependencySource(dep_source))]}"
    )

    package_manager_env = (
        list(config.local_build_env.items()) if config.local_build_env else None
    )
    resolved_deps = _resolve_dependencies_rpc(
        dep_src=dep_source,
        download_dependency_source_code=config.download_dependency_source_code,
        allow_local_builds=config.allow_local_builds,
        package_manager_env=package_manager_env,
        rpc_session=rpc_session,
    )
    new_deps = resolved_deps.new_deps
    new_errors = resolved_deps.new_errors
    new_targets = resolved_deps.new_targets

    logger.verbose(
        f"Dynamic resolution result: {new_deps}, {new_errors}, {new_targets}"
    )

    if new_deps is None:
        return DependencyResolutionResult(
            deps=out.UnresolvedReason(out.UnresolvedFailed()),
            errors=new_errors,
            targets=new_targets,
        )
    return DependencyResolutionResult(
        deps=(out.ResolutionMethod(out.DynamicResolution()), new_deps),
        errors=new_errors,
        targets=new_targets,
    )


def _handle_multi_lockfile_source(
    dep_source: out.MultiLockfile,
    config: DependencyResolutionConfig,
    rpc_session: Optional[RpcSession] = None,
) -> DependencyResolutionResult:
    """Handle dependency resolution for sources with multiple lockfiles."""
    all_resolved_deps: List[out.ResolvedDependency] = []
    all_parse_errors: List[Union[DependencyParserError, out.ScaResolutionError]] = []
    all_dep_targets: List[Path] = []

    resolution_methods: Set[out.ResolutionMethod] = set()

    for lockfile_source in dep_source.value:
        # We resolve each lockfile source independently.
        #
        # NOTE(sal): In the case of dynamic resolution, we should try to resolve
        # all the lockfiles together, and then get a single response for all of
        # them. Until then, we'll just resolve each lockfile independently. I am
        # concerned about performance here, but don't have enough data yet.
        res = resolve_dependency_source(
            lockfile_source,
            config,
            rpc_session=rpc_session,
        )
        new_resolved_info = res.deps
        new_errors = res.errors
        new_targets = res.targets
        if not isinstance(new_resolved_info, out.UnresolvedReason):
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

    return DependencyResolutionResult(
        deps=(resolution_method, all_resolved_deps),
        errors=all_parse_errors,
        targets=all_dep_targets,
    )


def _handle_lockfile_source(
    dep_source: Union[out.LockfileOnly, out.ManifestLockfile],
    config: DependencyResolutionConfig,
    rpc_session: Optional[RpcSession] = None,
) -> DependencyResolutionResult:
    """Handle dependency resolution for lockfile-based sources."""
    logger.verbose(
        f"\nResolving dependencies for lockfile-based subproject {dep_source.to_json()}"
    )
    lockfile = (
        dep_source.value
        if isinstance(dep_source, out.LockfileOnly)
        else dep_source.value[1]
    )
    lockfile_path = Path(lockfile.path.value)
    parser = PARSERS_BY_LOCKFILE_KIND.get(lockfile.kind, None)

    manifest_kind = (
        dep_source.value[0].kind
        if isinstance(dep_source, out.ManifestLockfile)
        else None
    )
    lockfile_kind = lockfile.kind

    use_nondynamic_ocaml_parsing = (
        (
            config.ptt_enabled
            and (manifest_kind, lockfile_kind) in PTT_OCAML_PARSER_SUBPROJECT_KINDS
        )
        or (manifest_kind, lockfile_kind) in ALWAYS_OCAML_PARSER_SUBPROJECT_KINDS
        or config.use_experimental_ocaml_parsers
    )

    use_dynamic_resolution = (
        config.ptt_enabled
        and config.allow_local_builds
        and (manifest_kind, lockfile_kind) in PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS
    )

    # If TR is requested (--x-tr = download_dependency_source_code (rename?))
    # then prefer to use the OCaml implementation of dependency resolution
    # when available (appears in TR_OCAML_RESOLVER_SUBPROJECT_KINDS).
    use_ocaml_resolver_for_tr = (
        config.download_dependency_source_code
        and (
            manifest_kind,
            lockfile_kind,
        )
        in TR_OCAML_RESOLVER_SUBPROJECT_KINDS
    )
    resolve_with_ocaml = (
        use_nondynamic_ocaml_parsing
        or use_dynamic_resolution
        or use_ocaml_resolver_for_tr
    )
    # Log the condition that leads us to use the OCaml implementation (RPC):
    if use_nondynamic_ocaml_parsing:
        logger.verbose(f"Using OCaml lockfile/manifest parsing")
    if use_dynamic_resolution:
        logger.verbose(f"Using dynamic dependency resolution")
    if use_ocaml_resolver_for_tr:
        logger.verbose(
            f"Attempting to use OCaml implementation for dependency resolution"
        )

    if resolve_with_ocaml:
        logger.verbose(
            f"Resolving path(s) via RPC to OCaml implementation: {[str(path) for path in get_display_paths(out.DependencySource(dep_source))]}"
        )
        package_manager_env = (
            list(config.local_build_env.items()) if config.local_build_env else None
        )
        resolved_deps = _resolve_dependencies_rpc(
            dep_src=dep_source,
            download_dependency_source_code=use_ocaml_resolver_for_tr,
            allow_local_builds=config.allow_local_builds,
            package_manager_env=package_manager_env,
            rpc_session=rpc_session,
        )
        for error in resolved_deps.new_errors:
            logger.verbose(f"\nRPC error: '{error}'")

        if resolved_deps.new_deps is not None:
            # TODO: Reimplement this once more robust error handling for lockfileless resolution is implemented
            return DependencyResolutionResult(
                deps=(
                    # TODO: move resolution method determination into ocaml
                    out.ResolutionMethod(out.LockfileParsing())
                    if use_nondynamic_ocaml_parsing
                    else out.ResolutionMethod(out.DynamicResolution()),
                    resolved_deps.new_deps,
                ),
                errors=resolved_deps.new_errors,
                targets=resolved_deps.new_targets,
            )
        else:
            logger.verbose(f"Falling back to Python implementation")

            if parser is None:
                logger.verbose(
                    f"No python parser available, marking resolution as failed"
                )
                return DependencyResolutionResult(
                    deps=out.UnresolvedReason(out.UnresolvedFailed()),
                    errors=resolved_deps.new_errors,
                    targets=resolved_deps.new_targets,
                )
    # if there is no parser or ecosystem for the lockfile, we can't resolve it
    # also skip resolving with python parsers is use_experimental_ocaml_parsers
    # is enabled, since this flag means that _only_ ocaml parsers should be used
    if parser is None or config.use_experimental_ocaml_parsers:
        return DependencyResolutionResult(
            deps=out.UnresolvedReason(out.UnresolvedUnsupported()),
            errors=[],
            targets=[],
        )

    # Parse lockfile (used for both standard parsing and as fallback for failed dynamic resolution)
    manifest_path = (
        Path(dep_source.value[0].path.value)
        if isinstance(dep_source, out.ManifestLockfile)
        else None
    )

    logger.verbose(f"Parsing {lockfile_path} and {manifest_path} with Python parser")
    found_dependencies, parse_errors = parser(
        lockfile_path=lockfile_path, manifest_path=manifest_path
    )

    return DependencyResolutionResult(
        deps=(
            out.ResolutionMethod(out.LockfileParsing()),
            [out.ResolvedDependency((fd, None)) for fd in found_dependencies],
        ),
        errors=parse_errors,
        targets=[lockfile_path],
    )


@simple_profiling
@telemetry.trace(telemetry.TraceOwner.SSC)
def resolve_dependency_source(
    dep_source: out.DependencySource,
    config: DependencyResolutionConfig,
    rpc_session: Optional[RpcSession] = None,
) -> DependencyResolutionResult:
    """
    Resolve the dependencies in the dependency source. Returns:
    - The list of ResolvedDependency objects that were resolved
    - The list of dependency parser errors encountered
    - The list of paths that should be considered dependency targets
    """
    dep_source_ = dep_source.value
    span = telemetry.get_current_span()
    span.set_attribute(
        "dependency_sources", [str(x) for x in get_display_paths(dep_source)]
    )

    if isinstance(dep_source_, out.LockfileOnly) or isinstance(
        dep_source_, out.ManifestLockfile
    ):
        return _handle_lockfile_source(dep_source_, config, rpc_session=rpc_session)
    elif isinstance(dep_source_, out.MultiLockfile):
        return _handle_multi_lockfile_source(
            dep_source_,
            config,
            rpc_session=rpc_session,
        )
    elif isinstance(dep_source_, out.ManifestOnly) and (
        (dep_source_.value.kind, None) in ALWAYS_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS
        or (
            config.ptt_enabled
            and (dep_source_.value.kind, None)
            in PTT_DYNAMIC_RESOLUTION_SUBPROJECT_KINDS
        )
        # if we are downloading dependency source code, we always need to use ocaml
        or config.download_dependency_source_code
    ):
        if config.allow_local_builds:
            return _handle_manifest_only_source(
                dep_source_,
                config,
                rpc_session=rpc_session,
            )
        else:
            return DependencyResolutionResult(
                deps=out.UnresolvedReason(out.UnresolvedDisabled()),
                errors=[],
                targets=[],
            )
    else:
        # dependency source type is not supported, do nothing
        return DependencyResolutionResult(
            deps=out.UnresolvedReason(out.UnresolvedUnsupported()),
            errors=[],
            targets=[],
        )


#### Helpers for parity testing between OCaml and Python parsers ##############

IGNORED_FIELDS_BY_LOCKFILE_KIND: Dict[out.LockfileKind, List[str]] = {
    out.LockfileKind(out.NpmPackageLockJson()): [
        # TODO: Remove allowed_hashes parsing support is added to the OCaml parser
        "allowed_hashes",
        # The python parser doesn't record children
        "children",
    ]
}


class ParityReporter:
    def __init__(self) -> None:
        self.issues: list[str] = []

    def check_failed(self, left: Any, right: Any, check: str, label: str) -> None:
        diff = "".join(
            ndiff(
                pformat(left).splitlines(keepends=True),
                pformat(right).splitlines(keepends=True),
            )
        )
        self.add_issue(f"{label} '{check}' check failed: \n{diff}")

    def check_eq(self, left: Any, right: Any, label: str) -> None:
        if left == right:
            return

        self.check_failed(left, right, "==", label)

    def check_gte(self, left: Any, right: Any, label: str) -> None:
        if left >= right:
            return
        self.check_failed(left, right, ">=", label)

    def add_issue(self, issue: str) -> None:
        self.issues.append(issue)

    @property
    def ok(self) -> bool:
        return len(self.issues) == 0

    def report(self) -> str:
        if self.ok:
            return "Parity OK."
        header = f"{len(self.issues)} parity issue(s):"
        return "\n---\n".join([header] + self.issues)


def found_dependency_is_at_least_as_accurate(
    py: out.FoundDependency,
    ml: out.FoundDependency,
    pr: ParityReporter,
    ignored_fields: List[str],
) -> None:
    for f in fields(py):
        if not f.name in ignored_fields:
            pr.check_eq(
                getattr(py, f.name),
                getattr(ml, f.name),
                f"FoundDependency field for {py.package} ({py.version}) '{f.name}'",
            )


def resolved_dependency_is_at_least_as_accurate(
    py: out.ResolvedDependency,
    ml: out.ResolvedDependency,
    pr: ParityReporter,
    ignored_fields: List[str],
) -> None:
    py_found_dep, py_download_dep = py.value
    ml_found_dep, ml_download_dep = ml.value
    found_dependency_is_at_least_as_accurate(
        py_found_dep, ml_found_dep, pr, ignored_fields
    )
    pr.check_eq(py_download_dep, ml_download_dep, "Download dependency")


def name_version_tuple(resolved_dep: out.ResolvedDependency) -> tuple[str, str]:
    found_dep, _ = resolved_dep.value
    return (found_dep.package, found_dep.version)


def get_line_number(resolved_dep: out.ResolvedDependency) -> int:
    found_dep, _ = resolved_dep.value
    return found_dep.line_number or -1


def group_packages_by_name_version_and_line_number(
    l: List[out.ResolvedDependency],
) -> dict[tuple[str, str], dict[int, out.ResolvedDependency]]:
    packages: dict[tuple[str, str], dict[int, out.ResolvedDependency]] = {}
    for r in l:
        nv = name_version_tuple(r)
        if nv not in packages:
            packages[nv] = {}
        n = get_line_number(r)
        packages[nv][n] = r
    return packages


def find_root_package_heuristic(
    resolved_deps: List[out.ResolvedDependency],
) -> Optional[out.ResolvedDependency]:
    """Heuristic to identify root package resolved by Python npm lockfile parser.

    The Python parser for npm lockfile v3 includes the root package, which the
    OCaml parser doesn't. We implement a heuristic to identify the root package
    in the Python parser results, so that we can ignore it in comparisons.
    """
    resolved = sorted(resolved_deps, key=lambda r: get_line_number(r))
    if resolved:
        first, _ = resolved[0].value
        transitive = out.DependencyKind(out.Transitive())
        # NOTE: It's quite unlikely that the first package resolved by other
        # parsers would be a transitive dependency. So, this shouldn't be a
        # problem with other parsers too.
        if (
            not first.allowed_hashes
            and first.resolved_url is None
            and first.transitivity == transitive
        ):
            return resolved[0]
    return None


def dependency_resolution_is_at_least_as_accurate(
    py: DependencyResolutionResult,
    ml: DependencyResolutionResult,
    ds: out.DependencySource,
) -> ParityReporter:
    pr = ParityReporter()

    if isinstance(py.deps, out.UnresolvedReason):
        pr.add_issue("Python parser result is unresolved")
        return pr

    if isinstance(ml.deps, out.UnresolvedReason):
        pr.add_issue("OCaml parser result is unresolved")
        return pr

    py_res_meth, py_resolved = py.deps
    ml_res_meth, ml_resolved = ml.deps

    pr.check_eq(set(py.errors), set(ml.errors), "Parser errors")
    pr.check_eq(set(py.targets), set(ml.targets), "Parser targets")
    pr.check_eq(py_res_meth, ml_res_meth, "Parser resolution method")
    pr.check_gte(len(py_resolved), len(ml_resolved), "Number of resolved dependencies")

    py_resolved_nvs = set(name_version_tuple(r) for r in py_resolved)
    ml_resolved_nvs = set(name_version_tuple(r) for r in ml_resolved)
    py_extra_nvs = py_resolved_nvs - ml_resolved_nvs
    # TODO: This if block exists only to handle the presence of the root
    # package in the Python npm lockfile parser. We can get rid of this
    # entirely once the Python npm lockfile parser is gone!
    if py_extra_nvs:
        pr.check_eq(
            1,
            len(py_extra_nvs),
            "Number of extra name/version pairs in Python parser result",
        )
        # If Python parser has resolved extra name/versions, check if it's the
        # root package (heuristically) and ignore it if so
        root_py = find_root_package_heuristic(py_resolved)
        if root_py:
            nv = name_version_tuple(root_py)
            py_resolved_nvs.remove(nv)

    pr.check_eq(
        py_resolved_nvs,
        ml_resolved_nvs,
        "Resolved dependency name/version pairs",
    )

    line_numbers_py = {get_line_number(r) for r in py_resolved}
    line_numbers_ml = {get_line_number(r) for r in ml_resolved}
    missing_lines = line_numbers_ml - line_numbers_py
    if missing_lines:
        pr.add_issue(
            f"Python parser resolved dependencies at line numbers: {line_numbers_py}\n"
            f"OCaml parser resolved dependencies at line numbers: {line_numbers_ml}.\n"
            f"Python parser is missing resolved dependencies at line numbers {missing_lines}."
        )

    # We assume that the Python parser may produce duplicate resolved
    # dependencies for the same name/version that differ only in line number,
    # while the OCaml parser produces at most one resolved dependency per
    # name/version. So, we group resolved dependencies by name/version and line
    # number, and then check that for each resolved dependency produced by the
    # OCaml parser, there is a resolved dependency produced by the Python
    # parser with the same name/version and line number, and that the resolved
    # dependency from the Python parser is at least as accurate as the one from
    # the OCaml parser.
    group_by_package_py = group_packages_by_name_version_and_line_number(py_resolved)
    group_by_package_ml = group_packages_by_name_version_and_line_number(ml_resolved)
    resolved_pairs = [
        (group_by_package_py[nv][n], r_ml)
        for nv, data in group_by_package_ml.items()
        for n, r_ml in data.items()
        if nv in group_by_package_py and n in group_by_package_py[nv]
    ]

    lockfile_kind = get_lockfile_kind(ds.value)
    ignored_fields = (
        IGNORED_FIELDS_BY_LOCKFILE_KIND.get(lockfile_kind, []) if lockfile_kind else []
    )
    for py_res, ml_res in resolved_pairs:
        resolved_dependency_is_at_least_as_accurate(py_res, ml_res, pr, ignored_fields)

    if not resolved_pairs and ml_resolved_nvs:
        pr.add_issue(
            "No resolved dependencies with matching name/version and line number were found between the two parsers. "
            "This may indicate that the parsers are producing completely different results, or that they are parsing different parts of the file."
        )

    return pr
