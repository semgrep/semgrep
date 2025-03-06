import hashlib
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict
from typing import Generator
from typing import List
from typing import Optional
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out


@dataclass(frozen=True)
class DependencyResolutionConfig:
    # Allow resolving dependencies by building projects, installing
    # dependencies, etc. This must be explicitly enabled because it introduces
    # a security risk since it may cause arbitrary code to be executed.
    allow_local_builds: bool

    # Use resolvers that have been updated to parse dependency paths from
    # lockfiles and other dependency sources.
    ptt_enabled: bool

    # If true, resolve all found subprojects, even if they are not explicitly
    # targeted. This is used because one customer requires that all subprojects
    # are resolved even for diff scans.
    resolve_untargeted_subprojects: bool

    # download source code for each subproject's dependencies.
    download_dependency_source_code: bool


def from_resolved_dependencies(
    deps: List[out.ResolvedDependency],
) -> Dict[out.DependencyChild, List[out.ResolvedDependency]]:
    mapping: Dict[out.DependencyChild, List[out.ResolvedDependency]] = {}
    for x in deps:
        (dep, downloaded_dep) = x.value
        k = out.DependencyChild(dep.package, dep.version)
        if k not in mapping:
            mapping[k] = []
        mapping[k].append(out.ResolvedDependency((dep, downloaded_dep)))
    return mapping


def iter_dependencies(
    d: Dict[out.DependencyChild, List[out.ResolvedDependency]]
) -> Generator[out.ResolvedDependency, None, None]:
    for dep_group in d.values():
        for dep in dep_group:
            yield dep


def iter_found_dependencies(
    d: Dict[out.DependencyChild, List[out.ResolvedDependency]]
) -> Generator[out.FoundDependency, None, None]:
    for x in iter_dependencies(d):
        (found_dep, _) = x.value
        yield found_dep


def count_resolved_dependencies(
    d: Dict[out.DependencyChild, List[out.ResolvedDependency]]
) -> int:
    """
    Count the number of dependencies
    """
    return sum(1 for _ in iter_dependencies(d))


def make_dependencies_by_source_path(
    d: Dict[out.DependencyChild, List[out.ResolvedDependency]]
) -> Tuple[Dict[str, List[out.FoundDependency]], List[out.FoundDependency]]:
    """
    Returns a mapping of lockfile paths to dependencies found in that lockfile.

    Also returns a list of FoundDependencies that did not have a lockfile
    available.
    """
    lockfile_to_deps: Dict[str, List[out.FoundDependency]] = defaultdict(list)
    unknown_lockfile: List[out.FoundDependency] = []

    for dep in iter_found_dependencies(d):
        if dep.lockfile_path is not None:
            lockfile_to_deps[str(dep.lockfile_path.value)].append(dep)
        else:
            unknown_lockfile.append(dep)

    return dict(lockfile_to_deps), unknown_lockfile


def get_display_paths(dep: out.DependencySource) -> List[Path]:
    ds = dep.value
    if isinstance(ds, out.ManifestOnlyDependencySource):
        return [Path(ds.value.path.value)]
    elif isinstance(ds, out.LockfileOnlyDependencySource):
        return [Path(ds.value.path.value)]
    elif isinstance(ds, out.ManifestLockfileDependencySource):
        # Original implementation shows only the lockfile's display path.
        return [Path(ds.value[1].path.value)]
    elif isinstance(ds, out.MultiLockfileDependencySource):
        # ds.sources is a list of lockfile_dependency_source variants.
        return [path for src in ds.value for path in get_display_paths(src)]
    else:
        raise TypeError(f"Unexpected dependency_source variant: {type(ds)}")


def get_all_source_files(dep: out.DependencySource) -> List[Path]:
    ds = dep.value
    if isinstance(ds, out.ManifestOnlyDependencySource):
        return [Path(ds.value.path.value)]
    elif isinstance(ds, out.LockfileOnlyDependencySource):
        return [Path(ds.value.path.value)]
    elif isinstance(ds, out.ManifestLockfileDependencySource):
        return [Path(ds.value[0].path.value), Path(ds.value[1].path.value)]
    elif isinstance(ds, out.MultiLockfileDependencySource):
        return [path for src in ds.value for path in get_all_source_files(src)]
    else:
        raise TypeError(f"Unexpected dependency_source variant: {type(ds)}")


def to_stats_output(dep: out.DependencySource) -> List[out.DependencySourceFile]:
    ds = dep.value
    if isinstance(ds, out.ManifestOnlyDependencySource):
        manifest = ds.value
        return [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(out.Manifest_(manifest.kind)),
                path=manifest.path,
            )
        ]
    elif isinstance(ds, out.LockfileOnlyDependencySource):
        lockfile = ds.value
        return [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(out.Lockfile_(lockfile.kind)),
                path=lockfile.path,
            )
        ]
    elif isinstance(ds, out.ManifestLockfileDependencySource):
        lockfile_entry = out.DependencySourceFile(
            kind=out.DependencySourceFileKind(
                value=out.Lockfile_(value=ds.value[1].kind)
            ),
            path=ds.value[1].path,
        )
        manifest_entry = out.DependencySourceFile(
            kind=out.DependencySourceFileKind(
                value=out.Manifest_(value=ds.value[0].kind)
            ),
            path=ds.value[0].path,
        )
        return [lockfile_entry, manifest_entry]
    elif isinstance(ds, out.MultiLockfileDependencySource):
        return [item for src in ds.value for item in to_stats_output(src)]
    else:
        raise TypeError(f"Unexpected dependency_source variant: {type(ds)}")


def subproject_to_stats(sub: out.Subproject) -> out.SubprojectStats:
    # subproject id is a hash based on the dependency field paths
    normalized_paths = sorted(
        str(path).strip() for path in get_display_paths(sub.dependency_source)
    )
    subproject_id = hashlib.sha256(
        "".join(normalized_paths).encode("utf-8")
    ).hexdigest()

    return out.SubprojectStats(
        subproject_id=subproject_id,
        dependency_sources=to_stats_output(sub.dependency_source),
        resolved_stats=None,
    )


def resolved_subproject_to_stats(sub: out.ResolvedSubproject) -> out.SubprojectStats:
    base_stats = subproject_to_stats(sub.info)
    return out.SubprojectStats(
        subproject_id=base_stats.subproject_id,
        dependency_sources=base_stats.dependency_sources,
        resolved_stats=out.DependencyResolutionStats(
            ecosystem=sub.ecosystem,
            resolution_method=sub.resolution_method,
            dependency_count=count_resolved_dependencies(sub.resolved_dependencies),
        ),
    )


def find_closest_subproject(
    path: Path, ecosystem: out.Ecosystem, candidates: List[out.Subproject]
) -> Optional[out.Subproject]:
    """
    Attempt to find the best SCA project for the given match by looking at the
    parent path of the match and comparing it to the root directories of the
    provided candidates. The best subproject is the one that matches the given
    `ecosystem` and whose root directory is the longest prefix of the given
    `path`.

    Note that this function finds the closest subproject, which is likely to be
    but not necessarily the relevant subproject. Many package managers will
    allow a subproject to be associated with a code file in an arbitrary
    location; potentially entirely outside the subproject's root directory.
    We cannot handle that case without extensive per-package-manager logic, so
    we assume that each code file is associated with the closest subproject up
    the directory tree

    Args:
        path (Path): The path to search for the closest subproject.
        ecosystem (Ecosystem): The ecosystem to consider subprojects for
        candidates (List[Subproject]): List of candidate subprojects.
    """
    # We order the candidates by root directory length so that we prefer
    # more specific subprojects over more general ones.
    sorted_candidates = sorted(
        candidates, key=lambda x: len(Path(x.root_dir.value).parts), reverse=True
    )

    for candidate in sorted_candidates:
        for parent in [path, *path.parents]:
            if (
                Path(candidate.root_dir.value) == parent
                and candidate.ecosystem == ecosystem
            ):
                return candidate

    return None


def find_closest_resolved_subproject(
    path: Path, ecosystem: out.Ecosystem, candidates: List[out.ResolvedSubproject]
) -> Optional[out.ResolvedSubproject]:
    """ """
    sorted_candidates = sorted(
        candidates, key=lambda x: len(Path(x.info.root_dir.value).parts), reverse=True
    )

    for candidate in sorted_candidates:
        for parent in [path, *path.parents]:
            if (
                Path(candidate.info.root_dir.value) == parent
                and candidate.ecosystem == ecosystem
            ):
                return candidate

    return None
