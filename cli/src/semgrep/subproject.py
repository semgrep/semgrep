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
import hashlib
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict
from typing import Generator
from typing import List
from typing import Optional
from typing import Tuple
from typing import Union

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.types import Target


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

    # Use only the experimental ocaml parsers. This may
    # produce different results from the python parsers. Note that ocaml parsers
    # may be used even when False when necessary for feature support or when
    # they have been validated.
    use_experimental_ocaml_parsers: bool = False


# A classification of subprojects we use to deterine support for various features.
# Not a perfect classification (doesn't handle multi-lockfile sources), but works
# well enough for current purposes.
SubprojectKind = Union[
    Tuple[out.ManifestKind, None],
    Tuple[None, out.LockfileKind],
    Tuple[out.ManifestKind, out.LockfileKind],
]


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
    d: Dict[out.DependencyChild, List[out.ResolvedDependency]],
) -> Generator[out.ResolvedDependency, None, None]:
    for dep_group in d.values():
        for dep in dep_group:
            yield dep


def iter_found_dependencies(
    d: Dict[out.DependencyChild, List[out.ResolvedDependency]],
) -> Generator[out.FoundDependency, None, None]:
    for x in iter_dependencies(d):
        (found_dep, _) = x.value
        yield found_dep


def count_resolved_dependencies(
    d: Dict[out.DependencyChild, List[out.ResolvedDependency]],
) -> int:
    """
    Count the number of dependencies
    """
    return sum(1 for _ in iter_dependencies(d))


def make_dependencies_by_source_path(
    d: Dict[out.DependencyChild, List[out.ResolvedDependency]],
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
    if isinstance(ds, out.ManifestOnly):
        return [Path(ds.value.path.value)]
    elif isinstance(ds, out.LockfileOnly):
        return [Path(ds.value.path.value)]
    elif isinstance(ds, out.ManifestLockfile):
        # Original implementation shows only the lockfile's display path.
        return [Path(ds.value[1].path.value)]
    elif isinstance(ds, out.MultiLockfile):
        # ds.sources is a list of lockfile_dependency_source variants.
        return [path for src in ds.value for path in get_display_paths(src)]
    else:
        raise TypeError(f"Unexpected dependency_source variant: {type(ds)}")


def get_all_source_files(dep: out.DependencySource) -> List[Path]:
    ds = dep.value
    if isinstance(ds, out.ManifestOnly):
        return [Path(ds.value.path.value)]
    elif isinstance(ds, out.LockfileOnly):
        return [Path(ds.value.path.value)]
    elif isinstance(ds, out.ManifestLockfile):
        return [Path(ds.value[0].path.value), Path(ds.value[1].path.value)]
    elif isinstance(ds, out.MultiLockfile):
        return [path for src in ds.value for path in get_all_source_files(src)]
    else:
        raise TypeError(f"Unexpected dependency_source variant: {type(ds)}")


def to_stats_output(dep: out.DependencySource) -> List[out.DependencySourceFile]:
    ds = dep.value
    if isinstance(ds, out.ManifestOnly):
        manifest = ds.value
        return [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(out.Manifest_(manifest.kind)),
                path=manifest.path,
            )
        ]
    elif isinstance(ds, out.LockfileOnly):
        lockfile = ds.value
        return [
            out.DependencySourceFile(
                kind=out.DependencySourceFileKind(out.Lockfile_(lockfile.kind)),
                path=lockfile.path,
            )
        ]
    elif isinstance(ds, out.ManifestLockfile):
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
    elif isinstance(ds, out.MultiLockfile):
        return [item for src in ds.value for item in to_stats_output(src)]
    else:
        raise TypeError(f"Unexpected dependency_source variant: {type(ds)}")


def _generate_subproject_id(sub: out.Subproject) -> str:
    """Generate a consistent ID hash for a subproject based on dependency source paths."""
    normalized_paths = sorted(
        str(path).strip() for path in get_display_paths(sub.dependency_source)
    )
    return hashlib.sha256("".join(normalized_paths).encode("utf-8")).hexdigest()


def _unresolved_subproject_to_stats(
    sub: out.UnresolvedSubproject,
) -> out.SubprojectStats:
    """Convert an unresolved subproject to subproject stats."""
    return out.SubprojectStats(
        subproject_id=_generate_subproject_id(sub.info),
        dependency_sources=to_stats_output(sub.info.dependency_source),
        resolved_stats=None,
        unresolved_reason=sub.reason,
        errors=sub.errors,
    )


def _resolved_subproject_to_stats(sub: out.ResolvedSubproject) -> out.SubprojectStats:
    """Convert a resolved subproject to subproject stats."""
    return out.SubprojectStats(
        subproject_id=_generate_subproject_id(sub.info),
        dependency_sources=to_stats_output(sub.info.dependency_source),
        resolved_stats=out.DependencyResolutionStats(
            ecosystem=sub.ecosystem,
            resolution_method=sub.resolution_method,
            dependency_count=count_resolved_dependencies(sub.resolved_dependencies),
        ),
        unresolved_reason=None,
        errors=sub.errors,
    )


def subproject_to_stats(
    sub: Union[out.ResolvedSubproject, out.UnresolvedSubproject],
) -> out.SubprojectStats:
    """Convert a subproject to subproject stats."""
    if isinstance(sub, out.UnresolvedSubproject):
        return _unresolved_subproject_to_stats(sub)
    elif isinstance(sub, out.ResolvedSubproject):
        return _resolved_subproject_to_stats(sub)
    else:
        raise ValueError(f"Unexpected subproject type: {type(sub)}")


def subproject_to_cli_output_info(
    sub: Union[out.ResolvedSubproject, out.UnresolvedSubproject],
) -> out.CliOutputSubprojectInfo:
    """Convert a subproject to cli output subproject info."""
    stats = subproject_to_stats(sub)
    return out.CliOutputSubprojectInfo(
        dependency_sources=[
            out.Fpath(str(path))
            for path in get_display_paths(sub.info.dependency_source)
        ],
        resolved=isinstance(sub, out.ResolvedSubproject),
        resolved_stats=stats.resolved_stats,
        unresolved_reason=stats.unresolved_reason,
    )


def find_closest_subproject(
    path: Target, ecosystem: out.Ecosystem, candidates: List[out.Subproject]
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
        path (Target): The path to search for the closest subproject.
        ecosystem (Ecosystem): The ecosystem to consider subprojects for
        candidates (List[Subproject]): List of candidate subprojects.
    """
    # We order the candidates by root directory length so that we prefer
    # more specific subprojects over more general ones.
    sorted_candidates = sorted(
        candidates, key=lambda x: len(Path(x.root_dir.value).parts), reverse=True
    )

    for candidate in sorted_candidates:
        for parent in [path, *path.fpath.parents]:
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


def subproject_sort_key(
    subproject: Union[out.ResolvedSubproject, out.UnresolvedSubproject],
) -> Tuple[Tuple[int, int], str]:
    """
    Used to sort subprojects in the table.
    """

    # We want descending order for dependency counts.
    # ResolvedSubprojects should come before UnresolvedSubprojects.
    # So, (0, -count) for resolved, (1, 0) for unresolved.
    if isinstance(subproject, out.ResolvedSubproject):
        dep_count = count_resolved_dependencies(subproject.resolved_dependencies)
        # primary_dep_key: 0 to sort these before unresolved
        # secondary_dep_key: -dep_count for descending numeric sort
        dependency_sort_tuple = (0, -dep_count)
    else:
        # primary_dep_key: 1 to sort these after resolved
        # secondary_dep_key: 0 (doesn't matter as much)
        dependency_sort_tuple = (1, 0)

    # Ascending order for display paths.
    paths = get_display_paths(subproject.info.dependency_source)
    path_key_str = ", ".join(sorted(str(p).lower() for p in paths))

    return (dependency_sort_tuple, path_key_str)


def dep_source_to_subproject_kind(dep_source: out.DependencySource) -> SubprojectKind:
    """
    Determine the "kind" of subproject based on the dependency source.
    """
    ds = dep_source.value
    if isinstance(ds, out.ManifestOnly):
        return (ds.value.kind, None)
    elif isinstance(ds, out.LockfileOnly):
        return (None, ds.value.kind)
    elif isinstance(ds, out.ManifestLockfile):
        return (ds.value[0].kind, ds.value[1].kind)
    elif isinstance(ds, out.MultiLockfile):
        # the SubprojectKind type doesn't really capture all the details
        # of multi-lockfile sources. We just take the first source in the list,
        # which should capture the kind of all of them.
        if len(ds.value) == 0:
            raise ValueError("MultiLockfile with no sources not allowed")
        first_dep_source = ds.value[0]
        if isinstance(first_dep_source, out.MultiLockfile):
            raise ValueError("MultiLockfile inside MultiLockfile not allowed")
        return dep_source_to_subproject_kind(first_dep_source)
    raise ValueError(f"Unexpected dependency_source variant: {type(ds)}")
