import hashlib
from collections import defaultdict
from dataclasses import dataclass
from dataclasses import field
from enum import Enum
from pathlib import Path
from typing import Dict
from typing import Generator
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple
from typing import TypeVar
from typing import Union

import semgrep.semgrep_interfaces.semgrep_output_v1 as out


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


@dataclass(frozen=True)
class ResolvedDependencies:
    # We use this mapping to efficiently find child dependencies from a
    # FoundDependency. We need to store multiple FoundDependencies per
    # package/version pair because a package might come from multiple places
    # in a lockfile
    _dependencies_by_package_version_pair: Dict[
        out.DependencyChild, List[out.FoundDependency]
    ]

    @classmethod
    def from_resolved_interfaces(
        cls, resolved: out.ResolutionOk
    ) -> "ResolvedDependencies":
        return cls.from_found_dependencies(resolved.value[0])

    @classmethod
    def from_found_dependencies(
        cls, found_deps: List[out.FoundDependency]
    ) -> "ResolvedDependencies":
        mapping: Dict[out.DependencyChild, List[out.FoundDependency]] = {}
        for dep in found_deps:
            k = out.DependencyChild(dep.package, dep.version)
            if k not in mapping:
                mapping[k] = []
            mapping[k].append(dep)
        return cls(mapping)

    def iter_found_dependencies(self) -> Generator[out.FoundDependency, None, None]:
        for dep_group in self._dependencies_by_package_version_pair.values():
            for dep in dep_group:
                yield dep

    def make_dependencies_by_source_path(
        self,
    ) -> Tuple[Dict[str, List[out.FoundDependency]], List[out.FoundDependency]]:
        """
        Returns a mapping of lockfile paths to dependencies found in that lockfile.

        Also returns a list of FoundDependencies that did not have a lockfile available.
        """
        lockfile_to_deps: Dict[str, List[out.FoundDependency]] = defaultdict(list)
        unknown_lockfile: List[out.FoundDependency] = []

        for dep in self.iter_found_dependencies():
            if dep.lockfile_path is not None:
                lockfile_to_deps[str(dep.lockfile_path.value)].append(dep)
            else:
                unknown_lockfile.append(dep)

        return dict(lockfile_to_deps), unknown_lockfile

    def pretty_print(self) -> None:
        """
        Print the dependency graph with nice indentation. This is
        only for debugging purposes.
        """

        def pretty_print_dependency(
            dep: out.FoundDependency,
            indent: int = 0,
            already_printed: Optional[Set[out.DependencyChild]] = None,
        ) -> Set[out.DependencyChild]:
            """
            Returns package, version pairs of the dependencies that were printed
            """
            if already_printed is None:
                already_printed = set()

            print(f"{' '*indent*2}- {dep.package}@{dep.version}")

            # if we already printed this dependency once, don't print its children again.
            # depends on mutation of already_printed in child calls to avoid re-printing
            if out.DependencyChild(dep.package, dep.version) in already_printed:
                print(f"{' '*2*(indent + 1)} (already printed)")
                return already_printed
            already_printed.add(out.DependencyChild(dep.package, dep.version))

            if dep.children is not None:
                for child in dep.children:
                    # always take the first child dependencies - we don't care to disambiguate
                    # between deps that have the same package and version
                    child_dep = self._dependencies_by_package_version_pair[child][0]
                    already_printed |= pretty_print_dependency(
                        child_dep, indent + 2, already_printed
                    )

            return already_printed

        print("Direct dependencies (plus children if available)")
        directs = [
            dep
            for dep in self.iter_found_dependencies()
            if dep.transitivity == out.Transitivity(out.Direct())
        ]
        printed_in_graph: set[out.DependencyChild] = set()
        for direct in directs:
            printed_in_graph |= pretty_print_dependency(direct)

        # print any remaining transitives/unknowns that were not already printed
        transitives = [
            dep
            for package_version_pair, dep_group in self._dependencies_by_package_version_pair.items()
            for dep in dep_group
            if dep.transitivity == out.Transitivity(out.Transitive())
            and package_version_pair not in printed_in_graph
        ]
        unknown = [
            dep
            for package_version_pair, dep_group in self._dependencies_by_package_version_pair.items()
            for dep in dep_group
            if dep.transitivity == out.Transitivity(out.Unknown())
            and package_version_pair not in printed_in_graph
        ]
        print("other transitives:")
        for dependency in transitives:
            print(f"- {dependency.package}@{dependency.version}")
        print("other unknown transitivity:")
        for dependency in unknown:
            print(f"- {dependency.package}@{dependency.version}")

    def count(self) -> int:
        """
        Count the number of dependencies
        """
        return sum(1 for _ in self.iter_found_dependencies())


@dataclass(frozen=True, order=True)
class Subproject:
    """
    A subproject, defined by some kind of manifest file (e.g. pyproject.toml,
    package.json, ...). This may be at the root of the repo being scanned or
    may be some other folder.
    Note that the dependency_source field is not used when hashing a Subproject
    so when adding subproject to a set only the (root_dir x ecosystem)
    will be used for comparison.

    Used as the unit of analysis for supply chain.
    """

    # the root of the subproject
    root_dir: Path

    # the ecosystem that the subproject belongs to. This is used to match code
    # files with subprojects. It is necessary to have it here, even before a
    # subproject's dependencies are resolved, in order to decide whether
    # a certain subproject must be resolved given the changes included in a
    # certain diff scan.
    # ecosystem can be None if this subproject is for a package manager whose
    # ecosystem is not yet supported (i.e. one that is identified only for
    # tracking purposes)
    ecosystem: Optional[out.Ecosystem]

    # the dependency source is how we resolved the dependencies. This might be a
    # lockfile/manifest pair (the only current one), but in the future it might
    # also be dynamic resolution based on a manifest, an SBOM, or something else
    # Note that this can be a MultiLockfileDependencySource which
    # contains a List which is not hashable which can lead to runtime
    # errors when adding a subproject in a Set hence the hash=False below
    # Fortunately root_dir x ecosystem is enough to discriminate
    dependency_source: out.DependencySource = field(compare=False, hash=False)

    def to_stats_output(self) -> out.SubprojectStats:
        # subproject id is a hash based on the dependency field paths
        normalized_paths = sorted(
            str(path).strip() for path in get_display_paths(self.dependency_source)
        )
        subproject_id = hashlib.sha256(
            "".join(normalized_paths).encode("utf-8")
        ).hexdigest()

        return out.SubprojectStats(
            subproject_id=subproject_id,
            dependency_sources=to_stats_output(self.dependency_source),
            resolved_stats=None,
        )


class UnresolvedReason(Enum):
    FAILED = "failed"
    SKIPPED = "skipped"
    UNSUPPORTED = "unsupported"


@dataclass(frozen=True)
class UnresolvedSubproject(Subproject):
    """
    A subproject for which resolution was attempted but did not succeed.
    """

    unresolved_reason: UnresolvedReason
    resolution_errors: List[Union[out.ScaResolutionError, out.DependencyParserError]]

    @classmethod
    def from_subproject(
        cls,
        base: Subproject,
        unresolved_reason: UnresolvedReason,
        resolution_errors: Sequence[
            Union[out.DependencyParserError, out.ScaResolutionError]
        ],
    ) -> "UnresolvedSubproject":
        return cls(
            root_dir=base.root_dir,
            dependency_source=base.dependency_source,
            ecosystem=base.ecosystem,
            resolution_errors=list(resolution_errors),
            unresolved_reason=unresolved_reason,
        )


@dataclass(frozen=True)
class ResolvedSubproject(Subproject):
    """
    A subproject plus its resolved set of dependencies.
    """

    ecosystem: out.Ecosystem

    resolution_errors: List[Union[out.DependencyParserError, out.ScaResolutionError]]

    # The resolution method is how we determined the dependencies from the
    # dependency source. This might be lockfile parsing, dependency resolution,
    # SBOM ingest, or something else.
    resolution_method: out.ResolutionMethod

    # the dependencies that were found
    found_dependencies: ResolvedDependencies

    @classmethod
    def from_unresolved(
        cls,
        unresolved: Subproject,
        resolution_method: out.ResolutionMethod,
        resolution_errors: Sequence[
            Union[out.DependencyParserError, out.ScaResolutionError]
        ],
        found_dependencies: List[out.FoundDependency],
        ecosystem: out.Ecosystem,
    ) -> "ResolvedSubproject":
        """
        Note that the ecosystem of the resolved subproject is passed separately. We could read it from the
        unresolved subproject, but by having it separately we can expose the fact that ecosystem must not
        be None in the type signature rather than relying on a runtime check.
        """
        return cls(
            root_dir=unresolved.root_dir,
            dependency_source=unresolved.dependency_source,
            ecosystem=ecosystem,
            resolution_errors=list(resolution_errors),
            found_dependencies=ResolvedDependencies.from_found_dependencies(
                found_dependencies
            ),
            resolution_method=resolution_method,
        )

    def to_stats_output(self) -> out.SubprojectStats:
        base_stats = super().to_stats_output()

        return out.SubprojectStats(
            subproject_id=base_stats.subproject_id,
            dependency_sources=base_stats.dependency_sources,
            resolved_stats=out.DependencyResolutionStats(
                ecosystem=self.ecosystem,
                resolution_method=self.resolution_method,
                dependency_count=self.found_dependencies.count(),
            ),
        )


S = TypeVar("S", bound=Subproject)


def find_closest_subproject(
    path: Path, ecosystem: out.Ecosystem, candidates: List[S]
) -> Optional[S]:
    """
    Attempt to find the best SCA project for the given match by looking at the parent path of the match
    and comparing it to the root directories of the provided candidates. The best subproject is
    the one that matches the given `ecosystem` and whose root directory is the longest prefix of
    the given `path`.

    Note that this function finds the closest subproject, which is likely to be but not necessarily the
    relevant subproject. Many package managers will allow a subproject to be associated with
    a code file in an arbitrary location; potentially entirely outside the subproject's root directory.
    We cannot handle that case without extensive per-package-manager logic, so we assume that each code file
    is associated with the closest subproject up the directory tree

    Args:
        path (Path): The path to search for the closest subproject.
        ecosystem (Ecosystem): The ecosystem to consider subprojects for
        candidates (List[Subproject]): List of candidate subprojects.
    """
    # We order the candidates by root directory length so that we prefer
    # more specific subprojects over more general ones.
    sorted_candidates = sorted(
        candidates, key=lambda x: len(x.root_dir.parts), reverse=True
    )

    for candidate in sorted_candidates:
        for parent in [path, *path.parents]:
            if candidate.root_dir == parent and candidate.ecosystem == ecosystem:
                return candidate

    return None
