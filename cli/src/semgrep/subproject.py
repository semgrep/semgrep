from abc import ABC
from abc import abstractmethod
from collections import defaultdict
from dataclasses import dataclass
from enum import auto
from enum import Enum
from pathlib import Path
from typing import Dict
from typing import Generator
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency


class PackageManagerType(Enum):
    PIP = auto()
    POETRY = auto()
    PIPENV = auto()
    NPM = auto()
    YARN = auto()
    PNPM = auto()
    RUBY_GEM = auto()
    GO_MOD = auto()
    CARGO = auto()
    MAVEN = auto()
    GRADLE = auto()
    COMPOSER = auto()
    NUGET = auto()
    DART_PUB = auto()
    SWIFT_PM = auto()
    ELIXIR_HEX = auto()


class ResolutionMethod(Enum):
    LOCKFILE_PARSING = auto()


class DependencySource(ABC):
    @abstractmethod
    def get_display_paths(self) -> List[Path]:
        return []


@dataclass(frozen=True)
class ManifestOnlyDependencySource(DependencySource):
    manifest_path: Path
    # we don't include package manager type for the manifest-only source,
    # because it's possible that we don't know (e.g. package.json, pyproject.toml
    # are both used for multiple package managers.)
    manifest_kind: out.ManifestKind

    def get_display_paths(self) -> List[Path]:
        return [self.manifest_path]


@dataclass(frozen=True)
class PackageManagerDependencySource(DependencySource):
    package_manager_type: PackageManagerType


@dataclass(frozen=True)
class LockfileDependencySource(PackageManagerDependencySource):
    manifest: Tuple[Optional[out.ManifestKind], Optional[Path]]
    lockfile_path: Path

    def get_display_paths(self) -> List[Path]:
        return [self.lockfile_path]


@dataclass(frozen=True)
class MultiLockfileDependencySource(DependencySource):
    # note: we use a tuple instead of a list here to allow hashing the whole type
    sources: Tuple[LockfileDependencySource, ...]

    def get_display_paths(self) -> List[Path]:
        return [source.lockfile_path for source in self.sources]


@dataclass(frozen=True)
class ResolvedDependencies:
    # we use this mapping to efficiently find child dependencies from a FoundDependency
    # We need to store multiple FoundDependencies per package/version pair because a package
    # might come from multiple places in a lockfile
    _dependencies_by_package_version_pair: Dict[
        out.DependencyChild, List[FoundDependency]
    ]

    @classmethod
    def from_resolved_interfaces(
        cls, resolved: out.ResolutionOk
    ) -> "ResolvedDependencies":
        return cls.from_found_dependencies(resolved.value)

    @classmethod
    def from_found_dependencies(
        cls, found_deps: List[FoundDependency]
    ) -> "ResolvedDependencies":
        mapping: Dict[out.DependencyChild, List[FoundDependency]] = {}
        for dep in found_deps:
            k = out.DependencyChild(dep.package, dep.version)
            if k not in mapping:
                mapping[k] = []
            mapping[k].append(dep)
        return cls(mapping)

    def iter_found_dependencies(self) -> Generator[FoundDependency, None, None]:
        for dep_group in self._dependencies_by_package_version_pair.values():
            for dep in dep_group:
                yield dep

    def make_dependencies_by_source_path(
        self,
    ) -> Tuple[Dict[str, List[FoundDependency]], List[FoundDependency]]:
        """
        Returns a mapping of lockfile paths to dependencies found in that lockfile.

        Also returns a list of FoundDependencies that did not have a lockfile available.
        """
        lockfile_to_deps: Dict[str, List[FoundDependency]] = defaultdict(list)
        unknown_lockfile: List[FoundDependency] = []

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
            dep: FoundDependency,
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


@dataclass(frozen=True)
class Subproject:
    """
    A subproject, defined by some kind of manifest file (e.g. pyproject.toml, package.json, ...).
    This may be at the root of the repo being scanned or may be some other folder.

    Used as the unit of analysis for supply chain.
    """

    # the root of the subproject
    root_dir: Path

    # the dependency source is how we resolved the dependencies. This might be a lockfile/manifest pair (the only current one),
    # but in the future it might also be dynamic resolution based on a manifest, an SBOM, or something else
    dependency_source: DependencySource


@dataclass(frozen=True)
class ResolvedSubproject(Subproject):
    """
    A subproject plus its resolved set of dependencies.
    """

    ecosystem: Ecosystem

    # the resolution method is how we determined the dependencies from the dependency source. This might be
    # lockfile parsing, dependency resolution, SBOM ingest, or something else.
    resolution_method: ResolutionMethod

    # the dependencies that were found
    found_dependencies: ResolvedDependencies

    @classmethod
    def from_unresolved(
        cls,
        unresolved: Subproject,
        resolution_method: ResolutionMethod,
        found_dependencies: List[FoundDependency],
        ecosystem: Ecosystem,
    ) -> "ResolvedSubproject":
        return cls(
            root_dir=unresolved.root_dir,
            dependency_source=unresolved.dependency_source,
            ecosystem=ecosystem,
            found_dependencies=ResolvedDependencies.from_found_dependencies(
                found_dependencies
            ),
            resolution_method=resolution_method,
        )


def find_closest_subproject(
    path: Path, ecosystem: Ecosystem, candidates: List[ResolvedSubproject]
) -> Optional[ResolvedSubproject]:
    """
    Find the best SCA project for the given match by looking at the parent path of the match
    and comparing it to the root directories of the provided candidates. The best SCA project is
    the one with the closest root directory to the match that has the provided ecosystem

    ! All provided candidates must have the same ecosystem.

    We also order the candidates by root directory length so that we prefer
    more specific subprojects over more general ones.

    Args:
        path (Path): The path to search for the closest subproject.
        ecosystem (Ecosystem): The ecosystem to search lockfiles for.
        candidates (List[Subproject]): List of candidate subprojects.
    """

    sorted_candidates = sorted(
        candidates, key=lambda x: len(x.root_dir.parts), reverse=True
    )

    for candidate in sorted_candidates:
        for parent in [path, *path.parents]:
            if candidate.root_dir == parent and candidate.ecosystem == ecosystem:
                return candidate

    return None
