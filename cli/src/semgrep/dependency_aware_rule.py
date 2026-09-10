#
# Copyright (c) 2022-2025 Semgrep Inc.
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
import dataclasses
import json
import os
from collections import defaultdict
from collections import deque
from functools import lru_cache
from pathlib import Path
from tempfile import mkstemp
from typing import Callable
from typing import Dict
from typing import Iterator
from typing import List
from typing import Optional
from typing import Tuple

import attr
from attr import dataclass
from attr import evolve

import semgrep.rpc_call as rpc_call
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.external.packaging.specifiers import InvalidSpecifier  # type: ignore
from semdep.external.packaging.specifiers import SpecifierSet  # type: ignore
from semdep.matchers.gradle import GradleMatcher
from semdep.package_restrictions import dependencies_range_match_any
from semdep.package_restrictions import is_in_range
from semgrep.dependency_path import DependencyParentIndex
from semgrep.error import SemgrepError
from semgrep.rpc import RpcSession
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.sca_subproject_support import GRADLE_MODULE_ATTRIBUTION_SUBPROJECT_KINDS
from semgrep.sca_subproject_support import TRANSITIVE_REACHABILITY_SUBPROJECT_KINDS
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.simple_profiling import simple_profiling
from semgrep.subproject import dep_source_to_subproject_kind
from semgrep.subproject import find_closest_resolved_subproject
from semgrep.subproject import iter_dependencies
from semgrep.subproject import iter_found_dependencies
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


SCA_FINDING_SCHEMA = 20220913


def parse_depends_on_yaml(entries: List[Dict[str, str]]) -> Iterator[out.ScaPattern]:
    """
    Convert the entries in the Yaml to ProjectDependsOnEntry objects that specify
    namespace, package name, and version ranges. The version format is
    ecosystem-dependent. The current implementation assumes PEP 440 which
    is not fully SemVer-compliant.

    TODO: this is incorrect because version parsing is ecosystem-dependent.
     See the Notion doc about version constraints entitled
     "RFC: Supply Chain Version Constraint Format" or similar.
     The parsing done here is somehow bypassed when checking a version
     against this version range, in file package_restrictions.py,
     function is_in_range.
    """
    for entry in entries:
        # schema checks should guarantee we have these fields, but we'll code defensively
        namespace = entry.get("namespace")
        if namespace is None:
            raise SemgrepError(f"project-depends-on is missing `namespace`")
        try:
            ecosystem = Ecosystem.from_json(namespace.lower())
        except ValueError:
            raise SemgrepError(f"unknown package ecosystem: {namespace}")
        package = entry.get("package")
        if package is None:
            raise SemgrepError(f"project-depends-on is missing `package`")
        semver_range = entry.get("version")
        if semver_range is None:
            raise SemgrepError(f"project-depends-on is missing `version`")
        try:
            SpecifierSet(semver_range)
        except InvalidSpecifier:
            raise SemgrepError(f"invalid semver range {semver_range}")

        # Pypi package names are case insensitive
        if ecosystem == Ecosystem(Pypi()):
            package = package.lower()

        yield out.ScaPattern(
            ecosystem=ecosystem, package=package, semver_range=semver_range
        )


@dataclass
class GradleModuleIndex:
    """
    The dependencies of a Gradle subproject grouped by the module build file
    they were reported from, used to scope reachable findings to the module
    that contains the matching code.

    With gradle_module_attribution on, the resolver reports each module's
    dependencies at that module's build file (`lockfile_path`), mapping a Gradle
    project path such as `:services:api` to the conventional
    `<root>/services/api/build.gradle[.kts]`. This index applies the same
    convention to source files: a file belongs to the nearest enclosing
    directory, up to the subproject root, that holds a Gradle build file. A
    module the resolver could not map to a build file (remapped `projectDir`,
    or no build file of its own) is reported at the root manifest, so both its
    dependencies and its code fall back to the root module. Preserving module
    identity in those cases is tracked separately (SC-4026).
    """

    root_dir: Path
    # Where the resolver reports dependencies of modules without a build file
    manifest_path: Path
    deps_by_build_file: Dict[Path, List[out.FoundDependency]]
    deps_by_key: Dict[Tuple[str, str], List[out.FoundDependency]]
    _build_file_cache: Dict[Path, Optional[Path]] = attr.ib(factory=dict)

    @classmethod
    def from_subproject(
        cls, subproject: out.ResolvedSubproject, deps: List[out.FoundDependency]
    ) -> Optional["GradleModuleIndex"]:
        """
        None unless the subproject is a Gradle build whose dependencies the
        resolver reports per module: a build file with no lockfile. A Gradle
        build resolved from an SBOM or a lockfile keeps the package-level view.
        """
        dependency_source = subproject.info.dependency_source.value
        if not isinstance(dependency_source, out.ManifestOnly) or (
            (dependency_source.value.kind, None)
            not in GRADLE_MODULE_ATTRIBUTION_SUBPROJECT_KINDS
        ):
            return None
        deps_by_build_file: Dict[Path, List[out.FoundDependency]] = defaultdict(list)
        deps_by_key: Dict[Tuple[str, str], List[out.FoundDependency]] = defaultdict(
            list
        )
        for dep in deps:
            if dep.lockfile_path is None:
                continue
            deps_by_build_file[Path(dep.lockfile_path.value)].append(dep)
            deps_by_key[(dep.package, dep.version)].append(dep)
        return cls(
            root_dir=Path(subproject.info.root_dir.value),
            manifest_path=Path(dependency_source.value.path.value),
            deps_by_build_file=dict(deps_by_build_file),
            deps_by_key=dict(deps_by_key),
        )

    def _build_file_in(self, directory: Path) -> Optional[Path]:
        if directory not in self._build_file_cache:
            self._build_file_cache[directory] = next(
                (
                    candidate
                    for candidate in (
                        directory / name for name in GradleMatcher.BUILD_FILENAMES
                    )
                    if candidate in self.deps_by_build_file or candidate.is_file()
                ),
                None,
            )
        return self._build_file_cache[directory]

    def module_build_file(self, path: Path) -> Path:
        """
        The build file of the module that owns the source file at `path`: the
        nearest Gradle build file in its directory or an enclosing one, up to
        the subproject root. A build file counts even when the module has no
        resolved dependencies, so root dependencies do not leak into it. Files
        under no build file belong to the root manifest.
        """
        directory = path.parent
        while True:
            build_file = self._build_file_in(directory)
            if build_file is not None:
                return build_file
            if directory == self.root_dir or directory == directory.parent:
                return self.manifest_path
            directory = directory.parent

    def applicable_dependencies(
        self, path: Path
    ) -> Tuple[List[out.FoundDependency], List[out.FoundDependency]]:
        """
        The dependencies code in `path` can use, as two lists: the owning
        module's own dependencies, then dependencies declared by other modules
        and reached through an explicit Gradle project dependency
        (app -> project(":lib") -> lib's dependencies).

        The resolver keeps a project dependency as a node in the consuming
        module whose children live in the module it points to, so following
        `children` across build files recovers those dependencies. Each child
        names the build file of the instance it refers to. A child without one
        (older resolver output) resolves to the same module's copy when there
        is one, otherwise to every module's copy.

        Two project dependencies can bring in the same package and version
        from two modules (app -> lib and app -> lib2, both using Guava). For
        the code in `path` those copies are interchangeable, so only the first
        one reached is reported: one code match must not turn into several
        reachable findings with the same finding ID. The other copies still
        get their own dependency-only findings. Every copy is still traversed,
        because the same package can resolve to different children in
        different modules.
        """
        own = self.deps_by_build_file.get(self.module_build_file(path), [])
        via_project_dependencies: List[out.FoundDependency] = []
        seen = {id(dep) for dep in own}
        reached: set[Tuple[str, str]] = {(dep.package, dep.version) for dep in own}
        queue = deque(own)
        while queue:
            dep = queue.popleft()
            for child in dep.children or []:
                instances = self.deps_by_key.get((child.package, child.version), [])
                if child.lockfile_path is not None:
                    referenced = [
                        instance
                        for instance in instances
                        if instance.lockfile_path == child.lockfile_path
                    ]
                else:
                    same_module = [
                        instance
                        for instance in instances
                        if instance.lockfile_path == dep.lockfile_path
                    ]
                    referenced = same_module or instances
                for instance in referenced:
                    if id(instance) in seen:
                        continue
                    seen.add(id(instance))
                    queue.append(instance)
                    key = (instance.package, instance.version)
                    if key not in reached:
                        reached.add(key)
                        via_project_dependencies.append(instance)
        return own, via_project_dependencies


@dataclass
class SubprojectDependencyIndex:
    """
    an index to efficiently find version matches within a subproject

    groups dependencies within a subproject by package name, making dependency pattern
    lookups approximately O(1).
    """

    index: dict[str, list[out.FoundDependency]]
    num_deps: int
    # the flat list of dependencies, in resolution order
    deps: list[out.FoundDependency]
    # the same dependencies grouped by Gradle module build file; only built
    # when gradle_module_attribution is on, and None for subprojects the
    # resolver never reports per module
    gradle_modules: Optional[GradleModuleIndex] = None

    @classmethod
    @simple_profiling
    def from_subproject(
        cls,
        subproject: out.ResolvedSubproject,
        gradle_module_attribution: bool = False,
    ) -> "SubprojectDependencyIndex":
        deps = list(iter_found_dependencies(subproject.resolved_dependencies))
        subproject_index: dict[str, list[out.FoundDependency]] = defaultdict(list)
        for dependency in deps:
            subproject_index[dependency.package].append(dependency)

        return cls(
            subproject_index,
            len(deps),
            deps,
            GradleModuleIndex.from_subproject(subproject, deps)
            if gradle_module_attribution
            else None,
        )

    def get_dependency_matches(
        self, sca_patterns: list[out.ScaPattern]
    ) -> Iterator[tuple[out.ScaPattern, out.FoundDependency]]:
        """
        Yields matches to the given sca patterns using the index to compute them.
        """
        for pattern in sca_patterns:
            candidates = self.index.get(pattern.package, [])
            for candidate in candidates:
                if (
                    pattern.ecosystem == candidate.ecosystem
                    and pattern.package == candidate.package
                    and is_in_range(
                        pattern.ecosystem, pattern.semver_range, candidate.version
                    )
                ):
                    yield (pattern, candidate)


# TODO: should be renamed undetermined_or_unreachable_...
#  or handle_transitive_findings
@simple_profiling
def generate_unreachable_sca_findings(
    rule: Rule,
    already_reachable: Callable[[Path, out.FoundDependency], bool],
    dependency_index: dict[
        Ecosystem, list[tuple[out.ResolvedSubproject, SubprojectDependencyIndex]]
    ],
    enable_transitive_reachability: Optional[bool],
    fips_mode: bool,
    write_to_tr_cache: bool = True,
    rpc_session: Optional[RpcSession] = None,
    parent_indexes: Optional[Dict[int, DependencyParentIndex]] = None,
) -> Tuple[List[RuleMatch], List[SemgrepError]]:
    """
    Returns matches to a only a rule's sca-depends-on patterns;
    ignoring any reachabiliy patterns it has.

    :param write_to_tr_cache: Whether to write to the transitive
        reachability cache (/tr_cache endpoint in the app).
    :param rpc_session: allows using a single RPC process for each call to
        transitive_reachability_filter to amortize the overhead of starting
        the process.
    """
    errors: List[SemgrepError] = []
    depends_on_entries = list(parse_depends_on_yaml(rule.project_depends_on))
    ecosystems = list(rule.ecosystems)
    # only populated when --x-dependency-paths is on (see run_scan); a present
    # entry is the signal to emit paths for that subproject.
    parent_indexes = parent_indexes or {}

    non_reachable_matches: List[RuleMatch] = []
    match_based_keys: Dict[tuple[str, Path, str], int] = defaultdict(int)
    for ecosystem in ecosystems:
        for subproject, subproject_dependency_index in dependency_index.get(
            ecosystem, {}
        ):
            subproject_kind = dep_source_to_subproject_kind(
                subproject.info.dependency_source
            )
            parent_index = parent_indexes.get(id(subproject))
            subproject_matches: List[RuleMatch] = []

            dependency_matches: List[Tuple[out.ScaPattern, out.FoundDependency]] = list(
                subproject_dependency_index.get_dependency_matches(depends_on_entries)
            )
            for dep_pat, found_dep in dependency_matches:
                if found_dep.lockfile_path is None:
                    # In rare cases, it's possible for a dependency to not have
                    # a lockfile path. This indicates a dev error and usually
                    # means that the parser did not associate the dep with a
                    # lockfile. So we'll just skip this dependency.
                    logger.warning(
                        f"Found a dependency ({found_dep.package}) without a lockfile path. Skipping..."
                    )
                    continue

                lockfile_path = Path(found_dep.lockfile_path.value)
                # for TR even if we could find a reachable finding in the
                # 1st party code, we could also investigate the 3rd party code
                # but let's KISS for now and just consider undetermined findings
                if already_reachable(lockfile_path, found_dep):
                    continue

                dep_match = out.DependencyMatch(
                    dependency_pattern=dep_pat,
                    found_dependency=found_dep,
                    lockfile=found_dep.lockfile_path,
                    dependency_paths=(parent_index.paths_for(found_dep) or None)
                    if parent_index is not None
                    else None,
                )
                sca_match = out.ScaMatch(
                    sca_finding_schema=SCA_FINDING_SCHEMA,
                    reachable=False,
                    reachability_rule=rule.should_run_on_semgrep_core,
                    dependency_match=dep_match,
                    # TODO: sca_match_kind? put Undetermined for now?
                )
                core_match = out.CoreMatch(
                    check_id=out.RuleId(rule.id),
                    path=found_dep.lockfile_path,
                    start=out.Position(found_dep.line_number or 1, 1),
                    end=out.Position(
                        (found_dep.line_number if found_dep.line_number else 1),
                        1,
                    ),
                    extra=out.CoreMatchExtra(
                        metavars=out.Metavars({}),
                        engine_kind=out.EngineOfFinding(out.OSS()),
                        is_ignored=False,
                        sca_match=sca_match,
                    ),
                )

                rule_match = RuleMatch(
                    match=core_match,
                    message=rule.message,
                    severity=rule.severity,
                    metadata=rule.metadata,
                    fips_mode=fips_mode,
                )
                new_rule_match = evolve(
                    rule_match,
                    match_based_index=match_based_keys[rule_match.match_based_key],
                )
                match_based_keys[rule_match.match_based_key] += 1
                subproject_matches.append(new_rule_match)

            if (
                enable_transitive_reachability
                and subproject_kind in TRANSITIVE_REACHABILITY_SUBPROJECT_KINDS
            ):
                # TODO: consider only the matches with reachable rules
                # For now we run TR only for supported subproject kinds. If TR
                # RPC perf were better, we would ideally remove this duplication
                # of logic and just rely on the RPC to do the right thing regardless
                # of whether the subproject kind is supported.
                transitive_findings = [
                    out.TransitiveFinding(m=rm.match) for rm in subproject_matches
                ]
                if transitive_findings:
                    logger.debug(
                        f"SCA TR is on! Running for rule {rule.id}, subproject {subproject.info.dependency_source}, {len(transitive_findings)} transitive findings"
                    )
                # We serialize as JSON, so the suffix must be `.json` —
                # OCaml's `Parse_rule.parse_file` dispatches purely on file
                # extension. A `.yaml` suffix would route this through
                # `Yaml_to_generic.parse_yaml_file` and parse JSON-as-YAML,
                # which is dramatically slower (and was the historical bug).
                fd, rules_tmp_path = mkstemp(
                    suffix=".json", prefix="semgrep-tr-rules-", text=True
                )
                try:
                    with os.fdopen(fd, "w") as fp:
                        fp.write(json.dumps([rule.raw]))
                    params = out.TransitiveReachabilityFilterParams(
                        rules_path=out.Fpath(rules_tmp_path),
                        findings=transitive_findings,
                        dependencies=list(
                            iter_dependencies(subproject.resolved_dependencies)
                        ),
                        write_to_cache=write_to_tr_cache,
                    )
                    # to debug: print(params.to_json_string())
                    if rpc_session:
                        ret = rpc_session.call(
                            out.FunctionCall(
                                out.CallTransitiveReachabilityFilter(params)
                            ),
                            out.RetTransitiveReachabilityFilter,
                        )
                        tr_filtered_matches = ret.value if ret else transitive_findings
                    else:
                        tr_filtered_matches = rpc_call.transitive_reachability_filter(
                            params
                        )
                finally:
                    os.remove(rules_tmp_path)

                # TODO: associate these in a more robust way. This currently
                # depends on the RPC call returning the same matches in the
                # same order.
                non_reachable_matches.extend(
                    [
                        evolve(rm, match=tm.m)
                        for rm, tm in zip(subproject_matches, tr_filtered_matches)
                    ]
                )
            else:
                non_reachable_matches.extend(subproject_matches)

    return non_reachable_matches, errors


@lru_cache(maxsize=100_000)
def transitive_dep_is_also_direct(
    package: str, deps: Tuple[Tuple[str, out.DependencyKind], ...]
) -> bool:
    """
    Assumes that [dep] is transitive
    Checks if there is a direct version of the transitive dependency [dep]
    """
    return (package, out.DependencyKind(out.Direct())) in deps


@simple_profiling
def generate_reachable_sca_findings(
    matches: List[RuleMatch],
    rule: Rule,
    dependency_index: dict[
        Ecosystem, list[tuple[out.ResolvedSubproject, SubprojectDependencyIndex]]
    ],
    parent_indexes: Optional[Dict[int, DependencyParentIndex]] = None,
    gradle_module_attribution: bool = False,
) -> Tuple[
    List[RuleMatch], List[SemgrepError], Callable[[Path, out.FoundDependency], bool]
]:
    """
    Turn the rule's code matches into reachable findings by pairing each one
    with the matching dependencies of the subproject that contains it.

    Also returns a predicate telling whether a dependency was paired with some
    code match, so that no dependency-only finding is reported for it.

    :param gradle_module_attribution: the resolver reported Gradle dependencies
        per module build file, so a code match only pairs with dependencies
        applicable to its own module (see GradleModuleIndex).
    """
    errors: List[SemgrepError] = []
    depends_on_entries = list(parse_depends_on_yaml(rule.project_depends_on))
    ecosystems = list(rule.ecosystems)
    # only populated when --x-dependency-paths is on (see run_scan); a present
    # entry is the signal to emit paths for that subproject.
    parent_indexes = parent_indexes or {}

    # Reachability rule
    reachable_matches: List[RuleMatch] = []
    reachable_deps = set()
    for ecosystem in ecosystems:
        # The per-subproject index (carrying the flat deps) is built once for
        # the whole scan in run_scan and reused here, rather than rebuilt per
        # rule. We index it by subproject identity to look it back up after
        # matching a code finding's path (same keying as parent_indexes).
        subproject_entries = dependency_index.get(ecosystem, [])
        subprojects = [subproject for subproject, _ in subproject_entries]
        index_by_subproject = {
            id(subproject): subproject_index
            for subproject, subproject_index in subproject_entries
        }
        for rule_match in matches:
            try:
                subproject = find_closest_resolved_subproject(
                    rule_match.path, ecosystem, subprojects
                )
                if subproject is None:
                    continue

                subproject_index = index_by_subproject[id(subproject)]
                parent_index = parent_indexes.get(id(subproject))

                module_index = (
                    subproject_index.gradle_modules
                    if gradle_module_attribution
                    else None
                )
                if module_index is not None:
                    # The module's own dependencies come first. Dependencies
                    # reached through a project dependency on another module
                    # only count when none of the module's own match.
                    candidate_deps = list(
                        module_index.applicable_dependencies(rule_match.path)
                    )
                else:
                    candidate_deps = [subproject_index.deps]

                deps: List[out.FoundDependency] = []
                dependency_matches: List[
                    Tuple[out.ScaPattern, out.FoundDependency]
                ] = []
                for deps in candidate_deps:
                    dependency_matches = list(
                        dependencies_range_match_any(depends_on_entries, deps)
                    )
                    if dependency_matches:
                        break

                pattern_deps = set(
                    dep_pattern.package for dep_pattern in depends_on_entries
                )

                # This list will be non-empty if any of the dependencies the rule searches for are present as direct dependencies
                # It only looks at the same dependencies the matches came from,
                # so with Gradle module attribution a direct copy in another
                # module does not suppress this module's transitive copy.
                rule_could_match_direct_deps = [
                    found_dep.package
                    for found_dep in deps
                    if found_dep.package in pattern_deps
                    and found_dep.transitivity.value == out.Direct()
                ]

                for dep_pat, found_dep in dependency_matches:
                    if found_dep.lockfile_path is None:
                        # In rare cases, it's possible for a dependency to not have a lockfile
                        # path. This indicates a dev error and usually means that the parser
                        # did not associate the dep with a lockfile. So we'll just skip this dependency.
                        logger.warning(
                            f"Found a dependency ({found_dep.package}) without a lockfile path. Skipping..."
                        )
                        continue

                    # Consider this (simplified) situation:
                    # LOCKFILE:
                    #   foo:
                    #     transitivity: direct
                    #     version: 1.0.0
                    #   foo-special:
                    #     transitivity: transitive
                    #     version: 2.0.0
                    # RULE:
                    #   r2c-internal-project-depends-on:
                    #     depends-on-either:
                    #       - package: foo
                    #         version: 2.0.0
                    #       - package: foo-special
                    #         version: 2.0.0
                    #   pattern:
                    #     - bad()
                    # CODE:
                    #   import foo
                    #   bad()
                    #
                    # We end up with a dependency match on `foo-special` and a code match on the call to `bad()`
                    # But we should not produce a reachable finding! The code is using `foo` and _not_ `foo-special`
                    # We don't have a mechanism to detect exactly which dependency is being used in the code right now,
                    # but given that `foo` is present in the direct dependencies and `foo-special` is not, we can conclude
                    # that it is much more likley that any code matching our pattern is using `foo` and not `foo-special`.
                    # APPROXIMATE SOLUTION:
                    # In the case where our dependency match is on a transitive, but one of the dependencies the rule searches for is
                    # present as a direct dependency, we skip this dependency match.
                    # This does not handle the case `foo` and `foo-special` are both direct dependencies
                    if (
                        found_dep.transitivity.value == out.Transitive()
                        and rule_could_match_direct_deps
                    ):
                        continue

                    reachable_deps.add(
                        (
                            Path(found_dep.lockfile_path.value),
                            found_dep.package,
                            found_dep.version,
                            found_dep.transitivity,
                        )
                    )
                    dep_match = out.DependencyMatch(
                        dependency_pattern=dep_pat,
                        found_dependency=found_dep,
                        lockfile=found_dep.lockfile_path,
                        dependency_paths=(parent_index.paths_for(found_dep) or None)
                        if parent_index is not None
                        else None,
                    )
                    sca_match = out.ScaMatch(
                        sca_finding_schema=SCA_FINDING_SCHEMA,
                        reachable=True,
                        reachability_rule=rule.should_run_on_semgrep_core,
                        dependency_match=dep_match,
                    )
                    new_rule_match = evolve(
                        rule_match,
                        match=dataclasses.replace(
                            rule_match.match,
                            extra=dataclasses.replace(
                                rule_match.match.extra, sca_match=sca_match
                            ),
                        ),
                    )
                    reachable_matches.append(new_rule_match)
            except SemgrepError as e:
                errors.append(e)

    return (
        reachable_matches,
        errors,
        (lambda p, d: (p, d.package, d.version, d.transitivity) in reachable_deps),
    )
