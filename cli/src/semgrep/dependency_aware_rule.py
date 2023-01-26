from functools import lru_cache
from pathlib import Path
from typing import Callable
from typing import Dict
from typing import Iterator
from typing import List
from typing import Set
from typing import Tuple

from packaging.specifiers import InvalidSpecifier
from packaging.specifiers import SpecifierSet

import semgrep.output_from_core as core
from semdep.find_lockfiles import find_single_lockfile
from semdep.package_restrictions import dependencies_range_match_any
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyMatch
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyPattern
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaInfo
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.target_manager import TargetManager

SCA_FINDING_SCHEMA = 20220913


def parse_depends_on_yaml(entries: List[Dict[str, str]]) -> Iterator[DependencyPattern]:
    """
    Convert the entries in the Yaml to ProjectDependsOnEntry objects that specify
    namespace, package name, and semver ranges
    """
    for entry in entries:
        # schema checks should gaurantee we have these fields, but we'll code defensively
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

        yield DependencyPattern(
            ecosystem=ecosystem, package=package, semver_range=semver_range
        )


def generate_unreachable_sca_findings(
    rule: Rule,
    target_manager: TargetManager,
    already_reachable: Callable[[Path, FoundDependency], bool],
) -> Tuple[List[RuleMatch], List[SemgrepError], Set[Path]]:
    depends_on_keys = rule.project_depends_on
    dep_rule_errors: List[SemgrepError] = []

    depends_on_entries = list(parse_depends_on_yaml(depends_on_keys))
    ecosystems = list(rule.ecosystems)

    non_reachable_matches = []
    targeted_lockfiles = set()
    for ecosystem in ecosystems:
        lockfile_data = target_manager.get_lockfile_dependencies(ecosystem)
        for lockfile_path, deps in lockfile_data.items():
            targeted_lockfiles.add(lockfile_path)
            dependency_matches = list(
                dependencies_range_match_any(depends_on_entries, list(deps))
            )
            for dep_pat, found_dep in dependency_matches:
                if already_reachable(lockfile_path, found_dep):
                    continue
                dep_match = DependencyMatch(
                    dependency_pattern=dep_pat,
                    found_dependency=found_dep,
                    lockfile=str(lockfile_path),
                )
                match = RuleMatch(
                    message=rule.message,
                    metadata=rule.metadata,
                    severity=rule.severity,
                    fix=None,
                    fix_regex=None,
                    match=core.CoreMatch(
                        rule_id=core.RuleId(rule.id),
                        location=core.Location(
                            path=str(lockfile_path),
                            start=core.Position(0, 0, 0),
                            end=core.Position(0, 0, 0),
                        ),
                        # TODO: we need to define the fields below in
                        # Output_from_core.atd so we can reuse core.MatchExtra
                        extra=core.CoreMatchExtra(
                            metavars=core.Metavars({}),
                            engine_kind=core.EngineKind(core.OSSMatch()),
                        ),
                    ),
                    extra={
                        "sca_info": ScaInfo(
                            sca_finding_schema=SCA_FINDING_SCHEMA,
                            reachable=False,
                            reachability_rule=rule.should_run_on_semgrep_core,
                            dependency_match=dep_match,
                        )
                    },
                )
                non_reachable_matches.append(match)
    return non_reachable_matches, dep_rule_errors, targeted_lockfiles


@lru_cache(maxsize=1000)
def transivite_dep_is_also_direct(
    package: str, deps: Tuple[Tuple[str, Transitivity], ...]
) -> bool:
    """
    Assumes that [dep] is transitive
    Checks if there is a direct version of the transitive dependency [dep]
    """
    for other_package, transitivity in deps:
        if other_package == package and transitivity == Transitivity(Direct()):
            return True
    return False


def generate_reachable_sca_findings(
    matches: List[RuleMatch], rule: Rule, target_manager: TargetManager
) -> Tuple[
    List[RuleMatch], List[SemgrepError], Callable[[Path, FoundDependency], bool]
]:
    depends_on_keys = rule.project_depends_on
    dep_rule_errors: List[SemgrepError] = []

    depends_on_entries = list(parse_depends_on_yaml(depends_on_keys))
    ecosystems = list(rule.ecosystems)

    # Reachability rule
    reachable_matches = []
    reachable_deps = set()
    for ecosystem in ecosystems:
        for match in matches:
            try:
                lockfile_data = find_single_lockfile(match.path, ecosystem)
                if lockfile_data is None:
                    continue
                lockfile_path, deps = lockfile_data
                frozen_deps = tuple((dep.package, dep.transitivity) for dep in deps)
                if str(lockfile_path) not in target_manager.lockfile_scan_info:
                    # If the lockfile is not part of the actual targets or we just haven't parsed this lockfile yet
                    target_manager.lockfile_scan_info[str(lockfile_path)] = len(deps)

                dependency_matches = list(
                    dependencies_range_match_any(depends_on_entries, deps)
                )
                for dep_pat, found_dep in dependency_matches:
                    if found_dep.transitivity == Transitivity(
                        Transitive()
                    ) and transivite_dep_is_also_direct(found_dep.package, frozen_deps):
                        continue
                    reachable_deps.add(
                        (
                            lockfile_path,
                            found_dep.package,
                            found_dep.version,
                            found_dep.transitivity,
                        )
                    )
                    dep_match = DependencyMatch(
                        dependency_pattern=dep_pat,
                        found_dependency=found_dep,
                        lockfile=str(lockfile_path),
                    )
                    match.extra["sca_info"] = ScaInfo(
                        sca_finding_schema=SCA_FINDING_SCHEMA,
                        reachable=True,
                        reachability_rule=rule.should_run_on_semgrep_core,
                        dependency_match=dep_match,
                    )
                    reachable_matches.append(match)
            except SemgrepError as e:
                dep_rule_errors.append(e)
    return (
        reachable_matches,
        dep_rule_errors,
        (lambda p, d: (p, d.package, d.version, d.transitivity) in reachable_deps),
    )
