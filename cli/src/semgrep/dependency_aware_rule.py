import dataclasses
import json
import os
from collections import defaultdict
from functools import lru_cache
from pathlib import Path
from tempfile import mkstemp
from typing import Callable
from typing import Dict
from typing import Iterator
from typing import List
from typing import Tuple

from attr import evolve

import semgrep.rpc_call as rpc_call
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.external.packaging.specifiers import InvalidSpecifier  # type: ignore
from semdep.external.packaging.specifiers import SpecifierSet  # type: ignore
from semdep.package_restrictions import dependencies_range_match_any
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.subproject import find_closest_resolved_subproject
from semgrep.subproject import iter_dependencies
from semgrep.subproject import iter_found_dependencies
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


SCA_FINDING_SCHEMA = 20220913


def parse_depends_on_yaml(entries: List[Dict[str, str]]) -> Iterator[out.ScaPattern]:
    """
    Convert the entries in the Yaml to ProjectDependsOnEntry objects that specify
    namespace, package name, and semver ranges
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


# TODO: should be renamed undetermined_or_unreachable_...
#  or handle_transitive_findings
def generate_unreachable_sca_findings(
    rule: Rule,
    already_reachable: Callable[[Path, out.FoundDependency], bool],
    resolved_deps: Dict[Ecosystem, List[out.ResolvedSubproject]],
    x_tr: bool,
) -> Tuple[List[RuleMatch], List[SemgrepError]]:
    """
    Returns matches to a only a rule's sca-depends-on patterns; ignoring any
    reachabiliy patterns it has.
    """
    errors: List[SemgrepError] = []
    depends_on_entries = list(parse_depends_on_yaml(rule.project_depends_on))
    ecosystems = list(rule.ecosystems)

    non_reachable_matches: List[RuleMatch] = []
    match_based_keys: Dict[tuple[str, Path, str], int] = defaultdict(int)
    for ecosystem in ecosystems:
        for subproject in resolved_deps.get(ecosystem, []):
            deps: List[out.FoundDependency] = list(
                iter_found_dependencies(subproject.resolved_dependencies)
            )
            subproject_matches: List[RuleMatch] = []
            dependency_matches: List[Tuple[out.ScaPattern, out.FoundDependency]] = list(
                dependencies_range_match_any(depends_on_entries, list(deps))
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
                )
                new_rule_match = evolve(
                    rule_match,
                    match_based_index=match_based_keys[rule_match.match_based_key],
                )
                match_based_keys[rule_match.match_based_key] += 1
                subproject_matches.append(new_rule_match)

            if x_tr:
                logger.info(f"SCA TR is on!")
                # TODO: consider only the matches with reachable rules
                transitive_findings = [
                    out.TransitiveFinding(m=rm.match) for rm in subproject_matches
                ]
                fd, rules_tmp_path = mkstemp(
                    suffix=".rules", prefix="semgrep-", text=True
                )
                with os.fdopen(fd, "w") as fp:
                    fp.write(json.dumps([rule.raw]))
                params = out.TransitiveReachabilityFilterParams(
                    rules_path=out.Fpath(rules_tmp_path),
                    findings=transitive_findings,
                    dependencies=list(
                        iter_dependencies(subproject.resolved_dependencies)
                    ),
                )
                # to debug: print(params.to_json_string())
                tr_filtered_matches = rpc_call.transitive_reachability_filter(params)
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
    package: str, deps: Tuple[Tuple[str, out.Transitivity], ...]
) -> bool:
    """
    Assumes that [dep] is transitive
    Checks if there is a direct version of the transitive dependency [dep]
    """
    return (package, out.Transitivity(out.Direct())) in deps


def generate_reachable_sca_findings(
    matches: List[RuleMatch],
    rule: Rule,
    resolved_deps: Dict[Ecosystem, List[out.ResolvedSubproject]],
) -> Tuple[
    List[RuleMatch], List[SemgrepError], Callable[[Path, out.FoundDependency], bool]
]:
    errors: List[SemgrepError] = []
    depends_on_entries = list(parse_depends_on_yaml(rule.project_depends_on))
    ecosystems = list(rule.ecosystems)

    # Reachability rule
    reachable_matches: List[RuleMatch] = []
    reachable_deps = set()
    for ecosystem in ecosystems:
        for rule_match in matches:
            try:
                subproject = find_closest_resolved_subproject(
                    rule_match.path, ecosystem, resolved_deps.get(ecosystem, [])
                )
                if subproject is None:
                    continue

                deps: List[out.FoundDependency] = list(
                    iter_found_dependencies(subproject.resolved_dependencies)
                )
                frozen_deps = tuple((dep.package, dep.transitivity) for dep in deps)

                dependency_matches: List[
                    Tuple[out.ScaPattern, out.FoundDependency]
                ] = list(dependencies_range_match_any(depends_on_entries, deps))
                for dep_pat, found_dep in dependency_matches:
                    if found_dep.lockfile_path is None:
                        # In rare cases, it's possible for a dependency to not have a lockfile
                        # path. This indicates a dev error and usually means that the parser
                        # did not associate the dep with a lockfile. So we'll just skip this dependency.
                        logger.warning(
                            f"Found a dependency ({found_dep.package}) without a lockfile path. Skipping..."
                        )
                        continue

                    # ???
                    if (
                        found_dep.transitivity.value == out.Transitive()
                        and transitive_dep_is_also_direct(
                            found_dep.package, frozen_deps
                        )
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
