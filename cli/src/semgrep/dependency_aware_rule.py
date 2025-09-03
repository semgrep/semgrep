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
from semgrep.sca_subproject_support import TRANSITIVE_REACHABILITY_SUBPROJECT_KINDS
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
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
    fips_mode: bool,
    write_to_tr_cache: bool = True,
) -> Tuple[List[RuleMatch], List[SemgrepError]]:
    """
    Returns matches to a only a rule's sca-depends-on patterns;
    ignoring any reachabiliy patterns it has.

    :param write_to_tr_cache: Whether to write to the transitive
        reachability cache (/tr_cache endpoint in the app).
    """
    errors: List[SemgrepError] = []
    depends_on_entries = list(parse_depends_on_yaml(rule.project_depends_on))
    ecosystems = list(rule.ecosystems)

    non_reachable_matches: List[RuleMatch] = []
    match_based_keys: Dict[tuple[str, Path, str], int] = defaultdict(int)
    for ecosystem in ecosystems:
        for subproject in resolved_deps.get(ecosystem, []):
            subproject_kind = dep_source_to_subproject_kind(
                subproject.info.dependency_source
            )
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
                    fips_mode=fips_mode,
                )
                new_rule_match = evolve(
                    rule_match,
                    match_based_index=match_based_keys[rule_match.match_based_key],
                )
                match_based_keys[rule_match.match_based_key] += 1
                subproject_matches.append(new_rule_match)

            if x_tr and subproject_kind in TRANSITIVE_REACHABILITY_SUBPROJECT_KINDS:
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
                fd, rules_tmp_path = mkstemp(
                    suffix=".yaml", prefix="semgrep-", text=True
                )
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
    package: str, deps: Tuple[Tuple[str, out.DependencyKind], ...]
) -> bool:
    """
    Assumes that [dep] is transitive
    Checks if there is a direct version of the transitive dependency [dep]
    """
    return (package, out.DependencyKind(out.Direct())) in deps


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

                dependency_matches: List[
                    Tuple[out.ScaPattern, out.FoundDependency]
                ] = list(dependencies_range_match_any(depends_on_entries, deps))

                pattern_deps = set(
                    dep_pattern.package for dep_pattern in depends_on_entries
                )

                # This list will be non-empty if any of the dependencies the rule searches for are present as direct dependencies
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
