from pathlib import Path
from typing import Dict
from typing import Generator
from typing import List
from typing import Tuple

from dependencyparser.models import PackageManagers
from dependencyparser.package_restrictions import dependencies_range_match_any
from dependencyparser.package_restrictions import find_and_parse_lockfiles
from dependencyparser.package_restrictions import ProjectDependsOnEntry

from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import CoreLocation
from semgrep.rule_match import RuleMatch
from semgrep.verbose_logging import getLogger

logger = getLogger(__file__)


PACKAGE_MANAGER_MAP = {
    "pypi": PackageManagers.PYPI,
    "npm": PackageManagers.NPM,
}


def parse_depends_on_restrictions(
    entries: List[Dict[str, str]]
) -> Generator[ProjectDependsOnEntry, None, None]:
    for entry in entries:
        # schema checks should gaurantee we have these fields, but we'll code defensively
        namespace = entry.get("namespace")
        if namespace is None:
            raise SemgrepError(f"project-depends-on is missing `namespace`")
        pm = PACKAGE_MANAGER_MAP.get(namespace)
        if pm is None:
            raise SemgrepError(
                f"unknown package namespace: {namespace}, only {list(PACKAGE_MANAGER_MAP.keys())} are supported"
            )
        package_name = entry.get("package")
        if package_name is None:
            raise SemgrepError(f"project-depends-on is missing `package`")
        semver_range = entry.get("version")
        if semver_range is None:
            raise SemgrepError(f"project-depends-on is missing `version`")
        yield ProjectDependsOnEntry(
            namespace=pm, package_name=package_name, semver_range=semver_range
        )


def run_dependency_aware_rule(
    matches: List[RuleMatch],
    rule: Rule,
    targets: List[Path],
) -> Tuple[List[RuleMatch], List[SemgrepError]]:
    """
    Run a dependency aware rule.
    """

    # print(list(targets[0].parents))
    top_level_target_rooted = list(targets[0].parents)
    top_level_target: Path = (
        targets[0] if len(top_level_target_rooted) == 0 else top_level_target_rooted[-1]
    )
    # TODO fix this; run on the top-level of all the targets
    dependencies: List[Dict[str, str]] = rule.project_depends_on or []
    dep_rule_errors: List[SemgrepError] = []

    if len(matches) == 0:
        # if there are no semgrep patterns in the rule, just the dependency restriction,
        # we may still report a result if the dependency is present
        if rule.has_runable_semgrep_rules:
            return [], dep_rule_errors
        else:
            # the rule didn't actually have any runnable semgrep rules
            # so it should fire at the root of the target if the dependencies match
            matches = [
                RuleMatch(
                    id=rule.id,
                    message=rule.message,
                    metadata=rule.metadata,
                    severity=rule.severity,
                    path=top_level_target,
                    fix=None,
                    fix_regex=None,
                    start=CoreLocation(0, 0, 0),
                    end=CoreLocation(0, 0, 0),
                    extra={},
                    lines_cache={},
                )
            ]

    try:
        depends_on_entries = list(parse_depends_on_restrictions(dependencies))
        output = list(
            dependencies_range_match_any(
                depends_on_entries, find_and_parse_lockfiles(top_level_target)
            )
        )
        final_matches = [] if len(output) == 0 else matches
        return final_matches, dep_rule_errors
    except SemgrepError as e:
        return [], [e]
