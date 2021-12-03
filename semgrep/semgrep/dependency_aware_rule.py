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


def parse_depends_on_restrictions(
    entries: List[Dict[str, str]]
) -> Generator[ProjectDependsOnEntry, None, None]:
    for entry in entries:
        for package_manager, dep_name_and_version_str in entry.items():
            pm = None
            if package_manager == "pypi":
                pm = PackageManagers.PYPI
            elif package_manager == "npm":
                pm = PackageManagers.NPM
            else:
                raise SemgrepError(f"unknown package manager: {package_manager}")
            package_name = dep_name_and_version_str.split(" ")[0]
            semver_range = (
                dep_name_and_version_str.replace(package_name + " ", "", 1)
                .strip('"')
                .strip("'")
            )

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
    top_level_target = list(targets[0].parents)[
        -1
    ]  # TODO fix this; run on the top-level of all the targets
    dependencies: List[Dict[str, str]] = rule.project_depends_on or [{}]
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
