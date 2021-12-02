from pathlib import Path
from typing import Any
from typing import Dict
from typing import Generator
from typing import List
from typing import Tuple

from dependencyparser.models import PackageManagers
from dependencyparser.package_restrictions import dependencies_range_match_any
from dependencyparser.package_restrictions import find_and_parse_lockfiles
from dependencyparser.package_restrictions import ProjectDependsOnEntry

from semgrep.error import SemgrepError
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
                # TODO: semgrep error instead
                raise AssertionError(f"unknown package manager: {package_manager}")
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
    dep_aware_rule: Dict[str, Any],
    targets: List[Path],
) -> Tuple[List[RuleMatch], List[SemgrepError]]:
    """
    Run a dependency aware rule.
    """

    # TODO: populate error output
    dep_rule_errors: List[SemgrepError] = []

    if len(matches) == 0:
        return [], dep_rule_errors

    dependencies = dep_aware_rule.get("project-depends-on", {})
    depends_on_entries = list(parse_depends_on_restrictions(dependencies))

    # print(list(targets[0].parents))
    target = list(targets[0].parents)[
        -1
    ]  # TODO fix this; run on the top-level of all the targets
    output = list(
        dependencies_range_match_any(
            depends_on_entries, find_and_parse_lockfiles(target)
        )
    )
    final_matches = [] if len(output) == 0 else matches
    return final_matches, dep_rule_errors
