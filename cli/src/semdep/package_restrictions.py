from typing import Iterator
from typing import List
from typing import Tuple

from semdep.external.packaging.specifiers import InvalidSpecifier  # type: ignore
from semdep.external.packaging.specifiers import SpecifierSet  # type: ignore
from semgrep.error import SemgrepError
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyPattern
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency


def semver_matches(expression: str, actual_version: str) -> bool:

    try:
        ss = SpecifierSet(expression)
        matched = len(list(ss.filter([actual_version]))) > 0
        # print(f'does {expression} match {actual_version}?: {matched}')
        return matched
    except InvalidSpecifier:
        raise SemgrepError(
            f"unknown package version comparison expression: {expression}"
        )


# compare vulnerable range to version in lockfile
def dependencies_range_match_any(
    search_for_ranges: List[DependencyPattern],
    have_deps: List[FoundDependency],
) -> Iterator[Tuple[DependencyPattern, FoundDependency]]:
    for have_dep in have_deps:
        for target_range in search_for_ranges:
            if (
                target_range.ecosystem == have_dep.ecosystem
                and target_range.package == have_dep.package
                and semver_matches(target_range.semver_range, have_dep.version)
            ):
                yield (target_range, have_dep)
