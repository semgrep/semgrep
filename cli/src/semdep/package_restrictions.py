from typing import Iterator
from typing import List
from typing import Tuple

from semdep.external.packaging.specifiers import InvalidSpecifier  # type: ignore
from semdep.external.packaging.specifiers import SpecifierSet  # type: ignore
from semdep.golang_version import compare_golang_specifier
from semdep.maven_version import compare_maven_specifier
from semgrep.error import SemgrepError
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyPattern
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven


def is_in_range(ecosystem: Ecosystem, range: str, version: str) -> bool:
    if ecosystem == Ecosystem(Maven()):
        specifiers = [s.strip(" ") for s in range.split(",")]
        return all(compare_maven_specifier(s, version) for s in specifiers)
    elif ecosystem == Ecosystem(Gomod()):
        if len(version.split("-")) < 3:
            try:
                ss = SpecifierSet(range)
                matched = len(list(ss.filter([version]))) > 0
                return matched
            except InvalidSpecifier:
                raise SemgrepError(
                    f"unknown package version comparison expression: {range}"
                )
        else:
            specifiers = [s.strip(" ") for s in range.split(",")]
            try:
                result = all(compare_golang_specifier(s, version) for s in specifiers)
            except Exception as e:
                raise SemgrepError(
                    f"bad golang module version comparison between version {version} and spec range {range}: {e}"
                )

            return result
    else:
        try:
            ss = SpecifierSet(range)
            matched = len(list(ss.filter([version]))) > 0
            return matched
        except InvalidSpecifier:
            raise SemgrepError(
                f"unknown package version comparison expression: {range}"
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
                and is_in_range(
                    target_range.ecosystem, target_range.semver_range, have_dep.version
                )
            ):
                yield (target_range, have_dep)
