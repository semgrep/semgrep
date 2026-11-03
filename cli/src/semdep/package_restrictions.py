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
import re
from dataclasses import dataclass
from typing import Any
from typing import FrozenSet
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Optional
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep import semver_specifier
from semdep.external.packaging.specifiers import InvalidSpecifier  # type: ignore
from semdep.external.packaging.specifiers import SpecifierSet  # type: ignore
from semdep.external.packaging.version import InvalidVersion  # type: ignore
from semdep.external.packaging.version import Version  # type: ignore
from semdep.golang_version import compare_golang_specifier
from semdep.maven_version import compare_maven_specifier
from semgrep.error import SemgrepError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# First characters of the version string the requirements.txt parser reports
# for a requirement that is not pinned to a single release: the PEP 440
# comparators and wildcard, e.g. ">=2.0", "~=1.26", "==2.2.*". A bare name is
# reported as "". Mirrors `is_version_range` in semgrep-app, which applies the
# same shapes to pypi rows for the Dependencies page and SBOM export.
UNPINNED_VERSION_LEADING_CHARS = "<>!~=*"

# Suffix of a PEP 440 prefix-matching clause, e.g. "==2.2.*".
PEP440_WILDCARD_SUFFIX = ".*"


@dataclass(frozen=True)
class VersionInterval:
    """
    The releases a set of PEP 440 clauses admits: one contiguous interval
    (`None` on a side means unbounded) minus the single releases its "!="
    clauses exclude. Bounds are packaging `Version`s, which are untyped here
    because the vendored packaging module is excluded from type checking.
    """

    lower: Optional[Any] = None
    lower_inclusive: bool = True
    upper: Optional[Any] = None
    upper_inclusive: bool = True
    excluded: FrozenSet[Any] = frozenset()


UNBOUNDED_VERSION_INTERVAL = VersionInterval()

# Matches runs of separators treated as equivalent in PyPI package names.
PYPI_PACKAGE_SEPARATORS_PATTERN = re.compile(r"[-_.]+")


def dependency_match_key(ecosystem: Ecosystem, package: str) -> str:
    """
    The key used to match a rule's package name against a dependency's.

    PyPI names are case insensitive and treat runs of "-", "_" and "." as
    equal (PEP 503), and parsers and rules don't all spell them the same way,
    e.g. a rule for "zope.interface" and a lockfile entry "zope-interface".
    """
    if ecosystem == Ecosystem(out.Pypi()):
        return PYPI_PACKAGE_SEPARATORS_PATTERN.sub("-", package).lower()
    return package


def is_unpinned_version(version: str) -> bool:
    """
    Whether a reported dependency version names no definitive installed
    version: an empty string or a version specifier such as ">=2.0".

    Such a dependency stays in the inventory and the SBOM. For pypi it is
    matched against an advisory by range overlap (see `is_in_range`), since
    the installed release can be any release the declared range admits.
    """
    stripped = version.strip()
    return stripped == "" or stripped[0] in UNPINNED_VERSION_LEADING_CHARS


def drop_shadowed_unpinned_copies(
    deps: Iterable[out.FoundDependency],
) -> List[out.FoundDependency]:
    """
    The dependencies worth matching: every pinned one, plus an unpinned one
    only when no pinned copy of the same package is present. A requirements.txt
    that lists `requests>=2.0` by hand next to pip-compile's `requests==2.28.1`
    installs 2.28.1, so the range line adds nothing to the pinned finding.
    """
    deps = list(deps)
    pinned_packages = {
        dependency_match_key(dep.ecosystem, dep.package)
        for dep in deps
        if not is_unpinned_version(dep.version)
    }
    return [
        dep
        for dep in deps
        if not (
            is_unpinned_version(dep.version)
            and dependency_match_key(dep.ecosystem, dep.package) in pinned_packages
        )
    ]


def declared_range_intersects(advisory_range: str, declared_range: str) -> bool:
    """
    Whether some release satisfies both a pypi advisory range and the PEP 440
    specifier a requirements.txt line declares. A bare name (declared as "")
    intersects every advisory. A declared clause that cannot be parsed
    constrains nothing, so an unreadable range is reported rather than hidden.
    """
    try:
        advisory_interval = _build_interval_from_specifier_set(
            SpecifierSet(advisory_range)
        )
    except InvalidSpecifier:
        raise SemgrepError(
            f"unknown package version comparison expression: {advisory_range}"
        )
    declared_interval = UNBOUNDED_VERSION_INTERVAL
    try:
        declared_interval = _build_interval_from_specifier_set(
            SpecifierSet(declared_range)
        )
    except InvalidSpecifier:
        pass
    return _is_nonempty(_intersect(advisory_interval, declared_interval))


def is_in_range(ecosystem: Ecosystem, range: str, version: str) -> bool:
    # An unpinned version is a range of candidate releases rather than one
    # release. Only requirements.txt reports such versions, so only pypi
    # matches them, by overlap with the advisory range.
    if is_unpinned_version(version):
        if ecosystem == Ecosystem(out.Pypi()):
            return declared_range_intersects(range, version)
        return False
    if ecosystem == Ecosystem(out.Npm()):
        # This is suitable for SemVer-compliant ecosystems that implement
        # the same weird math as NPM for version comparisons. Make sure
        # to review the test cases in test_semver_specifier.py.
        res = semver_specifier.version_matches_str(range, version)
        if res is not None:
            return res
        else:
            # TODO: add a fallback?
            return False
    elif ecosystem == Ecosystem(out.Maven()):
        specifiers = [s.strip(" ") for s in range.split(",")]
        return all(compare_maven_specifier(s, version) for s in specifiers)
    elif ecosystem == Ecosystem(out.Gomod()):
        # TODO: What's the purpose of this check?
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
    search_for_ranges: List[out.ScaPattern],
    have_deps: List[out.FoundDependency],
) -> Iterator[Tuple[out.ScaPattern, out.FoundDependency]]:
    for have_dep in drop_shadowed_unpinned_copies(have_deps):
        for target_range in search_for_ranges:
            if (
                target_range.ecosystem == have_dep.ecosystem
                and dependency_match_key(target_range.ecosystem, target_range.package)
                == dependency_match_key(have_dep.ecosystem, have_dep.package)
                and is_in_range(
                    target_range.ecosystem, target_range.semver_range, have_dep.version
                )
            ):
                yield (target_range, have_dep)


def _build_interval_from_specifier_set(specifier_set: Any) -> VersionInterval:
    interval = UNBOUNDED_VERSION_INTERVAL
    for specifier in specifier_set:
        interval = _intersect(
            interval, _build_interval_from_clause(specifier.operator, specifier.version)
        )
    return interval


def _build_interval_from_clause(operator: str, version_str: str) -> VersionInterval:
    """
    The interval one PEP 440 clause admits. A clause whose version is not a
    PEP 440 release (e.g. "===some-tag") admits everything.
    """
    is_wildcard = version_str.endswith(PEP440_WILDCARD_SUFFIX)
    try:
        version = Version(version_str.removesuffix(PEP440_WILDCARD_SUFFIX))
    except InvalidVersion:
        return UNBOUNDED_VERSION_INTERVAL
    if operator == "~=":
        # "~=2.2" is ">=2.2, ==2.*"; "~=1.4.5" is ">=1.4.5, ==1.4.*".
        return VersionInterval(
            lower=version,
            upper=_bump_release(version.release[:-1]),
            upper_inclusive=False,
        )
    if operator in ("==", "==="):
        if is_wildcard:
            return VersionInterval(
                lower=version,
                upper=_bump_release(version.release),
                upper_inclusive=False,
            )
        return VersionInterval(lower=version, upper=version)
    if operator == "!=":
        if is_wildcard:
            return UNBOUNDED_VERSION_INTERVAL
        return VersionInterval(excluded=frozenset([version]))
    if operator == ">=":
        return VersionInterval(lower=version)
    if operator == ">":
        return VersionInterval(lower=version, lower_inclusive=False)
    if operator == "<=":
        return VersionInterval(upper=version)
    if operator == "<":
        return VersionInterval(upper=version, upper_inclusive=False)
    return UNBOUNDED_VERSION_INTERVAL


def _bump_release(release: Tuple[int, ...]) -> Any:
    """The release just above every release that starts with `release`."""
    bumped = release[:-1] + (release[-1] + 1,)
    return Version(".".join(str(segment) for segment in bumped))


def _intersect(first: VersionInterval, second: VersionInterval) -> VersionInterval:
    lower, lower_inclusive = first.lower, first.lower_inclusive
    if second.lower is not None and (lower is None or second.lower > lower):
        lower, lower_inclusive = second.lower, second.lower_inclusive
    elif second.lower is not None and second.lower == lower:
        lower_inclusive = lower_inclusive and second.lower_inclusive

    upper, upper_inclusive = first.upper, first.upper_inclusive
    if second.upper is not None and (upper is None or second.upper < upper):
        upper, upper_inclusive = second.upper, second.upper_inclusive
    elif second.upper is not None and second.upper == upper:
        upper_inclusive = upper_inclusive and second.upper_inclusive

    return VersionInterval(
        lower=lower,
        lower_inclusive=lower_inclusive,
        upper=upper,
        upper_inclusive=upper_inclusive,
        excluded=first.excluded | second.excluded,
    )


def _is_nonempty(interval: VersionInterval) -> bool:
    """
    Whether the interval admits at least one release. An excluded release
    only empties an interval that is that single release: between any two
    distinct releases there is always another (e.g. 1.0.0.1 between 1.0 and
    1.0.1), so excluded points never empty a wider interval.
    """
    if interval.lower is None or interval.upper is None:
        return True
    if interval.lower < interval.upper:
        return True
    is_single_release = (
        interval.lower == interval.upper
        and interval.lower_inclusive
        and interval.upper_inclusive
    )
    return is_single_release and interval.lower not in interval.excluded
