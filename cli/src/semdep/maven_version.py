"""
Custom parser and comparator for Maven versions
Based on https://docs.oracle.com/middleware/1212/core/MAVEN/maven_version.htm#MAVEN400
"""
import re
from dataclasses import dataclass
from typing import Tuple
from typing import Union

from semdep.external.parsy import ParseError
from semdep.external.parsy import string
from semdep.parsers.util import any_str
from semdep.parsers.util import integer
from semdep.parsers.util import pair
from semdep.parsers.util import triple
from semgrep.error import SemgrepError


@dataclass
class VersionCore:
    major: int
    minor: int
    incrementals: Tuple[int, ...]


parse_version_core = triple(
    integer,
    string(".") >> integer,
    (string(".") >> integer).many(),
).map(lambda x: VersionCore(x[0], x[1], tuple(x[2])))


def normalize_incrementals(
    first: Tuple[int, ...], second: Tuple[int, ...]
) -> Tuple[Tuple[int, ...], Tuple[int, ...]]:
    if len(first) > len(second):
        return first, second + (0,) * (len(first) - len(second))
    elif len(second) > len(first):
        return first + (0,) * (len(second) - len(first)), second
    else:
        return first, second


def compare_version_core(first: VersionCore, second: VersionCore) -> int:
    if first.major != second.major:
        return first.major - second.major
    elif first.minor != second.minor:
        return first.minor - second.minor
    for inc1, inc2 in zip(
        *normalize_incrementals(first.incrementals, second.incrementals)
    ):
        if inc1 != inc2:
            return inc1 - inc2
    return 0


@dataclass
class ParsedMavenVersion:
    core: VersionCore
    qualifier: str
    raw_version: str


MavenVersion = Union[ParsedMavenVersion, str]


def parse_maven_version(version: str) -> MavenVersion:
    try:
        m = pair(parse_version_core, any_str).parse(version)
    except ParseError:
        # "If you do not follow Maven versioning standards in your project versioning scheme,
        # then for version comparison, Maven interprets the entire version as a simple string."
        return version
    return ParsedMavenVersion(core=m[0], qualifier=m[1], raw_version=version)


def cmp_str(x: str, y: str) -> int:
    if x == y:
        return 0
    elif x > y:
        return 1
    else:
        return -1


def cmp_maven_versions(first: MavenVersion, second: MavenVersion) -> int:
    """
    Return less than 0 if first < second
    0 if ==
    greater than 0 if first > second
    """
    if isinstance(first, ParsedMavenVersion) and isinstance(second, ParsedMavenVersion):
        core_cmp = compare_version_core(first.core, second.core)
        if core_cmp != 0:
            return core_cmp
        # "All versions with a qualifier are older than the same version without a qualifier (release version)."
        elif first.qualifier == "" and second.qualifier != "":
            return 1
        elif second.qualifier == "" and first.qualifier != "":
            return -1

        # "Maven treats the SNAPSHOT qualifier differently from all others.
        # If a version number is followed by -SNAPSHOT, then Maven considers it
        # the "as-yet-unreleased" version of the associated MajorVersion, MinorVersion, or IncrementalVersion."
        elif first.qualifier == "-SNAPSHOT" and second.qualifier != "-SNAPSHOT":
            return 1
        elif second.qualifier == "-SNAPSHOT" and first.qualifier != "-SNAPSHOT":
            return -1
        else:
            return cmp_str(first.qualifier, second.qualifier)
    else:
        if isinstance(first, ParsedMavenVersion) and isinstance(second, str):
            first_raw = first.raw_version
            second_raw = second
        elif isinstance(second, ParsedMavenVersion) and isinstance(first, str):
            first_raw = first
            second_raw = second.raw_version
        else:
            assert isinstance(first, str)
            assert isinstance(second, str)
            first_raw = first
            second_raw = second
        return cmp_str(first_raw, second_raw)


def compare_maven_specifier(specifier: str, version: str) -> bool:
    """
    Returns if version satisfies specifier requirement

    i.e. specifier: '< 1.0.0', version: 0.1.0 returns true

    See https://docs.oracle.com/middleware/1212/core/MAVEN/maven_version.htm#MAVEN400
    """
    specifier_regex = re.compile(
        r"""(?P<operator>(==|!=|<=|>=|<|>))\s*(?P<version>.*)"""
    )
    matched = specifier_regex.match(specifier)
    if not matched:
        raise SemgrepError(
            f"unknown package version comparison expression: {specifier}"
        )
    operator = matched.group("operator")
    specifier_version = parse_maven_version(matched.group("version"))
    parsed_version = parse_maven_version(version)

    if operator == "==":
        return cmp_maven_versions(parsed_version, specifier_version) == 0
    elif operator == "!=":
        return cmp_maven_versions(parsed_version, specifier_version) != 0
    elif operator == "<=":
        return cmp_maven_versions(parsed_version, specifier_version) <= 0
    elif operator == "<":
        return cmp_maven_versions(parsed_version, specifier_version) < 0
    elif operator == ">=":
        return cmp_maven_versions(parsed_version, specifier_version) >= 0
    elif operator == ">":
        return cmp_maven_versions(parsed_version, specifier_version) > 0
    else:
        raise SemgrepError(
            f"unknown package version comparison expression: {specifier}"
        )
