"""
Custom parser and comparator for Maven versions
Based on https://docs.oracle.com/middleware/1212/core/MAVEN/maven_version.htm#MAVEN400
"""
import re
from dataclasses import dataclass
from typing import Union

from semgrep.error import SemgrepError


@dataclass
class ParsedMavenVersion:
    major: int
    minor: int
    incremental: int
    qualifer: str
    raw_version: str


MavenVersion = Union[ParsedMavenVersion, str]


def parse_maven_version(version: str) -> MavenVersion:
    m = re.compile(
        r"(?P<major>\d+)\.(?P<minor>\d+)(?:\.(?P<incremental>\d+))?(?P<qualifier>.*)"
    ).match(version)
    # "If you do not follow Maven versioning standards in your project versioning scheme,
    # then for version comparison, Maven interprets the entire version as a simple string."
    if not m:
        return version
    if "." in m.group("qualifier"):
        return version
    return ParsedMavenVersion(
        major=int(m.group("major")),
        minor=int(m.group("minor")),
        incremental=int(m.group("incremental")) if m.group("incremental") else 0,
        qualifer=m.group("qualifier"),
        raw_version=m.group(0),
    )


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
        if first.major != second.major:
            return first.major - second.major
        elif first.minor != second.minor:
            return first.minor - second.minor
        elif first.incremental != second.incremental:
            return first.incremental - second.incremental
        # "All versions with a qualifier are older than the same version without a qualifier (release version)."
        elif first.qualifer == "" and second.qualifer != "":
            return 1
        elif second.qualifer == "" and first.qualifer != "":
            return -1

        # "Maven treats the SNAPSHOT qualifier differently from all others.
        # If a version number is followed by -SNAPSHOT, then Maven considers it
        # the "as-yet-unreleased" version of the associated MajorVersion, MinorVersion, or IncrementalVersion."
        elif first.qualifer == "-SNAPSHOT" and second.qualifer != "-SNAPSHOT":
            return 1
        elif second.qualifer == "-SNAPSHOT" and first.qualifer != "-SNAPSHOT":
            return -1
        else:
            return cmp_str(first.qualifer, second.qualifer)
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
