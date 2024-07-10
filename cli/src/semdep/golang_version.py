import re
from dataclasses import dataclass
from datetime import datetime
from typing import Optional

from semgrep.error import SemgrepError


@dataclass
class GolangVersionCore:
    """
    Holds integer impressions of the Golang Core Version
    assuming <major>:<minor>:<patch>
    """

    major: int
    minor: int
    patch: int


@dataclass
class GolangPseudoVersion:
    """
    If we see a golang version string which contains a commit hash at the end,
    this is referred to as a "pseudo-version". Pseudo-versions have the
    following format:

        <core>-YYYYMMDDHHMMSS-<commit-hash>

    """

    timestamp: datetime
    commit_hash: str


@dataclass
class ParsedGolangVersion:
    """
    Holds the core part an optional PseudoVersion if the raw Golang version is
    actually a Pseudo-version
    """

    core: GolangVersionCore
    pseudo: Optional[GolangPseudoVersion]


def parse_golang_core(raw_core: str) -> GolangVersionCore:
    """
    Splits the core into major, minor and patch components, turning them into
    integers which can be effectively used to compare against other like components
    in other GolangVersionCore objects
    """
    if raw_core == "0":  # Some rules are written with just a single 0 for the version
        return GolangVersionCore(major=0, minor=0, patch=0)
    split_raw_core = raw_core.split(".")
    return GolangVersionCore(
        major=int(split_raw_core[0]),
        minor=int(split_raw_core[1]),
        patch=int(split_raw_core[2]),
    )


def parse_golang_pseudo_datetime(raw_timestamp: str) -> datetime:
    """
    Casts the raw timestamp into a timestamp which can be compared
    temporally to another timestamp
    """
    return datetime.strptime(raw_timestamp, "%Y%m%d%H%M%S")


def parse_golang_version(raw_version: str) -> ParsedGolangVersion:
    """
    Parses a raw version string into a ParsedGolangVersion which holds the
    core component and an optional pseudo component
    """
    split_raw_version = raw_version.split("-")
    parsed_core = parse_golang_core(split_raw_version[0])
    parsed_pseudo = None
    if (
        len(split_raw_version) > 1
    ):  # Check if anything exists after the core version spec
        raw_timestamp = split_raw_version[1]
        if "." in split_raw_version[1]:
            # We have a few rules where the timestamp looks like "0.yymmdd..."
            raw_timestamp = split_raw_version[1].split(".")[1]
        parsed_pseudo = GolangPseudoVersion(
            timestamp=parse_golang_pseudo_datetime(raw_timestamp),
            commit_hash=split_raw_version[2],
        )
    return ParsedGolangVersion(core=parsed_core, pseudo=parsed_pseudo)


def cmp_core(
    parsed_version_core: GolangVersionCore, specifier_version_core: GolangVersionCore
) -> int:
    """
    Checks which core version is higher between the package version and the spec version
    from the rule. Does this by first comparing the major component and if equal, comparing
    the minor and vice versa.

    If at any point, the encounter a non-zero diff, we know to stop calculations there and
    just return the diff
    """
    major_diff = parsed_version_core.major - specifier_version_core.major
    if major_diff != 0:
        return major_diff
    minor_diff = parsed_version_core.minor - specifier_version_core.minor
    if minor_diff != 0:
        return minor_diff
    patch_diff = parsed_version_core.patch - specifier_version_core.patch
    if patch_diff != 0:
        return patch_diff
    return 0


def cmp_pseudo(
    parsed_version_pseudo: GolangPseudoVersion,
    specifier_version_pseudo: GolangPseudoVersion,
) -> int:
    """
    Compares the Pseudo portion of a golang pseudo-version, specifically the timestamp
    """
    isAfter = parsed_version_pseudo.timestamp > specifier_version_pseudo.timestamp
    if isAfter:
        return 1
    else:
        isBefore = parsed_version_pseudo.timestamp < specifier_version_pseudo.timestamp
        if isBefore:
            return -1
    return 0


def cmp_golang_versions(
    parsed_version: ParsedGolangVersion, specifier_version: ParsedGolangVersion
) -> int:
    """
    Compares the package version and the spec version using the following steps:
        1. Check core diff (if non-zero, return)
        2. Check if both the parsed version and spec version have a pseudo component
            a. if so, compute the pseudo diff and check if non zero (return if so)
        3. Else, check if parsed version has a pseudo (assuming that any commit-hash is
           temporally after the core version)
            a. if so, return 1 to signify the parsed version is higher
        4. Else, check if spec version has pseudo
            a. if so, return -1 to signify the parsed version is lower
        5. Return 0 (package versions are the same)
    """
    core_diff = cmp_core(parsed_version.core, specifier_version.core)
    if core_diff != 0:
        return core_diff
    if parsed_version.pseudo and specifier_version.pseudo:
        pseudo_diff = cmp_pseudo(parsed_version.pseudo, specifier_version.pseudo)
        if pseudo_diff != 0:
            return pseudo_diff
    elif parsed_version.pseudo:
        return 1
    elif specifier_version.pseudo:
        return -1
    return 0


def compare_golang_specifier(specifier: str, version: str) -> bool:
    """
    Returns if version satisfies specifier arguments
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
    specifier_version = parse_golang_version(matched.group("version"))
    parsed_version = parse_golang_version(version)

    # Depending on the operator in the rule, determine what to do with the diff
    # value we produce from cmp_golang_versions()
    if operator == "==":
        return cmp_golang_versions(parsed_version, specifier_version) == 0
    elif operator == "!=":
        return cmp_golang_versions(parsed_version, specifier_version) != 0
    elif operator == "<=":
        return cmp_golang_versions(parsed_version, specifier_version) <= 0
    elif operator == "<":
        return cmp_golang_versions(parsed_version, specifier_version) < 0
    elif operator == ">=":
        return cmp_golang_versions(parsed_version, specifier_version) >= 0
    elif operator == ">":
        return cmp_golang_versions(parsed_version, specifier_version) > 0
    else:
        raise SemgrepError(
            f"unknown package version comparison expression: {specifier}"
        )
