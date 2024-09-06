import re
from dataclasses import dataclass
from datetime import datetime
from typing import List
from typing import Optional
from typing import Union

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

    Note: This type is purely for capturing datetimes and commit hashes which are the sole
    modifiers of the core version. If a pre-release version is present, we will parse it as a
    pre-release version instead even if a timestamp is present since this will be captured in the
    alphanumeric comparison of the pre-release version.

    e.g.)
        "1.0.0-20220225172249-27dd8689420f" is parsed as a GolangPsuedoVersion
        "1.0.0-0.20220225172249-27dd8689420f" is parsed as a GolangPreReleaseVersion

    """

    timestamp: datetime
    commit_hash: str


@dataclass
class GolangPreReleaseVersion:
    """
    If we see a golang version string which contains a pre-release version at the end,
    this is referred to as a "pre-release version". Pre-release versions have the
    following format:

        <core>-<a list of pre-release identifiers separated by .>

    From the semantic versioning (semver) spec, pre-release versions can be of unlimited
    length and can contain any combination of alphanumerics and hyphens. The spec to compare
    pre-release versions can be found here: https://semver.org/#spec-item-11

    These versions are always prefixed by a future stable release of the core
    version, indicated by core in the format above
    """

    pre_release_identifiers: List[str]


@dataclass
class ParsedGolangVersion:
    """
    Holds the core part an optional PseudoVersion if the raw Golang version is
    actually a Pseudo-version
    """

    core: GolangVersionCore
    pseudo: Optional[GolangPseudoVersion] = None
    pre_release: Optional[GolangPreReleaseVersion] = None


def try_parse_golang_pseudo(
    split_raw_version: List[str],
) -> Union[GolangPseudoVersion, None]:
    """
    Tries to parse a raw pseudo version string into a GolangPseudoVersion object. Returns None
    if the raw version string does not conform to the pseudo version format

    args:
        split_raw_version: A list of strings split by "-" from the raw version string
    """
    raw_timestamp = split_raw_version[1]
    try:
        parsed_timestamp = parse_golang_pseudo_datetime(raw_timestamp)
        return GolangPseudoVersion(
            timestamp=parsed_timestamp, commit_hash=split_raw_version[2]
        )
    except ValueError:
        return None


def parse_golang_pre_release(split_raw_version: List[str]) -> GolangPreReleaseVersion:
    """
    Parses a raw pre-release version string into a GolangPreReleaseVersion object
    """
    raw_pre_release = "-".join(split_raw_version[1:])
    split_pre_release = raw_pre_release.split(".")
    return GolangPreReleaseVersion(pre_release_identifiers=split_pre_release)


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
    parsed_pre_release = None
    if (
        len(split_raw_version) > 1
    ):  # Check if anything exists after the core version spec
        if len(split_raw_version) == 3:
            # Indicates that we might be parsing a pseudo version
            # Try to parse into a pseudo version first
            parsed_pseudo = try_parse_golang_pseudo(split_raw_version)
            if parsed_pseudo is None:
                # If the first item after the core version does not parse into a timestamp,
                # then we are not parsing a pseudo version. Instead, we are parsing a pre-release version

                # Parse everything after the core version as a pre-release version
                parsed_pre_release = parse_golang_pre_release(split_raw_version)
        else:
            # Indicates we are parsing a pre-release version
            parsed_pre_release = parse_golang_pre_release(split_raw_version)
    return ParsedGolangVersion(
        core=parsed_core, pseudo=parsed_pseudo, pre_release=parsed_pre_release
    )


def cmp_core(
    parsed_version_core: GolangVersionCore, specifier_version_core: GolangVersionCore
) -> int:
    """
    Checks which core version is higher between the package version and the spec version
    from the rule. Does this by first comparing the major component and if equal, comparing
    the minor and vice versa.

    If at any point, the encounter a non-zero diff, we know to stop calculations there and
    just return the diff

    Returns:
     a) 1 if the parsed core is after the specifier core
     b) -1 if the specifier core is after the parsed core
     c) 0 if they have equal precedence
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

    Returns:
     a) 1 if the parsed pseudo is after the specifier pseudo
     b) -1 if the specifier pseudo is after the parsed pseudo
     c) 0 if they have equal precedence
    """
    if parsed_version_pseudo.commit_hash == specifier_version_pseudo.commit_hash:
        return 0
    isAfter = parsed_version_pseudo.timestamp > specifier_version_pseudo.timestamp
    if isAfter:
        return 1
    else:
        isBefore = parsed_version_pseudo.timestamp < specifier_version_pseudo.timestamp
        if isBefore:
            return -1
    return 0


def cmp_pre_release(
    parsed_version_pre_release: GolangPreReleaseVersion,
    specifier_version_pre_release: GolangPreReleaseVersion,
) -> int:
    """
    Compares the pre-release portion of a golang pre-release version, specifically the milestone and version (if applicable)

    Returns:
     a) 1 if the parsed pre-release is after the specifier pre-release
     b) -1 if the specifier pre-release is after the parsed pre-release
     c) 0 if they have equal precedence
    """
    parsed_num_identifiers = len(parsed_version_pre_release.pre_release_identifiers)
    specifier_num_identifiers = len(
        specifier_version_pre_release.pre_release_identifiers
    )

    # Compare each identifier
    for parsed_identifier, specifier_identifier in zip(
        parsed_version_pre_release.pre_release_identifiers,
        specifier_version_pre_release.pre_release_identifiers,
    ):
        if parsed_identifier == specifier_identifier:
            continue

        # Check if the identifier is a number
        is_parsed_identifier_number = parsed_identifier.isdigit()
        is_specifier_identifier_number = specifier_identifier.isdigit()

        # If both are numbers, compare them as integers
        if is_parsed_identifier_number and is_specifier_identifier_number:
            return int(parsed_identifier) - int(specifier_identifier)
        # If one is a number and the other isn't, the number is lower
        elif is_parsed_identifier_number:
            return -1
        elif is_specifier_identifier_number:
            return 1

        # If both are strings, compare them lexicographically
        if parsed_identifier < specifier_identifier:
            return -1
        elif parsed_identifier > specifier_identifier:
            return 1

    # If the number of identifiers is different, the one with more identifiers takes precedence
    # given that the identifiers are the same up to the length of the shorter list
    if specifier_num_identifiers > parsed_num_identifiers:
        return -1
    elif parsed_num_identifiers > specifier_num_identifiers:
        return 1

    return 0


def cmp_golang_versions(
    parsed_version: ParsedGolangVersion, specifier_version: ParsedGolangVersion
) -> int:
    """
    Compares the package version and the spec version. For a given core version, at a high level, the order
    of release between the three major categories of a golang version (pre-release, core and pseudo) is as follows:

    pre-release => core => pseudo

    e.g.) given a core version of 1.0.0, the following versions are ordered from lowest to highest:

    1.0.0-alpha.1 < 1.0.0 < 1.0.0-20220225172249-27dd8689420f

    Returns:
     a) 1 if the parsed version is after the specifier version
     b) -1 if the specifier version is after the parsed version
     c) 0 if they have equal precedence
    """
    # Check core version
    core_diff = cmp_core(parsed_version.core, specifier_version.core)
    if core_diff != 0:
        return core_diff

    # Check pre-release version
    if parsed_version.pre_release and specifier_version.pre_release:
        # Get pre-release precedence computation (see cmp_pre_release docstring) & return if non-zero
        pre_release_diff = cmp_pre_release(
            parsed_version.pre_release, specifier_version.pre_release
        )
        if pre_release_diff != 0:
            return pre_release_diff
    # If one has a pre-release and the other doesn't, the one with the pre-release is lower
    elif parsed_version.pre_release:
        return 1
    elif specifier_version.pre_release:
        return -1

    # Check pseudo version
    if parsed_version.pseudo and specifier_version.pseudo:
        # Get pseudo precedence computation (see cmp_pseudo docstring) & return if non-zero
        pseudo_diff = cmp_pseudo(parsed_version.pseudo, specifier_version.pseudo)
        if pseudo_diff != 0:
            return pseudo_diff
    # If one has a pseudo and the other doesn't, the one with the pseudo is higher
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
