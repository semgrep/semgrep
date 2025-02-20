from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.external.parsy import any_char
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.parsers.util import comma
from semdep.parsers.util import consume_line
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import filter_on_marked_lines
from semdep.parsers.util import JSON
from semdep.parsers.util import json_doc
from semdep.parsers.util import lparen
from semdep.parsers.util import mark_line
from semdep.parsers.util import new_lines
from semdep.parsers.util import not_any
from semdep.parsers.util import rparen
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semdep.parsers.util import whitespace
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import SwiftPM
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

# supported parsers for manifest files come from the official apple/swift-package-manager spec
# https://github.com/apple/swift-package-manager/blob/6ff5cbdfa8b694525b2223a6b832cce17e0b73ef/Sources/PackageDescription/PackageRequirement.swift

# url: "https://example.com/example-package.git"
url_block = regex(
    r'url:\s*"((git|ssh|http(s)?)|(git@[\w\.]+)):(//)?[\w\.@\:/\-~]+/(?P<project>[^"]+?)(\.git)?/?"',
    group="project",
)


separator_block = regex(r"\s*,\s*")

# from: "1.2.3"
from_block = regex(r'from:\s*".*?"')

# "1.2.3"..<"1.2.6"
range_block = regex(r'".*?".*?".*?"')

# .exact("1.2.3")
exact_block = regex(r'(\.exact\(".*?"\))|(exact:\s*(("[^"]+?")|(Version\([^)]+\))))')

# .revision("e74b07278b926c9ec6f9643455ea00d1ce04a021")
revision_block = regex(r'\.revision\(".*?"\)')

# .upToNextMajor("1.2.3")
up_to_next_major_block = regex(r'\.upToNextMajor\(\s*from:\s*".*?"\)')

# .upToNextMinor("1.2.3")
up_to_next_minor_block = regex(r'\.upToNextMinor\(\s*from:\s*".*?"\)')

# .branch("develop")
branch_block = regex(r'\.branch\(".*?"\)')

path_block = regex(r'path:\s*"[^"]+?"')

name_block = regex(r'name:\s*"(?P<project>[^"]+?)"', group="project")

# .package(url: "https://github.com/repo/package.git", .upToNextMajor(from: "7.8.0")), // this is something important
package_block = (
    whitespace
    >> regex(r"\.package")
    >> lparen
    >> mark_line(url_block | name_block)
    << whitespace
    << comma
    << whitespace
    << (
        from_block
        | range_block
        | exact_block
        | branch_block
        | revision_block
        | up_to_next_major_block
        | up_to_next_minor_block
        | path_block
    )
    << whitespace
    << rparen
    << string(",").optional()
    << not_any("\n").optional()
)

comment = whitespace >> regex(r" *//") >> consume_line

multiple_package_blocks = (comment | package_block).sep_by(new_lines)

dependencies_block = (
    regex(r"dependencies:\s*\[")
    >> whitespace
    >> multiple_package_blocks
    << consume_line.many()
)

package_swift_parser = (
    any_char.until(regex(r"dependencies\s*:")) >> dependencies_block << any_char.many()
)

# versions of Package.resolved are defined in swift source code at
# https://github.com/swiftlang/swift-package-manager/blob/4c206fb9edf118b213485f295295e41eed995bb1/Sources/PackageGraph/ResolvedPackagesStore.swift#L319


def parse_swiftpm_v2_v3(
    lockfile_path: Path,
    lockfile: Dict[str, JSON],
    direct_deps: Set[str],
    manifest_path: Optional[Path],
) -> tuple[List[FoundDependency], List[DependencyParserError]]:
    """
    Parse a SwiftPM Package.resolved file of version 2 or version 3. The only difference between v2
    and v3 is a single 'originHash' field that is used as a performance optimization in SwiftPM and that
    we do not care about, so the same parser is used for v2 and v3.

    See https://github.com/swiftlang/swift-package-manager/blob/4c206fb9edf118b213485f295295e41eed995bb1/Sources/PackageGraph/ResolvedPackagesStore.swift#L421-L519
    for the source code defining V2 and V3 to see the difference. The PR that introduced V3 can be found at https://github.com/swiftlang/swift-package-manager/pull/6698
    """
    result = []
    errors = []
    deps = lockfile.get("pins")
    if deps is None:
        return [], [
            DependencyParserError(
                out.Fpath(str(lockfile_path)),
                ScaParserName(out.PPackageResolved()),
                "Package.resolved v2/v3 file missing pins field",
            )
        ]
    for dep_json in deps.as_list():
        fields = dep_json.as_dict()
        if fields is None:
            continue

        package = fields.get("identity")
        if package is None:
            errors.append(
                DependencyParserError(
                    out.Fpath(str(lockfile_path)),
                    ScaParserName(out.PPackageResolved()),
                    "Package.resolved v2/v3 pin missing identity field",
                )
            )
            continue
        package_name = package.as_str().lower()
        repository_url = fields.get("location")

        state = fields.get("state")
        if state is None:
            errors.append(
                DependencyParserError(
                    out.Fpath(str(lockfile_path)),
                    ScaParserName(out.PPackageResolved()),
                    "Package.resolved v2/v3 pin missing state field",
                )
            )
            continue

        state_dict = state.as_dict()
        version = state_dict.get("version")
        # If there's no version field, or the version field is `null`
        # we skip this dependency
        if version is None or version.is_null():
            errors.append(
                DependencyParserError(
                    out.Fpath(str(lockfile_path)),
                    ScaParserName(out.PPackageResolved()),
                    f"Unable to determine version of dependency - {package_name} - skipping. This may be because the dependency is pinned to an unreleased commit.",
                )
            )
            continue

        revision = state_dict.get("revision")

        result.append(
            FoundDependency(
                package=package_name,
                version=version.as_str(),
                ecosystem=Ecosystem(SwiftPM()),
                allowed_hashes={},
                transitivity=transitivity(direct_deps, [package_name]),
                line_number=version.line_number,
                git_ref=revision.as_str() if revision else None,
                resolved_url=repository_url.as_str() if repository_url else None,
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
            )
        )

    return result, errors


def parse_swiftpm_v1(
    lockfile_path: Path,
    lockfile: Dict[str, JSON],
    direct_deps: Set[str],
    manifest_path: Optional[Path],
) -> tuple[List[FoundDependency], List[DependencyParserError]]:
    result = []
    errors = []

    obj = lockfile.get("object")
    if obj is None:
        return [], [
            DependencyParserError(
                out.Fpath(str(lockfile_path)),
                ScaParserName(out.PPackageResolved()),
                "Package.resolved v1 file missing object field",
            )
        ]
    deps = obj.as_dict().get("pins")
    if deps is None:
        return [], [
            DependencyParserError(
                out.Fpath(str(lockfile_path)),
                ScaParserName(out.PPackageResolved()),
                "Package.resolved v1 file missing pins field",
                line=obj.line_number,
            )
        ]
    for dep_json in deps.as_list():
        fields = dep_json.as_dict()
        package = fields.get("package")
        if package is None:
            errors.append(
                DependencyParserError(
                    out.Fpath(str(lockfile_path)),
                    ScaParserName(out.PPackageResolved()),
                    "Package.resolved v1 pin missing package field",
                    line=dep_json.line_number,
                )
            )
            continue

        package_name = package.as_str().lower()
        repository_url = fields.get("repositoryURL")

        state = fields.get("state")
        if state is None:
            errors.append(
                DependencyParserError(
                    out.Fpath(str(lockfile_path)),
                    ScaParserName(out.PPackageResolved()),
                    "Package.resolved v1 pin missing state field",
                    line=dep_json.line_number,
                )
            )
            continue

        state_dict = state.as_dict()
        version = state_dict.get("version")
        # If there's no version field, or the version field is `null`
        # we skip this dependency
        if version is None or version.is_null():
            errors.append(
                DependencyParserError(
                    out.Fpath(str(lockfile_path)),
                    ScaParserName(out.PPackageResolved()),
                    f"Unable to determine version of dependency - {package_name} - skipping. This may be because the dependency is pinned to an unreleased commit.",
                    line=version.line_number if version is not None else None,
                )
            )
            continue

        revision = state_dict.get("revision")

        result.append(
            FoundDependency(
                package=package_name,
                version=version.as_str(),
                ecosystem=Ecosystem(SwiftPM()),
                allowed_hashes={},
                transitivity=transitivity(direct_deps, [package_name]),
                line_number=version.line_number,
                git_ref=revision.as_str() if revision else None,
                resolved_url=repository_url.as_str() if repository_url else None,
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
            )
        )

    return (result, errors)


def parse_manifest_deps(manifest: List[Tuple]) -> Set[str]:
    result = set()
    for _line_number, package in manifest:
        result.add(package.lower())

    return result


def parse_package_resolved(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, json_doc, ScaParserName(out.PJsondoc())),
        DependencyFileToParse(
            manifest_path, package_swift_parser, ScaParserName(out.PPackageSwift())
        )
        if manifest_path
        else None,
    )

    if not parsed_lockfile:
        return [], errors

    direct_deps = (
        set()
        if not parsed_manifest
        else parse_manifest_deps(filter_on_marked_lines(parsed_manifest))
    )
    lockfile_json = parsed_lockfile.as_dict()
    lockfile_version = lockfile_json.get("version")
    if lockfile_version is None:
        logger.info("no version in lockfile %s", lockfile_path)
        errors.append(
            DependencyParserError(
                out.Fpath(str(lockfile_path)),
                ScaParserName(out.PPackageResolved()),
                "Unable to determine version of swift lockfile",
            )
        )
        return [], errors

    lockfile_version_int = lockfile_version.as_int()
    if not lockfile_version_int:
        return [], errors

    if lockfile_version_int == 1:
        all_deps, new_errors = parse_swiftpm_v1(
            lockfile_path, lockfile_json, direct_deps, manifest_path
        )
        errors.extend(new_errors)
    elif lockfile_version_int == 2 or lockfile_version_int == 3:
        all_deps, new_errors = parse_swiftpm_v2_v3(
            lockfile_path, lockfile_json, direct_deps, manifest_path
        )
        errors.extend(new_errors)
    else:
        all_deps = []
        errors.append(
            DependencyParserError(
                out.Fpath(str(lockfile_path)),
                ScaParserName(out.PPackageResolved()),
                "Invalid lockfile version. Expected 1, 2, or 3.",
            )
        )

    return all_deps, errors
