from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

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
from semdep.parsers.util import line
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
from semgrep.semgrep_interfaces.semgrep_output_v1 import Jsondoc
from semgrep.semgrep_interfaces.semgrep_output_v1 import PackageResolved
from semgrep.semgrep_interfaces.semgrep_output_v1 import PackageSwift
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import SwiftPM
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

# supported parsers for manifest files come from the official apple/swift-package-manager spec
# https://github.com/apple/swift-package-manager/blob/6ff5cbdfa8b694525b2223a6b832cce17e0b73ef/Sources/PackageDescription/PackageRequirement.swift

git_url = regex(
    r"((git|ssh|http(s)?)|(git@[\w\.]+)):(//)?[\w\.@\:/\-~]+/(?P<project>.*?).git/?",
    group="project",
)

# url: "https://example.com/example-package.git"
url_block = regex(r'url:\s*"') >> git_url << string('"')

separator_block = regex(r"\s*,\s*")

# from: "1.2.3"
from_block = regex(r'from:\s*".*?"')

# "1.2.3"..<"1.2.6"
range_block = regex(r'".*?".*?".*?"')

# .exact("1.2.3")
exact_block = regex(r'.exact\(".*?"\)')

# .revision("e74b07278b926c9ec6f9643455ea00d1ce04a021")
revision_block = regex(r'.revision\(".*?"\)')

# .upToNextMajor("1.2.3")
up_to_next_major_block = regex(r'.upToNextMajor\(\s*from:\s*".*?"\)')

# .upToNextMinor("1.2.3")
up_to_next_minor_block = regex(r'.upToNextMinor\(\s*from:\s*".*?"\)')

# .branch("develop")
branch_block = regex(r'.branch\(".*?"\)')

# .package(url: "https://github.com/repo/package.git", .upToNextMajor(from: "7.8.0")), // this is something important
package_block = (
    whitespace
    >> regex(r".package")
    >> lparen
    >> mark_line(url_block)
    << whitespace
    << comma
    << (
        from_block
        | range_block
        | exact_block
        | branch_block
        | revision_block
        | up_to_next_major_block
        | up_to_next_minor_block
    )
    << whitespace
    << rparen
    << string(",").optional()
    << not_any("\n").optional()
)

comment = whitespace >> regex(r" *//") >> line

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


def parse_swiftpm_v2(
    lockfile: Dict[str, JSON], direct_deps: Set[str]
) -> List[FoundDependency]:
    result = []

    deps = lockfile.get("pins")
    if deps is None:
        return []
    for dep_json in deps.as_list():
        fields = dep_json.as_dict()
        if fields is None:
            continue

        package = fields.get("identity")
        if package is None:
            continue
        package_name = package.as_str().lower()
        repository_url = fields.get("location")

        state = fields.get("state")
        if state is None:
            continue

        state_dict = state.as_dict()
        version = state_dict.get("version")
        if version is None:
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
            )
        )

    return result


def parse_swiftpm_v1(
    lockfile: Dict[str, JSON], direct_deps: Set[str]
) -> List[FoundDependency]:
    result = []

    obj = lockfile.get("object")
    if obj is None:
        return []
    deps = obj.as_dict().get("pins")
    if deps is None:
        return []
    for dep_json in deps.as_list():
        fields = dep_json.as_dict()
        package = fields.get("package")
        if package is None:
            continue

        package_name = package.as_str().lower()
        repository_url = fields.get("repositoryURL")

        state = fields.get("state")
        if state is None:
            continue

        state_dict = state.as_dict()
        version = state_dict.get("version")
        if version is None:
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
            )
        )

    return result


def parse_manifest_deps(manifest: List[Tuple]) -> Set[str]:
    result = set()
    for _line_number, package in manifest:
        result.add(package.lower())

    return result


def parse_package_resolved(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, json_doc, ScaParserName(Jsondoc())),
        DependencyFileToParse(
            manifest_path, package_swift_parser, ScaParserName(PackageSwift())
        )
        if manifest_path
        else None,
    )

    if not parsed_lockfile or not parsed_manifest:
        return [], errors

    direct_deps = parse_manifest_deps(filter_on_marked_lines(parsed_manifest))
    lockfile_json = parsed_lockfile.as_dict()
    lockfile_version = lockfile_json.get("version")
    if lockfile_version is None:
        logger.info("no version in lockfile %s", lockfile_path)
        errors.append(
            DependencyParserError(
                str(lockfile_path),
                ScaParserName(PackageResolved()),
                "Unable to determine version of swift lockfile",
            )
        )
        return [], errors

    lockfile_version_int = lockfile_version.as_int()
    if not lockfile_version_int:
        return [], errors

    all_deps = []
    if lockfile_version_int == 1:
        all_deps = parse_swiftpm_v1(lockfile_json, direct_deps)
    elif lockfile_version_int == 2:
        all_deps = parse_swiftpm_v2(lockfile_json, direct_deps)
    else:
        errors.append(
            DependencyParserError(
                str(lockfile_path),
                ScaParserName(PackageResolved()),
                "Invalid lockfile version. Expected 1 or 2.",
            )
        )

    return all_deps, errors
