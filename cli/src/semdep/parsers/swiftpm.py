from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

from semdep.external.parsy import any_char
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.external.parsy import success
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import JSON
from semdep.parsers.util import json_doc
from semdep.parsers.util import mark_line
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import upto
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Jsondoc
from semgrep.semgrep_interfaces.semgrep_output_v1 import PackageSwift
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import SwiftPM
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


package_name = string('"') >> upto('"').bind(lambda url: success(url))

package_section = (
    any_char.until(string("url: "), consume_other=True)
    >> mark_line(package_name)
    << any_char.until(string("\n"))
)

comment = string("\n").optional() >> regex(r" *//([^\n]*)", flags=0, group=1)

package_swift_parser = (
    any_char.until(regex(r"dependencies\s*:\s*\[.*?\n"))
    >> (comment | package_section)
    .sep_by(string("\n"))
    .map(lambda xs: [x for x in xs if isinstance(x, tuple)])
    << any_char.many()
)


def get_transitivity(package_name: str, direct_deps: Set[str]) -> Transitivity:
    if package_name in direct_deps:
        return Transitivity(Direct())
    else:
        return Transitivity(Transitive())


def parse_swiftpm_v2(
    lockfile: dict[str, JSON], direct_deps: Set[str]
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

        state = fields.get("state")
        if state is None:
            continue

        version = state.as_dict().get("version")
        if version is None:
            continue

        result.append(
            FoundDependency(
                package=package_name,
                version=version.as_str(),
                ecosystem=Ecosystem(SwiftPM()),
                allowed_hashes={},
                transitivity=get_transitivity(package_name, direct_deps),
                line_number=version.line_number,
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
        state = fields.get("state")
        if state is None:
            continue

        version = state.as_dict().get("version")
        if version is None:
            continue

        result.append(
            FoundDependency(
                package=package_name,
                version=version.as_str(),
                ecosystem=Ecosystem(SwiftPM()),
                allowed_hashes={},
                transitivity=get_transitivity(package_name, direct_deps),
                line_number=version.line_number,
            )
        )

    return result


def extract_package_name(package_uri: str) -> str:
    return package_uri.split("/")[-1].replace(".git", "")


def parse_manifest_deps(manifest: List[Tuple]) -> Set[str]:
    result = set()
    for _line_number, package in manifest:
        result.add(extract_package_name(package).lower())

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

    direct_deps = parse_manifest_deps(parsed_manifest)
    lockfile_json = parsed_lockfile.as_dict()
    lockfile_version = lockfile_json.get("version")
    if lockfile_version is None:
        logger.info("no version in lockfile %s", lockfile_path)
        return [], errors

    if not lockfile_version.as_int():
        return [], errors

    all_deps = []
    if lockfile_version == 1:
        all_deps = parse_swiftpm_v1(lockfile_json, direct_deps)
    elif lockfile_version == 2:
        all_deps = parse_swiftpm_v2(lockfile_json, direct_deps)

    return all_deps, errors
