"""
Parser for packages.lock.json files for the Nuget ecosystem
"""
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import JSON
from semdep.parsers.util import json_doc
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Jsondoc
from semgrep.semgrep_interfaces.semgrep_output_v1 import Nuget
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


def map_to_transitivity(type_value: Optional[str]) -> Transitivity:
    if type_value == "Direct":
        return Transitivity(Direct())
    elif type_value == "Transitive":
        return Transitivity(Transitive())
    else:
        return Transitivity(Unknown())


def parse_dependencies_field(deps: Dict[str, JSON]) -> List[FoundDependency]:
    output = []

    for framework, dep_json in deps.items():
        dependencies = dep_json.as_dict()

        for package, package_json in dependencies.items():
            fields = package_json.as_dict()
            version = fields.get("resolved")
            if not version:
                logger.info(
                    f"no version for dependency: {package} in framework: {framework}"
                )
                continue

            transitivity_json = fields.get("type")
            transitivity_str = transitivity_json.as_str() if transitivity_json else None

            output.append(
                FoundDependency(
                    package=package,
                    version=version.as_str(),
                    ecosystem=Ecosystem(Nuget()),
                    allowed_hashes={},
                    transitivity=map_to_transitivity(transitivity_str),
                    line_number=package_json.line_number,
                )
            )

    return output


def parse_packages_lock(
    lockfile_path: Path, _manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, _parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, json_doc, ScaParserName(Jsondoc())),
        None,
    )

    if not parsed_lockfile:
        return [], errors

    lockfile_json = parsed_lockfile.as_dict()

    # Start parsing dependencies
    deps = lockfile_json.get("dependencies")
    if deps is None:
        logger.warn("Found packages.lock.json with no 'dependencies'")
        return [], errors

    return parse_dependencies_field(deps.as_dict()), errors
