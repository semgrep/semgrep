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
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Jsondoc
from semgrep.semgrep_interfaces.semgrep_output_v1 import Nuget
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.verbose_logging import getLogger
from semdep.external.parsy import regex
from semdep.external.parsy import string


logger = getLogger(__name__)

def get_csproj_parser():
    # Utility to match whitespace and newline
    whitespace = regex(r'\s*')

    # Utility to match any content between two delimiters
    def between(start, end):
        return start >> regex(r'(?:.(?!' + end + '))*.') << end

    # Utility to match attribute value
    def attribute_value(attr_name):
        return between(string(f'{attr_name}="'), string('"'))

    # Utility to match XML tag with a given name
    def tag(tag_name, content_parser):
        return between(string(f'<{tag_name}'), string(f'</{tag_name}>')) \
            .then(content_parser)

    # Parser to match TargetFramework
    target_framework_parser = tag('TargetFramework', regex(r'[^\n]+'))

    # Parser to match PackageReference and extract Include and Version
    package_reference_parser = (string('<PackageReference ') >>
                                attribute_value('Include') +
                                attribute_value('Version') <<
                                string(' />')).map(tuple)

    # Parser to match ItemGroup and extract all PackageReferences
    item_group_parser = tag('ItemGroup', package_reference_parser.many())

    # Parser to match PropertyGroup and extract TargetFramework
    property_group_parser = tag('PropertyGroup', target_framework_parser)

    # Main parser for the .csproj file
    csproj_parser = (whitespace >>
                    property_group_parser +
                    item_group_parser.many() <<
                    whitespace)
    
    return csproj_parser


def map_to_transitivity(type_value: Optional[str]) -> Transitivity:
    if type_value == "Direct":
        return Transitivity(Direct())
    elif type_value == "Transitive":
        return Transitivity(Transitive())
    else:
        return Transitivity(Unknown())


def parse_dependencies_field(deps: Dict[str, JSON]) -> List[FoundDependency]:
    output = []
    unique_dependencies = set()  # Track unique combinations of package names and versions

    for framework, dep_json in deps.items():
        dependencies = dep_json.as_dict()

        for package, package_json in dependencies.items():
            fields = package_json.as_dict()
            version = fields.get("resolved")
            if not version:
                logger.info(f"no version for dependency: {package} in framework: {framework}")
                continue

            # Create a unique key for this package and version and check if it is already in the set; if so, skip this entry
            unique_key = f"{package}_{version.as_str()}"
            if unique_key in unique_dependencies:
                continue

            transitivity_str = fields.get("type")
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

            # Add the unique key to the set
            unique_dependencies.add(unique_key)

    return output


def parse_packages_lock(
    lockfile_path: Path, _manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, _parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, json_doc, ScaParserName(Jsondoc())),
        # DependencyFileToParse(manifest_path, get_csproj_parser(), ScaParserName(Xmldoc())) # change to CsProj parser after generating ATD
        # if manifest_path
        # else None,
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
