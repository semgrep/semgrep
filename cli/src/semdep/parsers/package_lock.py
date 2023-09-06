"""
Parser for package-lock.json files
Based on https://docs.npmjs.com/cli/v9/configuring-npm/package-lock-json
"""
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import extract_npm_lockfile_hash
from semdep.parsers.util import JSON
from semdep.parsers.util import json_doc
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Jsondoc
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def parse_packages_field(deps: Dict[str, JSON]) -> List[FoundDependency]:
    try:
        manifest_deps = set(deps[""].as_dict()["dependencies"].as_dict().keys())
    except KeyError:
        manifest_deps = None
    output = []
    for package, dep_json in deps.items():
        fields = dep_json.as_dict()
        version = fields.get("version")
        package_name = package[
            package.rfind("node_modules") + 13 :
        ]  # we only want the stuff after the final 'node_modules'
        if not version:
            logger.info(f"no version for dependency: {package}")
            continue
        resolved_url_json = fields.get("resolved")
        if resolved_url_json and not isinstance(resolved_url_json.value, str):
            resolved_url = None
        else:
            resolved_url = resolved_url_json.as_str() if resolved_url_json else None
        integrity = fields["integrity"].as_str() if "integrity" in fields else None

        nested = package.count("node_modules") > 1
        output.append(
            FoundDependency(
                package=package_name,
                version=version.as_str(),
                ecosystem=Ecosystem(Npm()),
                allowed_hashes=extract_npm_lockfile_hash(integrity)
                if integrity
                else {},
                resolved_url=resolved_url,
                transitivity=Transitivity(Transitive())
                if nested
                else transitivity(manifest_deps, [package]),
                line_number=dep_json.line_number,
            )
        )
    return output


def parse_dependencies_field(
    deps: Dict[str, JSON], manifest_deps: Optional[Set[str]], nested: bool
) -> List[FoundDependency]:
    # Dependency dicts in a package-lock.json can be nested:
    # {"foo" : {stuff, "dependencies": {"bar": stuff, "dependencies": {"baz": stuff}}}}
    # So we need to handle them recursively
    output = []
    for package, dep_json in deps.items():
        fields = dep_json.as_dict()
        version = fields.get("version")
        if not version:
            logger.info(f"no version for dependency: {package}")
            continue
        resolved_url_json = fields.get("resolved")
        if resolved_url_json and not isinstance(resolved_url_json.value, str):
            resolved_url = None
        else:
            resolved_url = resolved_url_json.as_str() if resolved_url_json else None

        integrity = fields["integrity"].as_str() if "integrity" in fields else None
        output.append(
            FoundDependency(
                package=package,
                version=version.as_str(),
                ecosystem=Ecosystem(Npm()),
                allowed_hashes=extract_npm_lockfile_hash(integrity)
                if integrity
                else {},
                resolved_url=resolved_url,
                transitivity=Transitivity(Transitive())
                if nested
                else transitivity(manifest_deps, [package]),
                line_number=dep_json.line_number,
            )
        )
        nested_deps = fields.get("dependencies")
        if nested_deps:
            output.extend(
                parse_dependencies_field(nested_deps.as_dict(), manifest_deps, True)
            )
    return output


def parse_package_lock(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, json_doc, ScaParserName(Jsondoc())),
        DependencyFileToParse(manifest_path, json_doc, ScaParserName(Jsondoc()))
        if manifest_path
        else None,
    )

    if not parsed_lockfile:
        return [], errors

    lockfile_json = parsed_lockfile.as_dict()

    lockfile_version_opt = lockfile_json.get("lockfileVersion")
    if not lockfile_version_opt:
        return [], errors

    lockfile_version = lockfile_version_opt.as_int()

    # v3 lockfiles have only 'packages', while previous versions have either only 'dependencies', or both
    # https://docs.npmjs.com/cli/v8/configuring-npm/package-lock-json
    if lockfile_version == 3:
        deps = lockfile_json.get("packages")
        if deps is None:
            logger.debug("Found package-lock with no 'packages'")
            return [], errors
        return parse_packages_field(deps.as_dict()), errors
    else:
        deps = lockfile_json.get("dependencies")
        if deps is None:
            logger.debug("Found package-lock with no 'dependencies'")
            return [], errors

        if not parsed_manifest:
            manifest_deps = None
        else:
            manifest_json = parsed_manifest.as_dict()
            manifest_deps = (
                set(manifest_json["dependencies"].as_dict().keys())
                if "dependencies" in manifest_json
                else set()
            )

        return parse_dependencies_field(deps.as_dict(), manifest_deps, False), errors
