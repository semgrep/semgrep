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

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import extract_npm_lockfile_hash
from semdep.parsers.util import JSON
from semdep.parsers.util import json_doc
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def parse_package_name(package_path: str) -> str:
    """
    Utility method for parsing a package name from the "packages" field
    Splits the package_path and uses the last element of each path as the dependency's name. In some cases, the path
    may contain a scope (https://docs.npmjs.com/cli/v8/using-npm/scope) in the second-to-last path component,
    so we check for that as well.
    """
    split_package_path = package_path.split("/")
    if (
        len(split_package_path) >= 2 and "@" in split_package_path[-2]
    ):  # The dependency has a scope, so include it in the name
        return "/".join(split_package_path[-2:])
    else:
        return split_package_path[-1]


def parse_packages_field(
    lockfile_path: Path,
    deps: Dict[str, JSON],
    manifest_path: Optional[Path],
) -> List[FoundDependency]:
    try:
        manifest_deps = set(deps[""].as_dict()["dependencies"].as_dict().keys())
    except KeyError:
        manifest_deps = None
    output = []
    for package, dep_json in deps.items():
        fields = dep_json.as_dict()
        version = fields.get("version")
        package_name = parse_package_name(package)
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
                transitivity=Transitivity(Transitive()) if nested
                # The manifest stores the pure package names but the deps names are all relative paths (prefix'd with 'node_modules'),
                # so check to see if `package_name` (without the 'node_modules' prefix) is present in the manifest.
                # https://docs.npmjs.com/cli/v10/configuring-npm/package-lock-json#packages
                else transitivity(manifest_deps, [package_name]),
                line_number=dep_json.line_number,
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
            )
        )
    return output


def parse_dependencies_field(
    lockfile_path: Path,
    deps: Dict[str, JSON],
    manifest_deps: Optional[Set[str]],
    nested: bool,
    manifest_path: Optional[Path],
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
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
            )
        )
        nested_deps = fields.get("dependencies")
        if nested_deps:
            output.extend(
                parse_dependencies_field(
                    lockfile_path,
                    nested_deps.as_dict(),
                    manifest_deps,
                    True,
                    manifest_path,
                )
            )
    return output


def parse_package_lock(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, parsed_manifest, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(lockfile_path, json_doc, ScaParserName(out.PJsondoc())),
        DependencyFileToParse(manifest_path, json_doc, ScaParserName(out.PJsondoc()))
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
        return (
            parse_packages_field(lockfile_path, deps.as_dict(), manifest_path),
            errors,
        )
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

        return (
            parse_dependencies_field(
                lockfile_path, deps.as_dict(), manifest_deps, False, manifest_path
            ),
            errors,
        )
