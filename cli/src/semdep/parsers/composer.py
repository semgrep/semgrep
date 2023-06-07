"""
Parsers for composer.lock files
Based on https://getcomposer.org/doc/01-basic-usage.md
"""
# Import necessary modules and classes
from pathlib import Path
from typing import List
from typing import Optional
from typing import Set

from semdep.parsers.util import json_doc
from semdep.parsers.util import ParserName
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Composer
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.verbose_logging import getLogger

# Instantiate logger
logger = getLogger(__name__)

# Function to parse the composer.json manifest file and return required and required-dev dependencies as a dictionary
def parse_composer_manifest(manifest_path: Path) -> Set[str]:
    manifest_json_opt = safe_path_parse(
        manifest_path, json_doc, ParserName.composer_lock
    )
    if not manifest_json_opt:
        logger.info("Failed to parse composer.json file")
        return set()

    else:
        manifest_json = manifest_json_opt.as_dict()
        manifest_deps = (
            set(manifest_json["require"].as_dict().keys())
            if "require" in manifest_json
            else set()
        )
        return manifest_deps


# Function to parse the composer.lock file and return a list of FoundDependency objects
def parse_composer_lock(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    lockfile_json_opt = safe_path_parse(
        lockfile_path, json_doc, ParserName.composer_lock
    )

    if not lockfile_json_opt:
        return []

    lockfile_json = lockfile_json_opt.as_dict()

    packages_list = []
    packages = lockfile_json.get("packages")
    if packages is not None:
        packages_list = packages.as_list()

    packages_dev_list = []
    packages_dev = lockfile_json.get("packages-dev")
    if packages_dev is not None:
        packages_dev_list = packages_dev.as_list()

    if packages_list is not None and packages_dev_list is not None:
        deps = packages_list + packages_dev_list
    elif packages_list is not None:
        deps = packages_list
    elif packages_dev_list is not None:
        deps = packages_dev_list
    else:
        logger.debug("Found package-lock with no 'packages'")
        return []

    # Get manifest dependencies by parsing composer.json manifest
    manifest_deps: Set[str]
    if manifest_path:
        manifest_deps = parse_composer_manifest(manifest_path)
    else:
        manifest_deps = set()

    # Initialize output list
    output = []
    # Iterate through the combined packages and packages-dev lists
    for dep in deps:
        fields = dep.as_dict()
        if "version" not in fields:
            logger.info(f"no version for dependency: {dep.as_dict()['name']}")
            continue

        # Extract version and package name from dependency fields
        version = fields["version"].as_str()
        package = fields["name"].as_str()

        # Initialize resolved_url and allowed_hashes
        resolved_url = None
        allowed_hashes = {}
        dist = fields.get("dist")
        if dist:
            dist_dict = dist.as_dict()
            if "url" in dist_dict:
                resolved_url = dist_dict["url"].as_str()
            if "shasum" in dist_dict:
                shasum = dist_dict["shasum"].as_str()
                if shasum:
                    allowed_hashes = {"sha1": [shasum]}

        # Append FoundDependency object to output list
        output.append(
            FoundDependency(
                package=package,
                version=version,
                ecosystem=Ecosystem(Composer()),
                resolved_url=resolved_url,
                allowed_hashes=allowed_hashes,
                transitivity=transitivity(manifest_deps, [package]),
                line_number=dep.line_number,
            )
        )
    # Return the output list containing FoundDependency objects
    return output
