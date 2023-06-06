"""
Parsers for composer.lock files
Based on https://getcomposer.org/doc/01-basic-usage.md
"""
# Import necessary modules and classes
from collections import defaultdict
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional

from semdep.parsers.util import json_doc
from semdep.parsers.util import safe_path_parse
from semdep.parsers.util import transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Composer
from semgrep.verbose_logging import getLogger

# Instantiate logger
logger = getLogger(__name__)

# Function to parse the composer.json manifest file and return required and required-dev dependencies as a dictionary
def parse_composer_manifest(manifest_path: Path) -> Dict[str, set]:
    manifest_json_opt = safe_path_parse(manifest_path, json_doc)
    if not manifest_json_opt:
        logger.info("Failed to parse composer.json file")
        return {}

    try:
        manifest_json = manifest_json_opt.as_dict()

        # Extract required and required-dev dependencies
        required = set(manifest_json.get("require", {}).as_dict().keys())
        required_dev = set(manifest_json.get("require-dev", {}).as_dict().keys())

        # Return a set containing both required and required-dev dependencies
        return required.union(required_dev)
    except Exception as e:
        logger.info(f"Error parsing composer.json: {str(e)}")
        return {}


# Function to parse the composer.lock file and return a list of FoundDependency objects
def parse_composer_lock(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    lockfile_json_opt = safe_path_parse(lockfile_path, json_doc)

    lockfile_json = lockfile_json_opt.as_dict()

    # Extract packages and packages-dev from lockfile_json
    packages = lockfile_json.get("packages", []).as_list() or []
    packages_dev = lockfile_json.get("packages-dev", []).as_list() or []

    # Combine packages and packages-dev lists
    deps = packages + packages_dev

    # Get manifest dependencies by parsing composer.json manifest
    if manifest_path:
        manifest_deps = parse_composer_manifest(manifest_path)
    else:
        manifest_deps = {}

    # Initialize output list
    output = []
    # Iterate through the combined packages and packages-dev lists
    for dep in deps:
        fields = dep.as_dict()
        if "version" not in fields:
            logger.info(f"no version for dependency: {dep['name']}")
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
