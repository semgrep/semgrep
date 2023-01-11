from typing import Dict
from typing import List
from typing import Optional

from packaging.version import InvalidVersion
from packaging.version import Version

from semdep.parsers.util import extract_npm_lockfile_hash
from semdep.parsers.util import JSON
from semdep.parsers.util import json_doc
from semdep.parsers.util import transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def parse_package_lock(
    lockfile_text: str, manifest_text: Optional[str]
) -> List[FoundDependency]:
    lockfile_json = json_doc.parse(lockfile_text).as_dict()
    # Newer versions of NPM (>= v7) use 'packages'
    # But 'dependencies' is kept up to date, and 'packages' uses relative, not absolute names
    # https://docs.npmjs.com/cli/v8/configuring-npm/package-lock-json
    deps = lockfile_json.get("dependencies")
    if deps is None:
        logger.debug("Found package-lock with no 'dependencies'")
        return []

    if manifest_text:
        manifest_json = json_doc.parse(manifest_text).as_dict()
        manifest_deps = (
            set(manifest_json["dependencies"].as_dict().keys())
            if "dependencies" in manifest_json
            else set()
        )
    else:
        manifest_deps = None

    def parse_deps(deps: Dict[str, JSON], nested: bool) -> List[FoundDependency]:
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
            try:
                Version(version.as_str())
            # Version was a github commit
            except InvalidVersion:
                logger.info(f"no version for dependency: {package}")
                continue

            resolved_url = fields["resolved"].as_str() if "resolved" in fields else None
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
                output.extend(parse_deps(nested_deps.as_dict(), True))
        return output

    return parse_deps(deps.as_dict(), False)
