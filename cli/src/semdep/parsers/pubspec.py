# from typing import Dict
from typing import List
from typing import Optional
# import yaml
# from yaml import YAMLError

from semdep.parsers.util import safe_path_parse
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pubspec
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

print("test")

def parse_pubspec_lock(
    lockfile_content: str, manifest_content: Optional[str]
) -> List[FoundDependency]:
    deps = safe_path_parse(lockfile_content, poetry)
    if not deps:
        return []
    # logger.debug(f"lockfile_content: {lockfile_content}")
    # print(lockfile_content)
    # print("test")


    # print(lockfile_content)

# def parse_yaml(yaml_content: str) -> Dict:
#     try:
#         return yaml.safe_load(yaml_content)
#     except YAMLError as e:
#         raise e

# def transitivity(manifest_deps: set, package_names: List[str]) -> str:
#     if any(package_name in manifest_deps for package_name in package_names):
#         return "direct"
#     return "transitive"

# def parse_pubspec_manifest(manifest_content: str) -> Dict[str, set]:
#     logger.debug(f"manifest_content: {manifest_content}")

#     try:
#         manifest_data = parse_yaml(manifest_content)
#     except YAMLError as e:
#         logger.error(f"Error parsing manifest_content with parse_yaml: {str(e)}")
#         return {}

#     try:
#         dependencies = set(manifest_data.get("dependencies", {}).keys())
#         dev_dependencies = set(manifest_data.get("dev_dependencies", {}).keys())

#         return dependencies.union(dev_dependencies)
#     except YAMLError as e:
#         logger.info(f"Error parsing pubspec.yaml: {str(e)}")
#         return {}

# def parse_pubspec_lock(
#     lockfile_content: str, manifest_content: Optional[str]
# ) -> List[FoundDependency]:
#     logger.debug(f"lockfile_content: {lockfile_content}")

#     try:
#         lockfile_data = parse_yaml(lockfile_content)
#     except YAMLError as e:
#         logger.error(f"Error parsing lockfile_content with parse_yaml: {str(e)}")
#         return []

#     packages = lockfile_data.get("packages", {})

#     # if manifest_content:
#     #     manifest_deps = parse_pubspec_manifest(manifest_content)
#     # else:
#     #     manifest_deps = {}

#     output = []
#     for package, data in packages.items():
#         version = data["version"]
#         source = data["source"]

#         resolved_url = None
#         allowed_hashes = {}

#         if source == "hosted":
#             if "url" in data:
#                 resolved_url = data["url"]

#             if "checksum" in data:
#                 allowed_hashes = {"sha256": [data["checksum"]]}

#         line_number = data.start_mark.line if hasattr(data, "start_mark") else None

#         output.append(
#             FoundDependency(
#                 package=package,
#                 version=version,
#                 ecosystem=Ecosystem(Pubspec()),
#                 resolved_url=resolved_url,
#                 allowed_hashes=allowed_hashes,
#                 transitivity=transitivity(manifest_deps, [package]),
#                 line_number=line_number,
#             )
#         )

#     return output
