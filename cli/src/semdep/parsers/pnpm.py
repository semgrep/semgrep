"""
Parsers for pnpm-lock.yaml files
Based on https://github.com/pnpm/spec/blob/master/lockfile/5.2.md
"""
import re
from dataclasses import dataclass
from pathlib import Path
from typing import List
from typing import Optional
from typing import Tuple
from typing import Union

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parsers.util import DependencyFileToParse
from semdep.parsers.util import DependencyParserError
from semdep.parsers.util import safe_parse_lockfile_and_manifest
from semdep.parsers.util import transitivity
from semgrep.rule_lang import parse_yaml_preserve_spans
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyChild
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName


# Pre-compiled regex patterns for better performance since they are used multiple times
PACKAGE_KEY_PATTERN_PRE_V6 = re.compile(r"/(.+)/([^/]+)")
PACKAGE_KEY_PATTERN_POST_V6 = re.compile(r"/?(.+?)@([^(@]+)")
PACKAGE_KEY_PATTERN_POST_V9 = re.compile(r"(.+?)@([^(@]+)")
SNAPSHOT_KEY_PATTERN = re.compile(
    r"^(?P<package>.+?)@(?P<version>[^\(]+)(?P<contexts>(\(.+?\))*)$"
)
BASE_VERSION_PATTERN_SIMPLE = re.compile(r"(?P<version>^[^\(]+)")
BASE_VERSION_PATTERN = re.compile(r"^(?P<version>[^\(\+]+(?:\+[^\(]+)?)\(")
BASE_VERSION_PATTERN_LESS_SIMPLE = re.compile(
    r"(?:/[@]?[^@]+@)?(?P<version>[^\(\+]+(?:\+[^\(]+)?)"
)
PACKAGE_ALIAS_PATTERN = re.compile(r"^(?P<package>[^\(\)@]+)@(?P<version>[^\(\)]+)")


# Helpers
def get_key_values(yaml: YamlTree[YamlMap], field: str) -> List[str]:
    """
    Extracts all keys from a specified field in a YamlMap.

    Args:
        yaml: A YamlTree[YamlMap] representing a section of the YAML file.
        field: The key whose values need to be extracted.

    Returns:
        A list of strings representing the keys in the specified field.
    """
    try:
        map = yaml.value[field].value
        return [k.value for k in map.keys()] if map else []
    except KeyError:
        return []


def validate_lockfile_dependencies_completeness(
    adjacency_map: dict[Tuple[str, str], list[DependencyChild]],
    all_deps_set: set[Tuple[str, str]],
    lockfile_path: Path,
) -> list[DependencyParserError]:
    """
    Validates the completeness of the lockfile by ensuring that all dependencies in the adjacency map
    are resolved in the lockfile.

    Args:
        adjacency_map: A map of packages to their dependency children.
        all_deps_set: A set of all resolved packages in the lockfile.
        lockfile_path: Path to the lockfile.

    Returns:
        A list of DependencyParserError objects if any dependencies are unresolved.
    """
    all_children = {
        (child.package, child.version)
        for children in adjacency_map.values()
        for child in children
    }
    missing_packages = all_children - all_deps_set

    if not missing_packages:
        return []

    error_str = "The following packages are referenced as dependencies of other packages but are not resolved in the lockfile:"

    for package, version in missing_packages:
        error_str += f"\n- {package}@{version}"

    return [
        DependencyParserError(
            path=out.Fpath(str(lockfile_path)),
            parser=ScaParserName(out.PPnpmLock()),
            reason=error_str,
        )
    ]


def build_package_key_mapping_post_v9(
    parsed_lockfile: YamlTree[YamlMap],
) -> dict[str, str]:
    """
    Builds a mapping of canonical package keys to their raw keys from a parsed pnpm-lock.yaml for versions post 9.
    This function processes the 'snapshots' section of the lockfile to handle peer dependencies and context-specific resolutions.
    This necessary to match the package key under "packages" to the correct package key under "snapshots".

    Args:
        parsed_lockfile (YamlTree[YamlMap]): The parsed PNPM lockfile.
    Returns:
        dict[Tuple[str, str], List[DependencyChild]]: A dictionary where the keys are canonical package keys (e.g., '@vitest/coverage-v8@2.0.5')
        and the values are the corresponding raw keys from the "snapshots" section of the lockfile.

    Example key/value pair:
        key: '@vitest/coverage-v8@2.0.5'
        value: '@vitest/coverage-v8@2.0.5(vitest@2.0.5(@types/node@20.12.7)(jsdom@25.0.1)(lightningcss@1.23.0)(sass@1.70.0)(terser@5.27.0))'
    """

    snapshots: YamlTree[YamlMap] | None = parsed_lockfile.value.get("snapshots")
    if not snapshots or not isinstance(snapshots.value, YamlMap):
        return {}

    package_key_mapping: dict[str, str] = {}

    for key, _value in snapshots.value.items():
        package_key_match = SNAPSHOT_KEY_PATTERN.match(key.value)
        if not package_key_match:
            continue

        canonical_package_key = (
            f"{package_key_match.group('package')}@{package_key_match.group('version')}"
        )

        package_key_mapping[canonical_package_key] = key.value

    return package_key_mapping


def extract_base_version(version_string: str) -> str:
    """
    Extracts the base version from a version string, ignoring any additional context.

    Args:
        version_string: A string like '8.2.0(eslint@9.9.1)(typescript@5.5.4)'.

    Returns:
        The base version string, e.g., '8.2.0' or '8.2.0+build'.
    """
    match = BASE_VERSION_PATTERN_LESS_SIMPLE.match(version_string)
    return match.group("version") if match else version_string


@dataclass
class ParseResult:
    package: str
    version: str


def parse_dependency_version(version_str: str) -> ParseResult:
    """
    Parses a dependency version string into its components.

    Examples:
    - "4.2.3" -> ParseResult("", "4.2.3")
    - "string-width@4.2.3" -> ParseResult("string-width", "4.2.3")
    - "@docusaurus/react-loadable@6.0.0" -> ParseResult("@docusaurus/react-loadable", "6.0.0")
    - "/@pnpm/node-fetch@1.0.0" -> ParseResult("@pnpm/node-fetch", "1.0.0")
    """
    if not version_str:
        return ParseResult("", "")

    base_version = version_str.split("(")[0]

    if base_version.startswith("/"):
        base_version = base_version[1:]

    if "_" in base_version:
        base_version = base_version.split("_")[0]

    if "@" not in base_version:
        return ParseResult("", base_version)

    if base_version.startswith("@"):
        package_end = base_version.rindex("@")
        return ParseResult(
            package=base_version[:package_end], version=base_version[package_end + 1 :]
        )
    else:
        package, version = base_version.split("@", 1)
        return ParseResult(package=package, version=version)


def sanitize_dependency_post_v9(dependency: DependencyChild) -> DependencyChild:
    """
    Sanitizes a DependencyChild object by resolving aliases and removing contexts.

    Examples:
    1. Regular package:
       Input:  DependencyChild(package="string-width", version="4.2.3")
       Output: DependencyChild(package="string-width", version="4.2.3")

    2. Aliased package:
       Input:  DependencyChild(package="string-width-cjs", version="string-width@4.2.3")
       Output: DependencyChild(package="string-width", version="4.2.3")

    3. Scoped package:
       Input:  DependencyChild(package="react-loadable", version="@docusaurus/react-loadable@6.0.0")
       Output: DependencyChild(package="@docusaurus/react-loadable", version="6.0.0")
    """
    # Parse the version string to extract any embedded package name
    result = parse_dependency_version(dependency.version)

    # Use the package name from the version string if present, as it's the canonical name from the registry
    # (e.g. "@docusaurus/react-loadable" in "react-loadable: '@docusaurus/react-loadable@6.0.0'")
    canonical_package = result.package or dependency.package

    # Use the parsed version
    canonical_version = result.version

    return DependencyChild(package=canonical_package, version=canonical_version)


# Direct dependencies
def parse_direct_pre_v6(yaml: YamlTree[YamlMap]) -> List[str]:
    """
    Extracts direct dependencies for pnpm-lock.yaml version <=5.4.

    Args:
        yaml: A YamlTree[YamlMap] for the package section.

    Returns:
        A list of strings representing direct dependencies.
    """
    return get_key_values(yaml, "specifiers")


def parse_direct_post_v6(yaml: YamlTree[YamlMap]) -> List[str]:
    """
    Extracts direct dependencies for pnpm-lock.yaml version >5.4 <9.0.

    Args:
        yaml: A YamlTree[YamlMap] for the package section.

    Returns:
        A list of strings representing direct and dev dependencies.
    """
    return get_key_values(yaml, "dependencies") + get_key_values(
        yaml, "devDependencies"
    )


# Package key
def parse_package_key_pre_v6(key: str) -> Optional[Tuple[str, str]]:
    """
    Parses the package key for pnpm-lock.yaml version <=5.4.

    Args:
        key: The raw package key as a string (`/package/version` `/@babel/helper-string-parser/7.19.4:`)

    Returns:
        A tuple of (package_name, version) or None if parsing fails.
    """
    match = PACKAGE_KEY_PATTERN_PRE_V6.match(key)
    return match.groups() if match else None  # type: ignore


def parse_package_key_post_v6(key: str) -> Optional[Tuple[str, str]]:
    """
    Extracts the package name and version from a package key for pnpm-lock.yaml version >5.4 <9.0.

    Args:
        key: The raw package key as a string (/package@version or /@scope/package@version)
            (starting / seems optional https://github.com/pnpm/pnpm/pull/7752/files)

    Returns:
        A tuple of (package_name, version) or None if parsing fails.
    """
    match = PACKAGE_KEY_PATTERN_POST_V6.match(key)
    return match.groups() if match else None  # type: ignore


def parse_package_key_post_v9(key: str) -> Optional[Tuple[str, str]]:
    """
    Parses the package key for pnpm-lock.yaml version >=9.0.

    Args:
        key: The raw package key as a string. (package@version or '@scope/package@version')

    Returns:
        A tuple of (package_name, version) or None if parsing fails.
    """
    match = PACKAGE_KEY_PATTERN_POST_V9.match(key)
    return match.groups() if match else None  # type: ignore


# Dependency children
def parse_dependencies(
    dependencies: Union[YamlTree[YamlMap], None],
) -> List[DependencyChild]:
    """
    Parses a YamlMap of dependencies into a list of DependencyChild objects.

    Args:
        dependencies: A YamlMap where keys are dependency names and values are versions.

    Returns:
        A list of DependencyChild objects with package names and versions.
    """
    if not dependencies or not isinstance(dependencies.value, YamlMap):
        return []

    return [
        DependencyChild(package=package.value, version=version.value)
        for package, version in dependencies.value.items()
    ]


def parse_peer_dependencies(package_info: YamlTree[YamlMap]) -> List[str]:
    """
    Parses the peer dependencies of a package from a pnpm-lock.yaml file.
    The peer dependencies are stored in the "peerDependencies" key of the package, under the "packages" key.

    Example section:
    ```
        '@typescript-eslint/eslint-plugin@8.2.0':
            resolution: {integrity: sha512-02tJIs655em7fvt9gps/+4k4OsKULYGtLBPJfOsmOq1+3cdClYiF0+d6mHu6qDnTcg88wJBkcPLpQhq7FyDz0A==}
            engines: {node: ^18.18.0 || ^20.9.0 || >=21.1.0}
            peerDependencies:
                '@typescript-eslint/parser': ^8.0.0 || ^8.0.0-alpha.0
                eslint: ^8.57.0 || ^9.0.0
                typescript: '*'
            peerDependenciesMeta:
            typescript:
                optional: true
    ```
    Should return: ['@typescript-eslint/parser', 'eslint', 'typescript']
    """
    if not package_info.value or "peerDependencies" not in package_info.value:
        return []

    return get_key_values(package_info, "peerDependencies")


def parse_dep_children_pre_v9(
    package_info: YamlTree[YamlMap],
    full_file: YamlTree[YamlMap],
    package_key: str,
) -> List[DependencyChild]:
    """
    Parses the dependencies of a package from a pnpm-lock.yaml file (version >5.4 <9.0).

    Args:
        package_info: YamlMap containing the package information under "packages"
        full_file: The complete pnpm-lock.yaml file
        package_key: The key of the current package

    Returns:
        List of DependencyChild objects representing the dependencies

    Example package_info:
        dependencies:
          string-width-cjs: string-width@4.2.3
          react-loadable: @docusaurus/react-loadable@6.0.0
          node-fetch: /@pnpm/node-fetch@1.0.0
          core-loggers: 10.0.4(@pnpm/logger@5.2.0)
    """
    if not package_info.value or "dependencies" not in package_info.value:
        return []

    all_dependencies = parse_dependencies(package_info.value.get("dependencies"))

    sanitized_deps = []
    for dep in all_dependencies:
        parsed = parse_dependency_version(dep.version)
        sanitized_deps.append(
            DependencyChild(
                package=parsed.package or dep.package, version=parsed.version
            )
        )

    return sanitized_deps


def parse_dep_children_post_v9(
    package_info: YamlTree[YamlMap],
    full_file: YamlTree[YamlMap],
    package_key: str,
) -> List[DependencyChild]:
    """
    Logic to parse the yaml tree of a pnpm-lock.yaml file version >=9.0.
    The provided package_info is a YamlMap containing the package information of a single package under the key "packages".
    The provided full_file is the entire parsed yaml file.
    full_file is required because dependency relationships in pnpm-lock.yaml >9.0 are not stored under the package key.
    They are, instead, defined separately under the "snapshots" key.
    Logic will need to search the "snapshots" for the key that matches the package_key, and then parse the dependencies from there.
    This logic will parse the dependencies of the package and return a list of DependencyChild objects.
    """
    """
    Parses the dependencies of a package in a `pnpm-lock.yaml` file (version >=9.0).

    In pnpm-lock.yaml files version >=9.0, dependencies are structured in the "snapshots" section, which holds
    context-specific resolutions (e.g., peer dependencies, aliased packages). This function retrieves the true
    dependencies for a given package, filters out peer dependencies, and normalizes aliased and contextual versions.

    Args:
        package_info: A `YamlTree[YamlMap]` containing the package-specific information
                      under the "packages" key of the lockfile. Not used directly but included for API consistency.
        full_file: A `YamlTree[YamlMap]` representing the entire parsed YAML structure of the lockfile.
                   This is needed to access the "snapshots" section where dependency relationships are stored.
        canonical_package_key: The canonical package key (e.g., `@babel/core@7.21.0`) used to match peer dependencies
                               in the "packages" section.
        raw_package_key: The raw package key (e.g., `@babel/core@7.21.0(eslint@8.57.0)`) used to retrieve the
                         appropriate entry from the "snapshots" section.

    Returns:
        A list of `DependencyChild` objects representing the dependencies of the package.

    Example Workflow:
        - The function retrieves the relevant snapshot using the `raw_package_key` from the "snapshots" section.
        - Dependencies are parsed from the snapshot's "dependencies" field using `parse_dependencies`.
        - Peer dependencies are retrieved using `parse_peer_dependencies_post_9` and filtered out.
        - Aliased/contextual versions (e.g., `string-width@4.2.3(eslint@8.57.0)`) are sanitized
          to their canonical forms using `sanitize_dependency_post_9`.

    Example Input:
        raw_package_key = "@babel/core@7.21.0(eslint@8.57.0)"
        canonical_package_key = "@babel/core@7.21.0"

        Snapshot in lockfile:
        ```
        '@babel/core@7.21.0(eslint@8.57.0)':
            dependencies:
                '@babel/runtime': 7.21.0(eslint@8.57.0)
                chalk: 4.1.0
        ```

    Example Output:
        [
            DependencyChild(package="@babel/runtime", version="7.21.0"),
            DependencyChild(package="chalk", version="4.1.0"),
        ]

    Notes:
        - Peer dependencies are included in the "dependencies" field of the snapshot but are filtered out for correctness.
        - Aliased and contextual versions are normalized to their canonical forms.
    """
    all_snapshots: YamlTree[YamlMap] | None = full_file.value.get("snapshots")

    if not all_snapshots or not isinstance(all_snapshots.value, YamlMap):
        return []

    snapshot = all_snapshots.value.get(package_key)
    if not snapshot or "dependencies" not in snapshot.value:
        return []

    all_dependencies: List[DependencyChild] = parse_dependencies(
        snapshot.value.get("dependencies")
    )
    return [sanitize_dependency_post_v9(dep) for dep in all_dependencies]


# Main Course
def parse_pnpm(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> Tuple[List[FoundDependency], List[DependencyParserError]]:
    parsed_lockfile, _, errors = safe_parse_lockfile_and_manifest(
        DependencyFileToParse(
            lockfile_path,
            lambda text: parse_yaml_preserve_spans(
                text, str(lockfile_path), allow_null=True
            ),
            ScaParserName(out.PPnpmLock()),
        ),
        None,
    )

    if not parsed_lockfile or not isinstance(parsed_lockfile.value, YamlMap):
        return [], errors
    try:
        lockfile_version = float(parsed_lockfile.value["lockfileVersion"].value)
    except KeyError:
        return [], errors

    # This will be important later
    canonical_package_key_mapping: dict[str, str] = {}

    # Select the appropriate parsing functions based on the lockfile version
    if lockfile_version <= 5.4:
        parse_direct, parse_package_key, parse_dep_children = (
            parse_direct_pre_v6,
            parse_package_key_pre_v6,
            parse_dep_children_pre_v9,
        )
    elif lockfile_version < 9.0:
        parse_direct, parse_package_key, parse_dep_children = (
            parse_direct_post_v6,
            parse_package_key_post_v6,
            parse_dep_children_pre_v9,
        )
    else:
        parse_direct, parse_package_key, parse_dep_children = (
            parse_direct_post_v6,
            parse_package_key_post_v9,
            parse_dep_children_post_v9,
        )
        # Build a mapping of canonical package keys to their raw keys for lockfile versions >=9.0
        canonical_package_key_mapping = build_package_key_mapping_post_v9(
            parsed_lockfile
        )

    # Extract direct dependencies
    if "importers" in parsed_lockfile.value:
        direct_deps = {
            x
            for _, v in parsed_lockfile.value["importers"].value.items()
            for x in parse_direct(v)
        }
    else:
        direct_deps = set(parse_direct(parsed_lockfile))

    # Extract all dependencies
    try:
        package_map = parsed_lockfile.value.get("packages")
        if (
            not package_map
            or not isinstance(package_map, YamlTree)
            or not package_map.value
        ):
            return [], errors

        all_deps_list: List[Tuple[int, Tuple[str, str]]] = []
        all_deps_set: set[Tuple[str, str]] = set()
        adjacency_map: dict[Tuple[str, str], List[DependencyChild]] = {}

        for key, map in package_map.value.items():
            line: int = key.span.start.line
            if map.value and "name" in map.value and "version" in map.value:
                data = (map.value["name"].value, map.value["version"].value)
                all_deps_list.append((line, data))
                all_deps_set.add(data)
            else:
                data = parse_package_key(key.value)  # type: ignore[assignment]
                if data:
                    # re does not have a way for us to refine the type of the match to what we know it is
                    all_deps_list.append((line, data))
                    all_deps_set.add(data)
                else:
                    errors.append(
                        DependencyParserError(
                            path=out.Fpath(str(lockfile_path)),
                            parser=ScaParserName(out.PPnpmLock()),
                            reason=f"Could not parse package key {key.value}",
                            line=line,
                        )
                    )
            if data:
                adjacency_map[data] = parse_dep_children(
                    map,
                    parsed_lockfile,
                    canonical_package_key_mapping.get(key.value, key.value),
                )
    except KeyError:
        return [], errors

    # TODO: SC-2021 - Validate the completeness of the lockfile for dependency graph construction purposes
    # if has_path_to_transitivity_enabled:
    #     errors.extend(
    #         validate_lockfile_dependencies_completeness(
    #             adjacency_map, all_deps_set, lockfile_path
    #         )
    #     )

    output = []
    for line_number, (package_str, version_str) in all_deps_list:
        if not package_str or not version_str:
            continue

        output.append(
            FoundDependency(
                package=package_str,
                version=version_str,
                ecosystem=Ecosystem(Npm()),
                transitivity=transitivity(direct_deps, [package_str]),
                children=adjacency_map.get((package_str, version_str), []),
                line_number=line_number,
                allowed_hashes={},
                lockfile_path=Fpath(str(lockfile_path)),
                manifest_path=Fpath(str(manifest_path)) if manifest_path else None,
            )
        )
    return output, errors
