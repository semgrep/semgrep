import base64
import collections
import json
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Generator
from typing import List
from typing import Optional

from defusedxml import ElementTree as ET  # type: ignore
from packaging.version import InvalidVersion
from packaging.version import Version

from semgrep.error import SemgrepError
from semgrep.verbose_logging import getLogger

# NOTE: Defused XML doesn't export types :(


logger = getLogger(__name__)

from semdep.models import LockfileDependency
from semdep.models import PackageManagers


def extract_npm_lockfile_hash(s: str) -> Dict[str, List[str]]:
    """
    Go from:
        sha512-aePbxDmcYW++PaqBsJ+HYUFwCdv4LVvdnhBy78E57PIor8/OVvhMrADFFEDh8DHDFRv/O9i3lPhsENjO7QX0+A==
    To:
        sha512,
    """
    algorithm = s.split("-")[0]
    rest = s[len(algorithm) + 1 :]
    decode_base_64 = base64.b64decode(rest)
    return {algorithm: [base64.b16encode(decode_base_64).decode("ascii").lower()]}


def parse_Yarnlock_str(lockfile_text: str) -> Generator[LockfileDependency, None, None]:
    def extract_yarn_name(line: str) -> str:
        """
        "@babel/code-frame@^7.0.0", "@babel/code-frame@^7.10.4", "@babel/code-frame@^7.8.3":
        should parse as @babel/code-frame

        "@babel/types@^7.0.0", "@babel/types@^7.10.4", "@babel/types@^7.10.5", "@babel/types@^7.11.0", "@babel/types@^7.3.0", "@babel/types@^7.3.3", "@babel/types@^7.4.0", "@babel/types@^7.4.4", "@babel/types@^7.7.0", "@babel/types@^7.9.0":
        should parse as @babel/types

        lodash@4.17.18:
        should parse as lodash
        """
        if '"' not in line:
            return line.split("@")[0]
        else:
            first_quoted = "".join(line.split('"')[1:2])
            parsed_name = "@".join(first_quoted.split("@")[:-1])
            return parsed_name

    def remove_trailing_octothorpe(s: str) -> str:
        return "#".join(s.split("#")[:-1]) if "#" in s else s

    def remove_quotes(s: str) -> str:
        return s[1:-1]

    _comment, all_deps_text = lockfile_text.split("\n\n\n")
    dep_texts = all_deps_text.split("\n\n")
    if dep_texts == [""]:  # No dependencies
        yield from []
    for dep_text in dep_texts:
        lines = dep_text.split("\n")
        package_name = extract_yarn_name(lines[0])
        version = remove_quotes(lines[1].split()[1])
        if len(lines) >= 3 and lines[2].strip().startswith("resolved"):
            resolved = [
                remove_trailing_octothorpe(remove_quotes(lines[2].split()[1]).strip())
            ]
        else:
            resolved = []
        integrity = (
            lines[3].split()[1]
            if len(lines) > 3 and lines[3].strip().startswith("integrity")
            else None
        )
        yield LockfileDependency(
            package_name,
            version,
            PackageManagers.NPM,
            allowed_hashes=extract_npm_lockfile_hash(integrity) if integrity else {},
            resolved_url=resolved,
        )


def parse_NPM_package_lock_str(
    lockfile_text: str,
) -> Generator[LockfileDependency, None, None]:
    as_json = json.loads(lockfile_text)
    # Newer versions of NPM (>= v7) use 'packages'
    # But 'dependencies' is kept up to date, and 'packages' uses relative, not absolute names
    # https://docs.npmjs.com/cli/v8/configuring-npm/package-lock-json
    if "dependencies" in as_json:
        deps = as_json["dependencies"]
    elif "packages" in as_json:
        deps = as_json["packages"]
    else:
        logger.info("Found package-lock with no 'dependencies' or 'packages'")
        return

    for dep, dep_blob in deps.items():
        version = dep_blob.get("version")
        if not version:
            logger.info(f"no version for dependency: {dep}")
            continue
        try:
            Version(version)
        # Version was a github commit
        except InvalidVersion:
            logger.info(f"no version for dependency: {dep}")
            continue

        resolved_url = dep_blob.get("resolved")
        integrity = dep_blob.get("integrity")
        yield LockfileDependency(
            dep,
            version,
            PackageManagers.NPM,
            allowed_hashes=extract_npm_lockfile_hash(integrity) if integrity else {},
            resolved_url=[resolved_url] if resolved_url else None,
        )


def parse_Pipfile_str(lockfile_text: str) -> Generator[LockfileDependency, None, None]:
    def extract_pipfile_hashes(
        hashes: List[str],
    ) -> Dict[str, List[str]]:
        output = collections.defaultdict(list)
        for h in hashes:
            algorithm = h.split(":")[0]
            rest = h[len(algorithm) + 1 :]  # pipfile is already in base16
            output[algorithm].append(rest.lower())
        return output

    def parse_dependency_blob(
        root_blob: Dict[str, Any]
    ) -> Generator[LockfileDependency, None, None]:
        for dep in root_blob:
            dep_blob = root_blob[dep]
            version = dep_blob.get("version", None)
            if not version:
                logger.info(f"no version for dependency: {dep}")
            else:
                version = version.replace("==", "")
                yield LockfileDependency(
                    dep,
                    version,
                    PackageManagers.PYPI,
                    resolved_url=None,
                    allowed_hashes=extract_pipfile_hashes(dep_blob["hashes"])
                    if "hashes" in dep_blob
                    else {},
                )

    as_json = json.loads(lockfile_text)
    yield from parse_dependency_blob(as_json["default"])
    develop_deps = as_json.get("develop")
    if develop_deps is not None:
        yield from parse_dependency_blob(develop_deps)


def parse_Gemfile_str(lockfile_text: str) -> Generator[LockfileDependency, None, None]:
    def parse_dep(s: str) -> LockfileDependency:
        # s == "    $DEP ($VERSION)"
        dep, paren_version = s.strip().split(" ")
        version = paren_version[1:-1]
        return LockfileDependency(
            dep, version, PackageManagers.GEM, resolved_url=None, allowed_hashes={}
        )

    lines = lockfile_text.split("\n")
    # No dependencies specified
    if "GEM" not in lines:
        yield from []
    GEM_idx = lines.index("GEM") + 1
    GEM_end_idx = lines[GEM_idx:].index(
        ""
    )  # A line with a single \n becomes the empty string upon splitting by \n
    all_deps = lines[GEM_idx:GEM_end_idx]
    yield from (
        parse_dep(dep) for dep in all_deps if dep[:4] == " " * 4 and dep[5] != " "
    )


def parse_Go_sum_str(lockfile_text: str) -> Generator[LockfileDependency, None, None]:
    # We currently ignore the +incompatible flag, pseudo versions, and the difference between a go.mod and a direct download
    def parse_dep(s: str) -> LockfileDependency:
        dep, version, hash = s.split()
        # drop 'v'
        version = version[1:]

        # drop /go.mod
        if "/" in version:
            version = version[: version.index("/")]
        # drop +incompatible
        if "+" in version:
            version = version[: version.index("+")]

        # drop pseudo version
        if "-" in version:
            version = version[: version.index("-")]

        # drop h1: and =
        hash = hash[3:-1]
        return LockfileDependency(
            dep,
            version,
            PackageManagers.GOMOD,
            # go.sum dep names are already URLs
            resolved_url=[dep],
            allowed_hashes={"gomod": [hash]},
        )

    lines = lockfile_text.split("\n")
    if len(lines[-1].split()) != 3:
        # Sometimes the last line will contain a carriage return character
        lines = lines[:-1]
    yield from (parse_dep(dep) for dep in lines)


def parse_Cargo_str(lockfile_text: str) -> Generator[LockfileDependency, None, None]:
    def parse_dep(s: str) -> LockfileDependency:
        lines = s.split("\n")[1:]
        dep = lines[0].split("=")[1].strip()[1:-1]
        version = lines[1].split("=")[1].strip()[1:-1]
        if len(lines) >= 3 and lines[3].startswith("checksum"):
            hash = {"sha256": [lines[3].split("=")[1].strip()[1:-1]]}
        else:
            hash = {}
        return LockfileDependency(
            dep,
            version,
            PackageManagers.CARGO,
            resolved_url=None,
            allowed_hashes=hash,
        )

    deps = lockfile_text.split("[[package]]")[1:]
    yield from (parse_dep(dep) for dep in deps)


def parse_Pom_str(manifest_text: str) -> Generator[LockfileDependency, None, None]:
    NAMESPACE = "{http://maven.apache.org/POM/4.0.0}"

    def parse_dep(
        properties: Any,
        # Optional[ET.Element]
        el: Any
        # ET.Element
    ) -> Optional[LockfileDependency]:
        dep_el = el.find(f"{NAMESPACE}artifactId")
        if dep_el is None:
            return None
        dep = dep_el.text
        if dep is None:
            return None
        version_el = el.find(f"{NAMESPACE}version")
        if version_el is None:
            return None
        version = version_el.text
        if version is None:
            return None
        if version[0] == "$":
            if properties is None:
                raise SemgrepError("invalid pom.xml?")

            version = version[2:-1]
            prop_version = properties.find(f"{NAMESPACE}{version}")
            if prop_version is None:
                return None
            version = prop_version.text
            if version is None:
                return None

        try:
            # pom.xml does not specify an exact version, so we give up
            Version(version)
        except InvalidVersion:
            return None

        return LockfileDependency(
            dep, version, PackageManagers.MAVEN, resolved_url=None, allowed_hashes={}
        )

    root = ET.fromstring(manifest_text)
    deps = root.find(f"{NAMESPACE}dependencies")
    if deps is None:
        raise SemgrepError("No dependencies in pom.xml?")
    properties = root.find(f"{NAMESPACE}properties")
    for dep in deps:
        dep_opt = parse_dep(properties, dep)
        if dep_opt:
            yield dep_opt


def parse_Gradle_str(lockfile_text: str) -> Generator[LockfileDependency, None, None]:
    def parse_dep(line: str) -> Optional[LockfileDependency]:
        dep = line.split(":")
        if len(dep) != 3:
            logger.info("Parse error in gradle lockfile")
            return None
        _, name, version = dep
        version, _ = version.split("=")
        try:
            Version(version)
        except InvalidVersion:
            logger.info("No valid version found for {name}")
            return None
        return LockfileDependency(
            name, version, PackageManagers.GRADLE, resolved_url=None, allowed_hashes={}
        )

    lines = lockfile_text.splitlines()[
        3:-1
    ]  # Drop the 3 comment lines at the top and the empty= line from the bottom
    deps = [parse_dep(line) for line in lines]
    yield from (dep for dep in deps if dep)


def parse_Poetry_str(lockfile_text: str) -> Generator[LockfileDependency, None, None]:
    def parse_dep(s: str) -> LockfileDependency:
        lines = s.split("\n")[1:]
        dep = lines[0].split("=")[1].strip()[1:-1]
        version = lines[1].split("=")[1].strip()[1:-1]
        return LockfileDependency(
            dep,
            version,
            PackageManagers.PYPI,
            resolved_url=None,
            allowed_hashes={},
        )

    deps = lockfile_text.split("[[package]]")[1:]  # drop the empty string at the start
    yield from (parse_dep(dep) for dep in deps)


LOCKFILE_PARSERS = {
    "pipfile.lock": parse_Pipfile_str,  # Python
    "yarn.lock": parse_Yarnlock_str,  # JavaScript
    "package-lock.json": parse_NPM_package_lock_str,  # JavaScript
    "gemfile.lock": parse_Gemfile_str,  # Ruby
    "go.sum": parse_Go_sum_str,  # Go
    "cargo.lock": parse_Cargo_str,  # Rust
    "pom.xml": parse_Pom_str,  # Java
    "gradle.lockfile": parse_Gradle_str,  # Java
    "poetry.lock": parse_Poetry_str,  # Python
}


def parse_lockfile_str(
    lockfile_text: str, filepath_for_reference: Path
) -> Generator[LockfileDependency, None, None]:
    # coupling with the github action, which decides to send files with these names back to us
    filepath = filepath_for_reference.name.lower()
    if filepath in LOCKFILE_PARSERS:
        try:
            yield from LOCKFILE_PARSERS[filepath](lockfile_text)
        # Such a general except clause is suspect, but the parsing error could be any number of
        # python errors, since our parsers are just using stdlib string processing functions
        # This will avoid catching dangerous to catch things like KeyboardInterrupt and SystemExit
        except Exception as e:
            logger.error(f"Failed to parse {filepath_for_reference} with exception {e}")
            yield from []
    else:
        raise SemgrepError(
            f"don't know how to parse this filename: {filepath_for_reference}"
        )
