import base64
import collections
import json
from functools import lru_cache
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Generator
from typing import List
from typing import Optional
from typing import Tuple

import tomli
from defusedxml import ElementTree as ET  # type: ignore
from packaging.version import InvalidVersion
from packaging.version import Version

from semgrep.error import SemgrepError
from semgrep.verbose_logging import getLogger

# NOTE: Defused XML doesn't export types :(


logger = getLogger(__name__)

from semgrep.semgrep_interfaces.semgrep_output_v0 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v0 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v0 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v0 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v0 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v0 import Gem
from semgrep.semgrep_interfaces.semgrep_output_v0 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v0 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v0 import Gradle
from semgrep.semgrep_interfaces.semgrep_output_v0 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v0 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v0 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v0 import Unknown


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


def parse_yarn(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    def version_sources(line: str) -> Tuple[str, List[Tuple[str, str]]]:
        # Takes a (multi-)key from the yarn.lock file and turns it into a list
        # A line might look like this
        # "foo@^1.0.0", "foo@^1.1.1":
        # This produces [("foo","^1.0.0"),("foo","^1.1.1")]
        constraint_strs = line[:-1].split(",")
        constraints = []
        for c in constraint_strs:
            c = c.strip('" ')
            if c[0] == "@":
                name, constraint = c[1:].split("@")
                name = "@" + name
            else:
                name, constraint = c.split("@")
            constraints.append((name, constraint))
        return constraints[0][0], constraints

    def remove_trailing_octothorpe(s: str) -> str:
        return "#".join(s.split("#")[:-1]) if "#" in s else s

    if manifest_text:
        manifest = json.loads(manifest_text)
        manifest_deps = manifest["dependencies"] if "dependencies" in manifest else {}
    else:
        manifest_deps = None
    lockfile_text = "\n".join(
        line + f" {i}" if line.strip().startswith("version") else line
        for i, line in enumerate(lockfile_text.split("\n"))
    )
    _comment, all_deps_text = lockfile_text.split("\n\n\n")
    dep_texts = all_deps_text.split("\n\n")
    if dep_texts == [""]:  # No dependencies
        return
    for dep_text in dep_texts:
        lines = dep_text.split("\n")
        package_name, constraints = version_sources(lines[0])
        _, version, line_number = lines[1].split()
        version = version.strip('" ')
        if len(lines) >= 3 and lines[2].strip().startswith("resolved"):
            resolved = remove_trailing_octothorpe(lines[2].split()[1].strip('" '))
        else:
            resolved = None
        integrity = (
            lines[3].split()[1]
            if len(lines) > 3 and lines[3].strip().startswith("integrity")
            else None
        )
        if manifest_deps is None:
            transitivity = Transitivity(Unknown())
        else:
            if package_name in manifest_deps:
                transitivity = Transitivity(
                    Direct()
                    if (package_name, manifest_deps[package_name]) in constraints
                    else Transitive()
                )
            else:
                transitivity = Transitivity(Transitive())

        yield FoundDependency(
            package=package_name,
            version=version,
            ecosystem=Ecosystem(Npm()),
            allowed_hashes=extract_npm_lockfile_hash(integrity) if integrity else {},
            resolved_url=resolved,
            transitivity=transitivity,
            line_number=int(line_number) + 1,
        )


def parse_package_lock(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    lockfile_text = "\n".join(
        line + f'"line_number": {i},' if line.strip().startswith('"version"') else line
        for i, line in enumerate(lockfile_text.split("\n"))
    )
    as_json = json.loads(lockfile_text)
    # Newer versions of NPM (>= v7) use 'packages'
    # But 'dependencies' is kept up to date, and 'packages' uses relative, not absolute names
    # https://docs.npmjs.com/cli/v8/configuring-npm/package-lock-json
    if "dependencies" in as_json:
        deps = as_json["dependencies"]
    else:
        logger.debug("Found package-lock with no 'dependencies'")
        return
    if manifest_text:
        manifest = json.loads(manifest_text)
        manifest_deps = manifest["dependencies"] if "dependencies" in manifest else {}
    else:
        manifest_deps = None
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
        line_number = dep_blob.get("line_number")
        if manifest_deps:
            transitivity = (
                Transitivity(Direct())
                if dep in manifest_deps
                else Transitivity(Transitive())
            )
        else:
            transitivity = Transitivity(Unknown())

        resolved_url = dep_blob.get("resolved")
        integrity = dep_blob.get("integrity")
        yield FoundDependency(
            package=dep,
            version=version,
            ecosystem=Ecosystem(Npm()),
            allowed_hashes=extract_npm_lockfile_hash(integrity) if integrity else {},
            resolved_url=resolved_url,
            transitivity=transitivity,
            line_number=int(line_number) + 1 if line_number else None,
        )


def parse_pipfile(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    lockfile_text = "\n".join(
        line + f', "line_number": {i}' if line.strip().startswith('"version"') else line
        for i, line in enumerate(lockfile_text.split("\n"))
    )
    manifest = tomli.loads(manifest_text) if manifest_text else None
    if manifest:
        manifest_deps = manifest["packages"] if "packages" in manifest else {}
    else:
        manifest_deps = None

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
    ) -> Generator[FoundDependency, None, None]:
        for dep in root_blob:
            dep_blob = root_blob[dep]
            version = dep_blob.get("version")
            if not version:
                logger.info(f"no version for dependency: {dep}")
            else:
                version = version.replace("==", "")
                line_number = dep_blob.get("line_number")
                if manifest_deps:
                    transitivity = (
                        Transitivity(Direct())
                        if dep in manifest_deps
                        else Transitivity(Transitive())
                    )
                else:
                    transitivity = Transitivity(Unknown())
                yield FoundDependency(
                    package=dep,
                    version=version,
                    ecosystem=Ecosystem(Pypi()),
                    resolved_url=None,
                    allowed_hashes=extract_pipfile_hashes(dep_blob["hashes"])
                    if "hashes" in dep_blob
                    else {},
                    transitivity=transitivity,
                    line_number=int(line_number) + 1 if line_number else None,
                )

    as_json = json.loads(lockfile_text)
    yield from parse_dependency_blob(as_json["default"])
    develop_deps = as_json.get("develop")
    if develop_deps is not None:
        yield from parse_dependency_blob(develop_deps)


def parse_gemfile(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    def parse_dep(s: str) -> FoundDependency:
        # s == "    $DEP ($VERSION)"
        dep, paren_version = s.strip().split(" ")
        version = paren_version[1:-1]
        return FoundDependency(
            package=dep,
            version=version,
            ecosystem=Ecosystem(Gem()),
            resolved_url=None,
            allowed_hashes={},
            transitivity=Transitivity(Unknown()),
        )

    lines = lockfile_text.split("\n")
    # No dependencies specified
    if "GEM" not in lines:
        return
    GEM_idx = lines.index("GEM") + 1
    GEM_end_idx = lines[GEM_idx:].index(
        ""
    )  # A line with a single \n becomes the empty string upon splitting by \n
    all_deps = lines[GEM_idx:GEM_end_idx]
    yield from (
        parse_dep(dep) for dep in all_deps if dep[:4] == " " * 4 and dep[5] != " "
    )


def parse_go_sum(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    # We currently ignore the +incompatible flag, pseudo versions, and the difference between a go.mod and a direct download
    def parse_dep(s: str) -> FoundDependency:
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
        return FoundDependency(
            package=dep,
            version=version,
            ecosystem=Ecosystem(Gomod()),
            # go.sum dep names are already URLs
            resolved_url=dep,
            allowed_hashes={"gomod": [hash]},
            transitivity=Transitivity(Unknown()),
        )

    lines = lockfile_text.split("\n")
    if len(lines[-1].split()) != 3:
        # Sometimes the last line will contain a carriage return character
        lines = lines[:-1]
    yield from (parse_dep(dep) for dep in lines)


def parse_cargo(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    def parse_dep(s: str) -> FoundDependency:
        lines = s.split("\n")[1:]
        dep = lines[0].split("=")[1].strip()[1:-1]
        version = lines[1].split("=")[1].strip()[1:-1]
        if len(lines) >= 3 and lines[3].startswith("checksum"):
            hash = {"sha256": [lines[3].split("=")[1].strip()[1:-1]]}
        else:
            hash = {}
        return FoundDependency(
            package=dep,
            version=version,
            ecosystem=Ecosystem(Cargo()),
            resolved_url=None,
            allowed_hashes=hash,
            transitivity=Transitivity(Unknown()),
        )

    deps = lockfile_text.split("[[package]]")[1:]
    yield from (parse_dep(dep) for dep in deps)


def parse_pom(
    manifest_text: str, _: Optional[str]
) -> Generator[FoundDependency, None, None]:
    NAMESPACE = "{http://maven.apache.org/POM/4.0.0}"

    def parse_dep(
        properties: Any,
        # Optional[ET.Element]
        el: Any
        # ET.Element
    ) -> Optional[FoundDependency]:
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

        return FoundDependency(
            package=dep,
            version=version,
            ecosystem=Ecosystem(Maven()),
            resolved_url=None,
            allowed_hashes={},
            transitivity=Transitivity(Unknown()),
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


def parse_gradle(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    def parse_dep(line: str) -> Optional[FoundDependency]:
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
        return FoundDependency(
            package=name,
            version=version,
            ecosystem=Ecosystem(Gradle()),
            resolved_url=None,
            allowed_hashes={},
            transitivity=Transitivity(Unknown()),
        )

    lines = lockfile_text.splitlines()[
        3:-1
    ]  # Drop the 3 comment lines at the top and the empty= line from the bottom
    deps = [parse_dep(line) for line in lines]
    yield from (dep for dep in deps if dep)


def parse_poetry(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    # poetry.lock files are not quite valid TOML >:(

    if manifest_text:
        manifest = tomli.loads(manifest_text)
        try:
            manifest_deps = manifest["tool"]["poetry"]["dependencies"]
        except KeyError:
            manifest_deps = {}
    else:
        manifest_deps = None

    def parse_dep(s: str) -> FoundDependency:
        lines = s.split("\n")[1:]
        _, dep, line_number = lines[0].split(" = ")
        dep = dep.strip(' "')
        version = lines[1].split("=")[1].strip()[1:-1]
        if manifest_deps:
            transitivity = (
                Transitivity(Direct())
                if dep in manifest_deps
                else Transitivity(Transitive())
            )
        else:
            transitivity = Transitivity(Unknown())
        return FoundDependency(
            package=dep,
            version=version,
            ecosystem=Ecosystem(Pypi()),
            resolved_url=None,
            allowed_hashes={},
            transitivity=transitivity,
            line_number=int(line_number) + 1,
        )

    lockfile_text = "\n".join(
        line + f" = {i}" if line.strip().startswith("name") else line
        for i, line in enumerate(lockfile_text.split("\n"))
    )
    deps = lockfile_text.split("[[package]]")[1:]  # drop the empty string at the start
    yield from (parse_dep(dep) for dep in deps)


LOCKFILE_PARSERS = {
    "pipfile.lock": parse_pipfile,  # Python
    "yarn.lock": parse_yarn,  # JavaScript
    "package-lock.json": parse_package_lock,  # JavaScript
    "gemfile.lock": parse_gemfile,  # Ruby
    "go.sum": parse_go_sum,  # Go
    "cargo.lock": parse_cargo,  # Rust
    "pom.xml": parse_pom,  # Java
    "gradle.lockfile": parse_gradle,  # Java
    "poetry.lock": parse_poetry,  # Python
}


@lru_cache(maxsize=1000)
def parse_lockfile_str(
    lockfile_text: str, filepath_for_reference: Path, manifest_text: Optional[str]
) -> List[FoundDependency]:
    # coupling with the github action, which decides to send files with these names back to us
    filepath = filepath_for_reference.name.lower()
    if filepath in LOCKFILE_PARSERS:
        try:
            return list(LOCKFILE_PARSERS[filepath](lockfile_text, manifest_text))
        # Such a general except clause is suspect, but the parsing error could be any number of
        # python errors, since our parsers are just using stdlib string processing functions
        # This will avoid catching dangerous to catch things like KeyboardInterrupt and SystemExit
        except Exception as e:
            logger.error(f"Failed to parse {filepath_for_reference} with exception {e}")
            return []
    else:
        raise SemgrepError(
            f"don't know how to parse this filename: {filepath_for_reference}"
        )
