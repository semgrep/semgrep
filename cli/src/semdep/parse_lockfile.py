import collections
import json
from functools import lru_cache
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Generator
from typing import Iterator
from typing import List
from typing import Optional
from typing import Tuple

import tomli
from packaging.version import InvalidVersion
from packaging.version import Version

from semgrep.error import SemgrepError
from semgrep.verbose_logging import getLogger

# NOTE: Defused XML doesn't export types :(


logger = getLogger(__name__)

from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown

from semdep.parsers.util import extract_npm_lockfile_hash
from semdep.parsers.requirements import parse_requirements
from semdep.parsers.pom_tree import parse_pom_tree

from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown


def parse_yarn1(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    def get_version(lines: List[str]) -> Tuple[str, str]:
        """
        Given list representation of a single package's yarn lock info object
        (with injected line number next to version), return the version and line number

        Returns the version without the surrounding quotation marks

        Assumes only a single element in line starts with "version" after removing whitespace
        """
        for line in lines:
            if line.strip().startswith("version"):
                _, version, line_number = line.split()
                version = version.strip('" ')
                return version, line_number
        raise SemgrepError(f"yarn.lock dependency missing version for {lines[0]}")

    def get_resolved(lines: List[str]) -> Optional[str]:
        """
        Given list representation of a single package's yarn lock info object
        (with injected line number next to version), return the resolved url if it exists

        Return None if no resolved url exists. Removes trailing octothorpe and whitespace
        """
        for line in lines:
            if line.strip().startswith("resolved"):
                _, resolved = line.split()
                resolved = resolved.strip('" ')
                resolved = remove_trailing_octothorpe(resolved)
                return resolved
        return None

    def get_integrity(lines: List[str]) -> Optional[str]:
        """
        Given list representation of a single package's yarn lock info object
        return integrity hash if exits
        """
        for line in lines:
            if line.strip().startswith("integrity"):
                _, integrity = line.split(" ", 1)
                return integrity.strip('"')
        return None

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
        version, line_number = get_version(lines)
        resolved = get_resolved(lines)
        integrity = get_integrity(lines)
        if not manifest_deps:
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


def parse_yarn2(
    lockfile_text: str, manifest_text: Optional[str]
) -> Iterator[FoundDependency]:

    lockfile_text = "\n".join(
        line + f" {i}" if line.strip().startswith("version") else line
        for i, line in enumerate(lockfile_text.split("\n"))
    )

    if manifest_text:
        manifest = json.loads(manifest_text)
        manifest_deps = manifest["dependencies"] if "dependencies" in manifest else {}
    else:
        manifest_deps = None

    def version_sources(line: str) -> Tuple[str, List[Tuple[str, str]]]:
        constraint_strs = line[:-1].strip('"').split(", ")
        constraints = []
        for c in constraint_strs:
            if c[0] == "@":
                name, constraint = c[1:].split("@", 1)
                name = "@" + name
            else:
                name, constraint = c.split("@", 1)

            if ":" in constraint:
                constraint = constraint.split(":")[1]  # npm:^1.0.0 --> ^1.0.0
            constraints.append((name, constraint))

        return constraints[0][0], constraints

    def parse_dep(dep: str) -> FoundDependency:
        lines = dep.split("\n")
        lines = [l.strip(" ") for l in lines if len(l) - len(l.lstrip(" ")) < 4]
        package, constraints = version_sources(lines[0])
        field_lines = [
            l.split(":")
            for l in lines[1:]
            if not (l.startswith("dependencies") or l.startswith("resolution"))
            if l
        ]
        fields = {f: v.strip(" ") for f, v in field_lines}
        if "version" not in fields:
            raise SemgrepError("yarn.lock dependency {package} missing version?")
        version, line_number = fields["version"].split(" ")

        if not manifest_deps:
            transitivity = Transitivity(Unknown())
        else:
            if package in manifest_deps:
                transitivity = Transitivity(
                    Direct()
                    if (package, manifest_deps[package]) in constraints
                    else Transitive()
                )
            else:
                transitivity = Transitivity(Transitive())

        return FoundDependency(
            package=package,
            version=version,
            ecosystem=Ecosystem(Npm()),
            allowed_hashes={"sha512": [fields["checksum"]]}
            if "checksum" in fields
            else {},
            resolved_url=None,
            transitivity=transitivity,
            line_number=int(line_number),
        )

    deps = lockfile_text.split("\n\n")[2:]
    for dep in deps:
        if "@patch:" in dep:
            continue
        yield parse_dep(dep)


def parse_yarn(
    lockfile_text: str, manifest_text: Optional[str]
) -> Iterator[FoundDependency]:
    if lockfile_text.startswith("# This file is"):
        yield from parse_yarn2(lockfile_text, manifest_text)
    else:
        yield from parse_yarn1(lockfile_text, manifest_text)


def parse_package_lock(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    lockfile_text = "\n".join(
        (
            line + f'"line_number": {i},'
            if line.strip().endswith(",")
            else f'"line_number": {i},' + line
        )
        if line.strip().startswith('"version"')
        else line
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

    def parse_deps(deps: Dict[str, Any], nested: bool) -> Iterator[FoundDependency]:
        # Dependency dicts in a package-lock.json can be nested:
        # {"foo" : {stuff, "dependencies": {"bar": stuff, "dependencies": {"baz": stuff}}}}
        # So we need to handle them recursively
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
            if nested:
                # Nested dependencies are always transitive
                transitivity = Transitivity(Transitive())
            elif manifest_deps:
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
                allowed_hashes=extract_npm_lockfile_hash(integrity)
                if integrity
                else {},
                resolved_url=resolved_url,
                transitivity=transitivity,
                line_number=int(line_number) + 1 if line_number else None,
            )
            nested_deps = dep_blob.get("dependencies")
            if nested_deps:
                yield from parse_deps(nested_deps, True)

    yield from parse_deps(deps, False)


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


def parse_gradle(
    lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    def parse_dep(line: str) -> Optional[FoundDependency]:
        dep = line.split(":")
        if len(dep) != 3:
            return None
        _, name, version = dep
        version, _ = version.split("=")
        try:
            Version(version)
        except InvalidVersion:
            logger.info(f"No valid version found for {name}")
            return None
        return FoundDependency(
            package=name,
            version=version,
            ecosystem=Ecosystem(Maven()),
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


OLD_LOCKFILE_PARSERS = {
    "go.sum": parse_go_sum,  # Go
    "cargo.lock": parse_cargo,  # Rust
    "yarn.lock": parse_yarn,  # JavaScript
    "gradle.lockfile": parse_gradle,  # Java
    "pipfile.lock": parse_pipfile,  # Python
    "package-lock.json": parse_package_lock,  # JavaScript
    "gemfile.lock": parse_gemfile,  # Ruby
    "poetry.lock": parse_poetry,  # Python
}

NEW_LOCKFILE_PARSERS = {
    "requirements.txt": parse_requirements,  # Python
    "maven_dep_tree.txt": parse_pom_tree,  # Java
}


@lru_cache(maxsize=1000)
def parse_lockfile_path(
    lockfile_path: Path, manifest_path: Optional[Path]
) -> List[FoundDependency]:
    # coupling with the github action, which decides to send files with these names back to us
    lockfile_name = lockfile_path.name.lower()
    if lockfile_name in NEW_LOCKFILE_PARSERS:
        parse_lockfile = NEW_LOCKFILE_PARSERS[lockfile_name]
        return parse_lockfile(lockfile_path, manifest_path)

    if lockfile_name in OLD_LOCKFILE_PARSERS:
        lockfile_text = lockfile_path.read_text()
        if manifest_path:
            manifest_text = manifest_path.read_text()
        else:
            manifest_text = None

        try:
            return list(
                OLD_LOCKFILE_PARSERS[lockfile_name](lockfile_text, manifest_text)
            )
        # Such a general except clause is suspect, but the parsing error could be any number of
        # python errors, since our parsers are just using stdlib string processing functions
        # This will avoid catching dangerous to catch things like KeyboardInterrupt and SystemExit
        except Exception as e:
            logger.error(f"Failed to parse {lockfile_path} with exception {e}")
            return []
    else:
        raise SemgrepError(f"don't know how to parse this filename: {lockfile_path}")
