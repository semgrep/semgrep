import base64
import json
from functools import lru_cache
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Generator
from typing import Iterator
from typing import List
from typing import Optional

from packaging.version import InvalidVersion
from packaging.version import Version

from semgrep.error import SemgrepError
from semgrep.verbose_logging import getLogger

# NOTE: Defused XML doesn't export types :(


logger = getLogger(__name__)

from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown

from semdep.parsers.poetry import parse_poetry
from semdep.parsers.requirements import parse_requirements
from semdep.parsers.yarn import parse_yarn
from semdep.parsers.gradle import parse_gradle
from semdep.parsers.pipfile import parse_pipfile


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


def parse_pom_tree(tree_str: str, _: Optional[str]) -> Iterator[FoundDependency]:
    def package_index(line: str) -> int:
        i = 0
        while line[i] in ["+", "-", " ", "\\"]:
            i += 1
        return i

    def depth(package_index: int) -> int:
        return package_index // 3 + 1

    lines = tree_str.split("\n")
    # Drop first line, it's the name of the project itself
    deps = [l for l in lines[1:] if l]  # Filter any empty lines
    for i, dep in enumerate(deps):
        j = package_index(dep)
        dep = dep[j:]
        transitivity = Transitivity(Direct() if depth(j) else Transitive())
        [_, package, _, version, _] = dep.strip().split(":")
        yield FoundDependency(
            package=package,
            version=version,
            ecosystem=Ecosystem(Maven()),
            resolved_url=None,
            allowed_hashes={},
            transitivity=transitivity,
            line_number=i + 1,
        )


OLD_LOCKFILE_PARSERS = {
    "package-lock.json": parse_package_lock,  # JavaScript
    "gemfile.lock": parse_gemfile,  # Ruby
    "go.sum": parse_go_sum,  # Go
    "cargo.lock": parse_cargo,  # Rust
    "maven_dep_tree.txt": parse_pom_tree,  # Java
}

NEW_LOCKFILE_PARSERS = {
    "poetry.lock": parse_poetry,  # Python
    "requirements.txt": parse_requirements,  # Python
    "yarn.lock": parse_yarn,  # JavaScript
    "gradle.lockfile": parse_gradle,  # Java
    "pipfile.lock": parse_pipfile,  # Python
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
