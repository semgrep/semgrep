import base64
import collections
import json
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Generator
from typing import List

from semgrep.error import SemgrepError
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

from dependencyparser.models import LockfileDependency
from dependencyparser.models import PackageManagers


def parse_lockfile_str(
    lockfile_text: str, filepath_for_reference: Path
) -> Generator[LockfileDependency, None, None]:
    # coupling with the github action, which decides to send files with these names back to us
    if filepath_for_reference.name.lower() == "pipfile.lock":
        yield from parse_Pipfile_str(lockfile_text)
    elif filepath_for_reference.name.lower() == "yarn.lock":
        yield from parse_Yarnlock_str(lockfile_text)
    elif filepath_for_reference.name.lower() == "package-lock.json":
        yield from parse_NPM_package_lock_str(lockfile_text)
    else:
        raise SemgrepError(
            f"don't know how to parse this filename: {filepath_for_reference}"
        )


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
        # print(line)
        if '"' not in line:
            return line.split("@")[0]
        else:
            first_quoted = "".join(line.split('"')[1:2])
            parsed_name = "@".join(first_quoted.split("@")[:-1])
            print(line, first_quoted, parsed_name)
            return parsed_name

    def remove_trailing_octothorpe(s: str) -> str:
        return "#".join(s.split("#")[:-1])

    package_name, version, resolved, integrity = None, None, None, None
    for line in lockfile_text.split("\n") + [""]:
        line = line.strip()
        # print(line)
        # print('>>>', package_name, version, resolved)
        if line.startswith("#"):
            continue
        if package_name is None and len(line) != 0:
            package_name = extract_yarn_name(line)
            continue

        if line.startswith("version") and not version:
            version = line.split("version")[1].replace('"', "").strip()
            continue

        elif line.startswith("resolved") and not resolved:
            resolved = remove_trailing_octothorpe(
                line.split("resolved")[1].replace('"', "").strip()
            )
            continue

        elif line.startswith("integrity") and not integrity:
            integrity = line.split("integrity")[1].replace('"', "").strip()
            continue

        if len(line) == 0 and package_name and version and resolved and integrity:
            yield LockfileDependency(
                package_name,
                version,
                PackageManagers.NPM,
                allowed_hashes=extract_npm_lockfile_hash(integrity),
                resolved_url=[resolved],
            )
            package_name, version, resolved, integrity = None, None, None, None


def parse_NPM_package_lock_str(
    lockfile_text: str,
) -> Generator[LockfileDependency, None, None]:
    as_json = json.loads(lockfile_text)
    for dep in as_json["dependencies"]:
        dep_blob = as_json["dependencies"][dep]
        version = dep_blob.get("version", None)
        if not version:
            logger.info(f"no version for dependency: {dep}")
        else:
            resolved_url = dep_blob.get("resolved")
            integrity = dep_blob["integrity"]
            yield LockfileDependency(
                dep,
                version,
                PackageManagers.NPM,
                allowed_hashes=extract_npm_lockfile_hash(integrity),
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
                    allowed_hashes=extract_pipfile_hashes(dep_blob["hashes"]),
                )

    as_json = json.loads(lockfile_text)
    yield from parse_dependency_blob(as_json["default"])
    develop_deps = as_json.get("develop")
    if develop_deps is not None:
        yield from parse_dependency_blob(develop_deps)
