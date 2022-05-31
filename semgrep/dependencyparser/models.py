from dataclasses import dataclass
from enum import Enum
from typing import Dict
from typing import List
from typing import Optional
from typing import Set


KNOWN_HASH_ALGORITHMS: Set[str] = {
    "sha256",
    "sha512",
    "sha1",
    "gomod",
    # go.sum files use a non-standard hashing algorithm based on multiple uses of sha256 and conversion to base 64
}


class PackageManagers(str, Enum):
    NPM = "npm"
    PYPI = "pypi"
    GEM = "gem"
    GOMOD = "gomod"
    CARGO = "cargo"
    MAVEN = "maven"
    GRADLE = "gradle"


@dataclass(eq=True, order=True, frozen=True)
class LockfileDependency:
    name: str
    version: str
    namespace: PackageManagers
    # map from hash algorithm to list of hashes that are ok
    allowed_hashes: Dict[str, List[str]]
    # sometimes the lockfile gives us a resolved URL, sweet
    resolved_url: Optional[List[str]] = None

    def __post_init__(self) -> None:
        for k in self.allowed_hashes.keys():
            assert (
                k in KNOWN_HASH_ALGORITHMS
            ), f"unknown hash type {k} not in {KNOWN_HASH_ALGORITHMS}"


NAMESPACE_TO_LOCKFILES = {
    PackageManagers.PYPI: ["Pipfile.lock", "poetry.lock"],
    PackageManagers.NPM: ["package-lock.json", "yarn.lock"],
    PackageManagers.GEM: ["Gemfile.lock"],
    PackageManagers.GOMOD: ["go.sum"],
    PackageManagers.CARGO: ["Cargo.lock"],
    PackageManagers.MAVEN: ["pom.xml"],
    PackageManagers.GRADLE: ["gradle.lockfile"],
}
