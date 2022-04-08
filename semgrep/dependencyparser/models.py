from dataclasses import dataclass
from enum import Enum
from typing import Dict
from typing import List
from typing import Optional
from typing import Set

from semgrep.error import SemgrepError
from semgrep.semgrep_types import Language

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


def languages_to_namespaces(langs: List[Language]) -> Set[PackageManagers]:
    LANGUAGE_TO_NAMESPACE = {
        Language("python"): PackageManagers.PYPI,
        Language("js"): PackageManagers.NPM,
        Language("ts"): PackageManagers.NPM,
        Language("ruby"): PackageManagers.GEM,
        Language("go"): PackageManagers.GOMOD,
        Language("rust"): PackageManagers.CARGO,
        Language("java"): PackageManagers.MAVEN,
    }
    if Language("generic") in langs:
        return set(LANGUAGE_TO_NAMESPACE.values())

    namespaces = set()
    for lang in langs:
        if lang not in LANGUAGE_TO_NAMESPACE:
            raise SemgrepError(
                f"r2c-internal-project-depends-on does not support language {lang}"
            )
        namespaces.add(LANGUAGE_TO_NAMESPACE[lang])
    return namespaces


NAMESPACE_TO_LOCKFILES = {
    PackageManagers.PYPI: {"Pipfile.lock"},
    PackageManagers.NPM: {"package-lock.json", "yarn.lock"},
    PackageManagers.GEM: {"Gemfile.lock"},
    PackageManagers.GOMOD: {"go.sum"},
    PackageManagers.CARGO: {"cargo.lock"},
    PackageManagers.MAVEN: {"pom.xml"},
}
