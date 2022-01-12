import hashlib
from enum import Enum
from typing import Callable
from typing import Dict
from typing import List
from typing import Optional

import attr

KNOWN_HASH_ALGORITHMS: Dict[str, Callable] = {
    "sha256": hashlib.sha256,
    "sha512": hashlib.sha512,
    "sha1": hashlib.sha1,
}


class PackageManagers(Enum):
    NPM = "npm"
    PYPI = "pypy"


@attr.s(auto_attribs=True, frozen=True, eq=True, order=True)
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
        for hash_list in self.allowed_hashes.values():
            for h in hash_list:
                assert h.lower() == h, "uhoh, hashes should always be lowercased..."
