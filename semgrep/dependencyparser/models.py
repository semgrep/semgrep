import hashlib
import io
from dataclasses import dataclass
from enum import Enum
from typing import Any
from typing import Callable
from typing import Dict
from typing import Generator
from typing import List
from typing import Optional
from typing import Tuple

from semgrep.error import SemgrepError

MAX_REFERENCES = 3

KNOWN_HASH_ALGORITHMS: Dict[str, Callable] = {
    "sha256": hashlib.sha256,
    "sha512": hashlib.sha512,
    "sha1": hashlib.sha1,
}


class PackageManagers(Enum):
    NPM = "npm"
    PYPI = "pypy"


@dataclass(frozen=True, eq=True, order=True)
class LockfileDependency:
    name: str
    version: str
    namespace: PackageManagers
    # map from hash algorithm to list of hashes that are ok
    allowed_hashes: Dict[str, List[str]]
    # sometimes the lockfile gives us a resolved URL, sweet
    resolved_url: Optional[List[str]] = None

    def get_single_hash(self) -> Tuple[str, str]:
        assert (
            len(self.allowed_hashes) == 1
        ), f"when fetching a package, expected only one hash sought {self.allowed_hashes}"
        allowed_alg = list(self.allowed_hashes.keys())[0]
        assert (
            len(self.allowed_hashes[allowed_alg]) == 1
        ), f"when fetching a package, expected only one hash sought {self.allowed_hashes[allowed_alg]}"
        allowed_hash = list(self.allowed_hashes[allowed_alg])[0]
        return allowed_alg, allowed_hash

    def __post_init__(self) -> None:
        for k in self.allowed_hashes.keys():
            assert (
                k in KNOWN_HASH_ALGORITHMS
            ), f"unknown hash type {k} not in {KNOWN_HASH_ALGORITHMS}"
        for hash_list in self.allowed_hashes.values():
            for h in hash_list:
                assert h.lower() == h, "uhoh, hashes should always be lowercased..."

    def package_url(self) -> Optional[str]:
        if self.namespace == PackageManagers.PYPI:
            return f"https://pypi.org/project/{self.name}/{self.version}"
        elif self.namespace == PackageManagers.NPM:
            return f"https://www.npmjs.com/package/{self.name}"
        else:
            raise SemgrepError(f"unknown namespace for package: {self.namespace}")

    @classmethod
    def hash_file_all_algorithms(
        cls, file_bytes: bytes
    ) -> Generator[Tuple[str, str], None, None]:
        for alg in KNOWN_HASH_ALGORITHMS.keys():
            yield alg, LockfileDependency.hash_file_bytes(alg, file_bytes)

    @classmethod
    def hash_file_bytes(cls, algorithm: str, file_bytes: bytes) -> str:
        hash_func = KNOWN_HASH_ALGORITHMS[algorithm]()
        BLOCKSIZE = 64 * 1024
        fp = io.BytesIO(file_bytes)
        while True:
            data = fp.read(BLOCKSIZE)
            if not data:
                break
            hash_func.update(data)
        hexdigest: str = hash_func.hexdigest()
        return hexdigest

    @classmethod
    def hash_file(cls, algorithm: str, filepath: str) -> str:
        hash_func = KNOWN_HASH_ALGORITHMS[algorithm]()
        BLOCKSIZE = 64 * 1024
        with open(filepath, "rb") as fp:
            while True:
                data = fp.read(BLOCKSIZE)
                if not data:
                    break
                hash_func.update(data)
        hexdigest: str = hash_func.hexdigest()
        return hexdigest

    def to_json(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "version": self.version,
            "namespace": self.namespace.value,
            "resolved_url": self.resolved_url if self.resolved_url else None,
            "allowed_hashes": self.allowed_hashes if self.allowed_hashes else None,
        }
