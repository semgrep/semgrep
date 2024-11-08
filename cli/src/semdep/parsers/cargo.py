from pathlib import Path
from typing import Generator
from typing import List
from typing import Optional

from semgrep.semgrep_interfaces.semgrep_output_v1 import Cargo
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown


def parse_cargo(
    lockfile_path: Path, lockfile_text: str, manifest_text: Optional[str]
) -> Generator[FoundDependency, None, None]:
    def parse_dep(s: str, raw_lines: List[str]) -> FoundDependency:
        lines = s.split("\n")[1:]
        dep = lines[0].split("=")[1].strip()[1:-1]
        version = lines[1].split("=")[1].strip()[1:-1]
        if len(lines) >= 4 and lines[3].startswith("checksum"):
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
            manifest_path=None,
            lockfile_path=Fpath(str(lockfile_path)),
            line_number=raw_lines.index(lines[0]) + 1,
        )

    raw_lines = lockfile_text.split("\n")
    deps = lockfile_text.split("[[package]]")[1:]
    yield from (parse_dep(dep, raw_lines) for dep in deps)
