#
# Copyright (c) 2024-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
from pathlib import Path
from typing import Generator
from typing import List
from typing import Optional

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.semgrep_interfaces.semgrep_output_v1 import FoundDependency
from semgrep.semgrep_interfaces.semgrep_output_v1 import Fpath


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
            ecosystem=out.Ecosystem(out.Cargo()),
            resolved_url=None,
            allowed_hashes=hash,
            transitivity=out.DependencyKind(out.Unknown()),
            manifest_path=None,
            lockfile_path=Fpath(str(lockfile_path)),
            line_number=raw_lines.index(lines[0]) + 1,
        )

    raw_lines = lockfile_text.split("\n")
    deps = lockfile_text.split("[[package]]")[1:]
    yield from (parse_dep(dep, raw_lines) for dep in deps)
