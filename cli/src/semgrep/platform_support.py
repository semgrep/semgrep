#
# Copyright (c) 2026 Semgrep Inc.
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
# Guidance for platforms we no longer ship binaries for.
#
# Only stdlib imports: console_scripts/entrypoint.py imports this before the
# rest of the semgrep package is loaded.
import platform
import sys


def is_intel_macos() -> bool:
    # Also true for x86_64 Python running under Rosetta on Apple Silicon.
    return sys.platform == "darwin" and not platform.machine().startswith(
        ("arm", "aarch")
    )


def intel_macos_guidance(build_from_source: bool) -> str:
    lines = [
        "Semgrep does not provide Intel macOS binaries as of version 1.183.0. Alternatives:",
        "  - Run the semgrep/semgrep Docker image (linux/amd64).",
        "  - On Apple Silicon, use a native arm64 Python.",
    ]
    if build_from_source:
        lines.append("  - Build semgrep-core from source and put it in your PATH.")
    return "\n".join(lines)
