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
"""Select ``tree-sitter generate`` ABI flags for a grammar directory.

Rules:
- ``--abi=15`` only when the grammar dir has a ``tree-sitter.json`` and
  ``SEMGREP_ENABLE_ABI15`` is truthy. ABI 15 requires tree-sitter >= 0.25.0.
  Off by default: we are not yet ready to ship ABI 15 parsers.
- ``--abi=14`` for tree-sitter >= 0.24.0 (the release that introduced ``--abi``).
- ``--no-bindings`` for older tree-sitter.
"""
from __future__ import annotations

import os
import sys
from collections.abc import Mapping
from pathlib import Path
from typing import TextIO

from ts_versions import version_at_least

ABI15_TRUTHY = frozenset({"1", "true", "TRUE", "yes", "YES", "on", "ON"})


class GenerateAbiError(RuntimeError):
    """ABI 15 was requested but the pinned tree-sitter version is too old."""


def abi15_enabled(env: Mapping[str, str] | None = None) -> bool:
    """Return True when ``SEMGREP_ENABLE_ABI15`` is set to a truthy value."""
    source = os.environ if env is None else env
    return source.get("SEMGREP_ENABLE_ABI15", "") in ABI15_TRUTHY


def generate_abi_args(
    grammar_dir: Path | str,
    ts_version: str,
    *,
    env: Mapping[str, str] | None = None,
    warn: TextIO | None = None,
) -> str:
    """Return the ``tree-sitter generate`` flag for a grammar and tree-sitter version."""
    directory = Path(grammar_dir)
    has_json = (directory / "tree-sitter.json").is_file()
    if has_json and abi15_enabled(env):
        if not version_at_least(ts_version, "0.25.0"):
            raise GenerateAbiError(
                f"Error: {directory} uses ABI 15 (tree-sitter.json present, "
                f"SEMGREP_ENABLE_ABI15 set) and requires tree-sitter >= 0.25.0, "
                f"but this grammar is pinned to {ts_version}."
            )
        return "--abi=15"
    fallback = "--abi=14" if version_at_least(ts_version, "0.24.0") else "--no-bindings"
    if abi15_enabled(env) and not has_json:
        # ABI 15 was requested but cannot apply without a tree-sitter.json,
        # so we silently produce an older ABI. Warn rather than surprise the
        # user, who is expecting --abi=15. (stderr keeps stdout flag-only.)
        print(
            f"Warning: SEMGREP_ENABLE_ABI15 is set but {directory} has no "
            f"tree-sitter.json, which ABI 15 requires; generating with "
            f"{fallback} instead.",
            file=sys.stderr if warn is None else warn,
        )
    return fallback
