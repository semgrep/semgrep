#!/usr/bin/env python3
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
"""Print GitHub outputs for grammar tests affected by stdin paths.

Paths are lang/ paths relative to the ocaml-tree-sitter-semgrep root. The
comparison-base upstream-grammars.json is the only argument; HEAD's registry
and wrappers are read from this checkout.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path
from pathlib import PurePosixPath
from typing import Any

from grammar_registry import grammar_test_targets
from grammar_registry import load
from grammar_registry import load_file
from grammar_registry import RegistryError
from grammar_registry import resolve_in_registry


class SelectionError(Exception):
    """Cannot select grammar tests."""


def consumers(registry: dict[str, dict[str, Any]], keys: set[str]) -> set[str]:
    """Add transitive consumers from the dependency graph."""
    affected = keys & registry.keys()
    while (
        added := {
            key
            for key, entry in registry.items()
            if any(dependency in affected for dependency in entry.get("depends_on", []))
        }
        - affected
    ):
        affected |= added
    return affected


def lookup(name: str, registry: dict[str, dict[str, Any]]) -> set[str]:
    """Resolve a key, destination, or alias."""
    key = resolve_in_registry(name, registry)
    return {key} if key is not None else set()


def path_keys(path: str, registry: dict[str, dict[str, Any]]) -> set[str] | None:
    """Map a changed path to registry keys."""
    match PurePosixPath(path).parts:
        case ["lang", "semgrep-grammars", "src", name, *_] if name.startswith(
            "semgrep-"
        ):
            key = name.removeprefix("semgrep-")
            return {key} if key in registry else None
        case ["lang", "semgrep-grammars", "lang", name, *_]:
            return lookup(name, registry) or None
        case ["lang", name, _, *_]:
            return lookup(name, registry) or None
        case _:
            return None


def select_languages(
    changed_paths: list[str],
    base: dict[str, dict[str, Any]],
    current: dict[str, dict[str, Any]],
    test_targets: set[str],
) -> list[str]:
    """Select test-lang targets affected by changed paths."""
    keys: set[str] = set()
    for path in changed_paths:
        if path == "lang/upstream-grammars.json":
            keys |= {
                key
                for key in base.keys() | current.keys()
                if base.get(key) != current.get(key)
            }
            continue
        if not path.startswith("lang/"):
            raise SelectionError(f"not a grammar path: {path}")
        found = path_keys(path, current)
        if found is None:
            print(f"warning: cannot map {path}; testing all grammars", file=sys.stderr)
            return sorted(test_targets)
        keys |= found

    affected = consumers(current, keys)
    missing = sorted(affected - test_targets)
    if missing:
        raise SelectionError("no grammar wrappers for " + ", ".join(missing))
    return sorted(affected)


def main() -> int:
    """Select tests from stdin for GitHub Actions."""
    if len(sys.argv) != 2:
        print("usage: select_grammar_tests.py BASE_REGISTRY.json", file=sys.stderr)
        return 2
    try:
        test_targets = set(grammar_test_targets())
        if not test_targets:
            raise SelectionError("no grammar test targets found")
        languages = select_languages(
            [line.strip() for line in sys.stdin if line.strip()],
            load_file(Path(sys.argv[1])),
            load(),
            test_targets,
        )
    except (RegistryError, SelectionError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2
    print(f"languages={json.dumps(languages, separators=(',', ':'))}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
