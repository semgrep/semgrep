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
"""Tree-sitter version resolution from upstream-grammars.json."""
from __future__ import annotations

import re
from pathlib import Path

from grammar_registry import load
from grammar_registry import resolve
from grammar_registry import UnknownGrammarError

JSON_NAME_RE = re.compile(r'"name"\s*:\s*"([^"]+)"')


class TsVersionError(LookupError):
    """A language or grammar directory has no unique tree-sitter version pin."""


def default_lang_dir() -> Path:
    return Path(__file__).resolve().parent.parent / "lang"


def version_sort_key(version: str) -> tuple[int, ...]:
    return tuple(int(part) for part in version.split("."))


def version_at_least(version: str, minimum: str) -> bool:
    return version_sort_key(version) >= version_sort_key(minimum)


def list_pinned_versions(lang_dir: Path | None = None) -> list[str]:
    reg = load(lang_dir)
    versions = {entry["tree_sitter"] for entry in reg.values()}
    return sorted(versions, key=version_sort_key)


def _tree_sitter_for_key(key: str, lang_dir: Path | None) -> str:
    return str(load(lang_dir)[key]["tree_sitter"])


def version_for_lang(name: str, lang_dir: Path | None = None) -> str:
    root = lang_dir or default_lang_dir()
    try:
        key = resolve(name, root)
        return _tree_sitter_for_key(key, root)
    except UnknownGrammarError as exc:
        raise TsVersionError(
            f"Error: '{name}' is not in upstream-grammars.json.\n"
            "Add a registry entry or alias for the dialect name."
        ) from exc


def extract_grammar_name(grammar_dir: Path) -> str:
    resolved = grammar_dir.resolve()
    for rel in ("tree-sitter.json", "src/grammar.json"):
        json_file = resolved / rel
        if not json_file.is_file():
            continue
        match = JSON_NAME_RE.search(json_file.read_text())
        if match:
            name = match.group(1)
            break
    else:
        name = resolved.name
        for prefix in ("tree-sitter-", "semgrep-"):
            if name.startswith(prefix):
                name = name[len(prefix) :]
    return name.replace("_", "-")


def version_for_grammar_dir(
    grammar_dir: Path | str, lang_dir: Path | None = None
) -> str:
    return version_for_lang(extract_grammar_name(Path(grammar_dir)), lang_dir)
