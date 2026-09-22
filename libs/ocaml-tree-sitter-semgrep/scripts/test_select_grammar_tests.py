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
"""Unit tests for scripts/select_grammar_tests.py."""
from __future__ import annotations

import subprocess
from pathlib import Path
from typing import Any

import pytest
import select_grammar_tests as sel
from grammar_registry import default_lang_dir

SCRIPT = Path(__file__).resolve().parent / "select_grammar_tests.py"


def entry(*, depends_on: list[str] | None = None, regen: list[str] | None = None):
    value: dict[str, Any] = {
        "url": "https://example.com/grammar.git",
        "commit": "a" * 40,
        "tree_sitter": "0.22.6",
        "regen": regen or [],
    }
    if depends_on is not None:
        value["depends_on"] = depends_on
    return value


def select(
    paths: list[str],
    base: dict[str, dict[str, Any]],
    current: dict[str, dict[str, Any]] | None = None,
    test_targets: set[str] | None = None,
) -> list[str]:
    current = base if current is None else current
    if test_targets is None:
        test_targets = set(base) | set(current)
    return sel.select_languages(paths, base, current, test_targets)


def test_selects_changed_language_and_not_prefix_match():
    """Select the exact language directory."""
    registry = {
        "java": entry(regen=["java"]),
        "javascript": entry(regen=["javascript"]),
    }
    assert select(["lang/java/test/ok/Test.java"], registry) == ["java"]


def test_selects_transitive_consumers_but_not_dependencies():
    """Select transitive consumers, not dependencies."""
    registry = {
        "base": entry(regen=["base"]),
        "middle": entry(depends_on=["base"], regen=["middle"]),
        "leaf": entry(depends_on=["middle"], regen=["leaf"]),
    }
    assert select(["lang/base/test/example"], registry) == ["base", "leaf", "middle"]
    assert select(["lang/leaf/test/example"], registry) == ["leaf"]


def test_resolves_alias():
    """Resolve dialect aliases from the current registry."""
    typescript = {
        "javascript": entry(regen=["javascript"]),
        "typescript": entry(depends_on=["javascript"], regen=["typescript", "tsx"]),
    }
    assert select(["lang/tsx/test/example.tsx"], typescript) == ["typescript"]


def test_registry_pin_change_selects_consumers():
    """A pin change selects the grammar and its current consumers."""
    old = {
        "c": entry(regen=["c"]),
        "cpp": entry(depends_on=["c"], regen=["cpp"]),
    }
    new = {
        "c": {**old["c"], "commit": "b" * 40},
        "cpp": old["cpp"],
    }
    assert select(["lang/upstream-grammars.json"], old, new) == ["c", "cpp"]


def test_formatting_only_registry_change_selects_nothing():
    """Ignore registry edits with unchanged data."""
    registry = {"python": entry(regen=["python"])}
    assert select(["lang/upstream-grammars.json"], registry) == []


def test_deleted_key_is_omitted():
    """Omit deleted registry keys."""
    old = {"removed": entry(regen=["removed"])}
    assert select(["lang/upstream-grammars.json"], old, {}, {"other"}) == []


def test_unmapped_lang_path_selects_all(capsys):
    """Select every grammar when a lang/ path does not name one."""
    registry = {"python": entry(regen=["python"]), "ruby": entry(regen=["ruby"])}
    assert select(["lang/unknown-file"], registry) == ["python", "ruby"]
    assert "testing all grammars" in capsys.readouterr().err


def test_non_grammar_path_is_rejected():
    """Refuse paths outside lang/."""
    registry = {"python": entry(regen=["python"])}
    with pytest.raises(sel.SelectionError, match="not a grammar path"):
        select(["Makefile"], registry)
    with pytest.raises(sel.SelectionError, match="not a grammar path"):
        select(["lang/python/test/ok/a.py", "scripts/grammar_registry.py"], registry)


def test_no_changed_paths_selects_nothing():
    """Select nothing for an empty diff."""
    registry = {"python": entry(regen=["python"])}
    assert select([], registry) == []


def test_missing_wrapper_is_an_error():
    """Fail when an affected grammar has no test target."""
    registry = {
        "python": entry(regen=["python"]),
        "ruby": entry(regen=["ruby"]),
    }
    with pytest.raises(sel.SelectionError, match="python"):
        select(["lang/python/test/example.py"], registry, test_targets={"ruby"})


def test_cli_reads_base_registry_from_argv(tmp_path):
    """Read the old registry and emit workflow output."""
    registry = default_lang_dir() / "upstream-grammars.json"
    base_registry = tmp_path / "base.json"
    base_registry.write_text(registry.read_text())
    missing = subprocess.run(
        [SCRIPT, str(tmp_path / "missing.json")],
        input="lang/python/test/example.py\n",
        capture_output=True,
        text=True,
    )
    assert missing.returncode != 0

    proc = subprocess.run(
        [SCRIPT, base_registry],
        input="lang/python/test/ok/a.py\n",
        capture_output=True,
        text=True,
    )
    assert proc.returncode == 0, proc.stderr
    assert proc.stdout == 'languages=["python"]\n'
