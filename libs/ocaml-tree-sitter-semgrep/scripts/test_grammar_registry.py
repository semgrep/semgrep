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
"""Unit tests for scripts/grammar_registry.py."""
from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import pytest
from grammar_registry import clone_for_wrapper_name
from grammar_registry import clone_name
from grammar_registry import dests
from grammar_registry import load
from grammar_registry import RegistryError
from grammar_registry import resolve
from grammar_registry import UnknownGrammarError
from grammar_registry import validate
from grammar_registry import wrapper_dir
from ts_versions import TsVersionError
from ts_versions import version_for_lang

SCRIPTS_DIR = Path(__file__).resolve().parent
LANG_DIR = SCRIPTS_DIR.parent / "lang"


def test_resolve_dest_and_key():
    assert resolve("apex", LANG_DIR) == "sfapex"
    assert resolve("tsx", LANG_DIR) == "typescript"
    assert resolve("sfapex", LANG_DIR) == "sfapex"
    assert resolve("python", LANG_DIR) == "python"


def test_clone_name_gomod():
    assert clone_name("gomod", LANG_DIR) == "go-mod"


def test_wrapper_dir():
    assert wrapper_dir("sfapex", LANG_DIR) == "semgrep-sfapex"


def test_clone_for_wrapper():
    assert clone_for_wrapper_name("gomod", LANG_DIR) == "go-mod"
    assert clone_for_wrapper_name("sfapex", LANG_DIR) == "sfapex"


def test_regen_dest_uniqueness():
    reg = load(LANG_DIR)
    seen: set[str] = set()
    for _key, entry in reg.items():
        for d in entry["regen"]:
            assert d not in seen, f"duplicate dest {d}"
            seen.add(d)


def test_depends_on_dag():
    reg = load(LANG_DIR)
    assert reg["cpp"]["depends_on"] == ["c"]
    assert reg["typescript"]["depends_on"] == ["javascript"]
    validate(reg)


def test_clone_only_empty_regen():
    for key in ("javascript", "c", "haskell", "sqlite", "make", "cfml"):
        assert dests(key, LANG_DIR) == []


def test_version_aliases():
    assert version_for_lang("soql", LANG_DIR) == "0.20.8"
    assert version_for_lang("php-only", LANG_DIR) == "0.26.3"
    assert version_for_lang("apex", LANG_DIR) == "0.20.8"


def test_resolve_unknown():
    with pytest.raises(UnknownGrammarError):
        resolve("not-a-language", LANG_DIR)


def test_version_unknown():
    with pytest.raises(TsVersionError):
        version_for_lang("not-a-language", LANG_DIR)


def _git(cwd, *args: str) -> str:
    return subprocess.run(
        ["git", "-C", str(cwd), *args],
        check=True,
        capture_output=True,
        text=True,
    ).stdout.strip()


def test_fetch_pin_not_on_default_branch(tmp_path):
    """Clone of origin/main must still check out a pin that lives only on a side branch."""
    import json

    import grammar_registry as gr

    upstream = tmp_path / "upstream"
    upstream.mkdir()
    _git(tmp_path, "init", "-b", "main", "upstream")
    _git(upstream, "config", "user.email", "test@example.com")
    _git(upstream, "config", "user.name", "test")
    (upstream / "f").write_text("a\n")
    _git(upstream, "add", "f")
    _git(upstream, "commit", "-m", "a")
    _git(upstream, "checkout", "-b", "side")
    (upstream / "f").write_text("b\n")
    _git(upstream, "commit", "-am", "b")
    pin = _git(upstream, "rev-parse", "HEAD")
    _git(upstream, "checkout", "main")

    lang_dir = tmp_path / "lang"
    lang_dir.mkdir()
    (lang_dir / "upstream-grammars.json").write_text(
        json.dumps(
            {
                "toy": {
                    "commit": pin,
                    "regen": [],
                    "tree_sitter": "0.22.6",
                    "url": str(upstream),
                }
            }
        )
        + "\n"
    )
    gr.clear_load_cache()
    ots_root = tmp_path / "ots"
    dest = gr.fetch(ots_root, "toy", lang_dir)
    assert _git(dest, "rev-parse", "HEAD") == pin
    assert (dest / "f").read_text() == "b\n"

    _git(dest, "remote", "set-url", "origin", str(tmp_path / "unavailable"))
    _git(dest, "checkout", "main")
    assert gr.fetch(ots_root, "toy", lang_dir) == dest
    assert _git(dest, "rev-parse", "HEAD") == pin


@pytest.mark.parametrize(
    "name,key,clone",
    [
        ("php-only", "php", "php"),
        ("soql", "sfapex", "sfapex"),
        ("sosl", "sfapex", "sfapex"),
        ("apex", "sfapex", "sfapex"),
        ("tsx", "typescript", "typescript"),
        ("cfquery", "cfml", "cfml"),
        ("cfscript", "cfml", "cfml"),
        ("go-mod", "gomod", "go-mod"),
        ("python", "python", "python"),
    ],
)
def test_alias_resolution_and_cli(name, key, clone):
    assert resolve(name, LANG_DIR) == key
    assert version_for_lang(name, LANG_DIR) == version_for_lang(key, LANG_DIR)
    for command, expected in [
        ("wrapper-for-lang", f"semgrep-{key}"),
        ("clone-for-wrapper", clone),
    ]:
        proc = subprocess.run(
            [sys.executable, SCRIPTS_DIR / "grammar_registry.py", command, name],
            capture_output=True,
            text=True,
        )
        assert proc.returncode == 0, proc.stderr
        assert proc.stdout.strip() == expected


@pytest.mark.parametrize("name", ["python", "php-only"])
@pytest.mark.parametrize("invalid", ["duplicate", "cycle", "schema"])
def test_version_preserves_registry_error(tmp_path, name, invalid):
    reg = load(LANG_DIR)
    if invalid == "duplicate":
        reg["python"]["regen"].append("tsx")
        message = "duplicate regen dest tsx"
    elif invalid == "cycle":
        reg["python"]["depends_on"] = ["php"]
        reg["php"]["depends_on"] = ["python"]
        message = "depends_on cycle"
    else:
        del reg["python"]["tree_sitter"]
        message = "python: missing tree_sitter"
    (tmp_path / "upstream-grammars.json").write_text(json.dumps(reg))
    with pytest.raises(RegistryError, match=message):
        version_for_lang(name, tmp_path)
