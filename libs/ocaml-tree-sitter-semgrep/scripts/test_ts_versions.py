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
"""Unit tests for scripts/ts_versions.py and its CLI entry points."""
from __future__ import annotations

import json
import shutil
import subprocess
from pathlib import Path

import pytest
from grammar_registry import key_for_package_name
from ts_versions import extract_grammar_name
from ts_versions import list_pinned_versions
from ts_versions import TsVersionError
from ts_versions import version_for_grammar_dir
from ts_versions import version_for_lang
from ts_versions import version_sort_key

SCRIPTS_DIR = Path(__file__).resolve().parent
LANG_DIR = SCRIPTS_DIR.parent / "lang"
SCRIPTS = SCRIPTS_DIR


def test_list_pinned_versions_matches_repo():
    versions = list_pinned_versions(LANG_DIR)
    assert versions
    assert versions == sorted(versions, key=version_sort_key)
    assert "0.22.6" in versions
    assert "0.26.3" in versions


def test_version_for_lang_known_language():
    assert version_for_lang("php", LANG_DIR) == "0.26.3"
    assert version_for_lang("scala", LANG_DIR) == "0.22.6"


def test_version_for_lang_dialect_alias():
    assert version_for_lang("php-only", LANG_DIR) == "0.26.3"
    assert version_for_lang("soql", LANG_DIR) == "0.26.3"


def test_version_for_lang_missing():
    with pytest.raises(TsVersionError, match="not in upstream-grammars"):
        version_for_lang("missing-language-xyz", LANG_DIR)


def test_extract_grammar_name_from_tree_sitter_json(tmp_path):
    grammar_dir = tmp_path / "php_only"
    grammar_dir.mkdir()
    (grammar_dir / "tree-sitter.json").write_text(
        '{"grammars": [{"name": "php_only", "scope": "source.php", "path": "."}]}'
    )
    assert extract_grammar_name(grammar_dir) == "php-only"


def test_extract_grammar_name_from_directory_basename(tmp_path):
    grammar_dir = tmp_path / "semgrep-scala"
    grammar_dir.mkdir()
    assert extract_grammar_name(grammar_dir) == "scala"


def test_version_for_grammar_dir_delegates_to_registry():
    grammar_dir = LANG_DIR / "semgrep-grammars/src/semgrep-php/php"
    assert version_for_grammar_dir(grammar_dir, lang_dir=LANG_DIR) == "0.26.3"


def test_version_for_grammar_dir_inherits_wrapper_pin(tmp_path):
    """Unlisted nested grammars inherit the enclosing semgrep-<wrapper> pin."""
    grammar_dir = tmp_path / "semgrep-php" / "mystery-dialect"
    grammar_dir.mkdir(parents=True)
    (grammar_dir / "tree-sitter.json").write_text(
        '{"grammars": [{"name": "mystery-dialect", "scope": "source.x", "path": "."}]}'
    )
    assert version_for_grammar_dir(grammar_dir, lang_dir=LANG_DIR) == "0.26.3"


def test_version_for_grammar_dir_unlisted_without_wrapper(tmp_path):
    """Unlisted names outside a wrapper package still raise TsVersionError."""
    grammar_dir = tmp_path / "orphan" / "mystery-dialect"
    grammar_dir.mkdir(parents=True)
    with pytest.raises(TsVersionError, match="not in upstream-grammars"):
        version_for_grammar_dir(grammar_dir, lang_dir=LANG_DIR)


def test_key_for_package_name_clone_suffix_is_not_the_registry_key():
    """tree-sitter-go-mod maps to gomod, not the clone dirname."""
    assert key_for_package_name("go-mod", LANG_DIR) == "gomod"


def test_version_for_grammar_dir_inherits_clone_name(tmp_path):
    """Nested dirs under tree-sitter-<clone> inherit that entry's pin."""
    lang_dir = tmp_path / "lang"
    lang_dir.mkdir()
    (lang_dir / "upstream-grammars.json").write_text(
        json.dumps(
            {
                "mywrap": {
                    "url": "https://example.com/x.git",
                    "commit": "abc",
                    "tree_sitter": "0.22.6",
                    "regen": ["dest"],
                    "clone": "my-wrap",
                }
            }
        )
    )
    grammar_dir = tmp_path / "tree-sitter-my-wrap" / "nested"
    grammar_dir.mkdir(parents=True)
    (grammar_dir / "tree-sitter.json").write_text(
        '{"grammars": [{"name": "mystery-nested", "scope": "source.x", "path": "."}]}'
    )
    assert key_for_package_name("my-wrap", lang_dir) == "mywrap"
    assert version_for_grammar_dir(grammar_dir, lang_dir=lang_dir) == "0.22.6"


@pytest.mark.parametrize(
    "script,args,expected",
    [
        ("ts-versions", [], None),
        ("ts-version-for-lang", ["php"], "0.26.3\n"),
        (
            "ts-version-for-grammar-dir",
            [str(LANG_DIR / "semgrep-grammars/src/semgrep-php/php")],
            "0.26.3\n",
        ),
    ],
)
def test_cli_entry_points(script, args, expected):
    proc = subprocess.run([SCRIPTS / script, *args], capture_output=True, text=True)
    assert proc.returncode == 0, proc.stderr
    if expected is None:
        assert proc.stdout.strip()
    else:
        assert proc.stdout == expected


@pytest.mark.parametrize(
    "script", ["ts-version-for-lang", "ts-version-for-grammar-dir"]
)
def test_cli_usage_errors(script):
    proc = subprocess.run([SCRIPTS / script], capture_output=True, text=True)
    assert proc.returncode == 2


@pytest.mark.parametrize(
    "script", ["ts-version-for-lang", "ts-version-for-grammar-dir"]
)
def test_cli_preserves_registry_error(tmp_path, script):
    scripts = tmp_path / "scripts"
    scripts.mkdir()
    lang = tmp_path / "lang"
    lang.mkdir()
    (lang / "upstream-grammars.json").write_text('{"python": {}}')
    for name in [script, "grammar_registry.py", "ts_versions.py"]:
        shutil.copy(SCRIPTS_DIR / name, scripts / name)
    proc = subprocess.run([scripts / script, "python"], capture_output=True, text=True)
    assert proc.returncode == 1
    assert "python: missing url" in proc.stderr
    assert "not in upstream-grammars" not in proc.stderr
    assert "Traceback" not in proc.stderr
    assert not proc.stdout
