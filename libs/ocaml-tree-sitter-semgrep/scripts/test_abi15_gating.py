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
"""Test ABI selection for tree-sitter generate (SEMGREP_ENABLE_ABI15 gating).

Layer 1 exercises ``scripts/generate_abi_args.py`` (decision table).
Layer 2 verifies the env var actually changes ``LANGUAGE_VERSION`` in
``parser.c`` when running ``tree-sitter generate`` (requires tree-sitter
>= 0.25.0 and node).

Run with: pytest scripts/test_abi15_gating.py
"""
import io
import os
import re
import shutil
import subprocess
from pathlib import Path

import pytest
from generate_abi_args import generate_abi_args
from generate_abi_args import GenerateAbiError
from ts_versions import list_pinned_versions
from ts_versions import version_at_least

SCRIPTS_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPTS_DIR.parent
LANG_DIR = REPO_ROOT / "lang"
ABI_ARGS_SCRIPT = SCRIPTS_DIR / "ts-generate-abi-args"

LANGUAGE_VERSION_RE = re.compile(r"^#define LANGUAGE_VERSION (\d+)", re.MULTILINE)
VERSION_RE = re.compile(r"tree-sitter (\d+\.\d+\.\d+)")

GRAMMAR_JS = """\
module.exports = grammar({
  name: 'abitest',
  rules: {
    source_file: $ => repeat($.word),
    word: $ => /[a-z]+/,
  },
});
"""

TREE_SITTER_JSON = """\
{
  "grammars": [{ "name": "abitest", "scope": "source.abitest", "path": "." }],
  "metadata": { "version": "0.0.0", "license": "MIT", "description": "ABI gating test grammar" },
  "bindings": { "c": true, "go": false, "node": false, "python": false, "rust": false, "swift": false }
}
"""


def _abi_args_env(*, abi15: str | None = None) -> dict[str, str]:
    """Build an environment mapping for ABI 15 gating tests."""
    env = os.environ.copy()
    if abi15 is None:
        env.pop("SEMGREP_ENABLE_ABI15", None)
    else:
        env["SEMGREP_ENABLE_ABI15"] = abi15
    return env


def _run_abi_args(
    grammar_dir: Path, ts_version: str, *, abi15: str | None = None
) -> str:
    """Return ABI args from the Python API with a controlled env."""
    return generate_abi_args(grammar_dir, ts_version, env=_abi_args_env(abi15=abi15))


def _run_abi_args_cli(
    grammar_dir: Path, ts_version: str, *, abi15: str | None = None
) -> str:
    """Return ABI args from the CLI wrapper with a controlled env."""
    proc = subprocess.run(
        [ABI_ARGS_SCRIPT, str(grammar_dir), ts_version],
        capture_output=True,
        text=True,
        env=_abi_args_env(abi15=abi15),
    )
    assert proc.returncode == 0, proc.stderr
    return proc.stdout.strip()


def _default_tree_sitter_bin() -> Path | None:
    """Return an ABI-15-capable tree-sitter binary if one is available."""
    override = os.environ.get("TREE_SITTER_BIN")
    if override:
        path = Path(override)
        return path if path.is_file() and os.access(path, os.X_OK) else None
    version = max(
        list_pinned_versions(LANG_DIR), key=lambda v: tuple(map(int, v.split(".")))
    )
    candidate = REPO_ROOT / "core" / f"tree-sitter-{version}" / "bin" / "tree-sitter"
    return candidate if candidate.is_file() and os.access(candidate, os.X_OK) else None


def _reported_version(ts_bin: Path) -> str | None:
    """Parse the semver from ``tree-sitter --version`` output."""
    proc = subprocess.run([ts_bin, "--version"], capture_output=True, text=True)
    match = VERSION_RE.search(proc.stdout)
    return match.group(1) if match else None


def _generate_and_read_language_version(
    grammar_dir: Path,
    ts_bin: Path,
    ts_version: str,
    *,
    abi15: str | None = None,
) -> str | None:
    """Generate parser.c and return the LANGUAGE_VERSION macro value."""
    parser_c = grammar_dir / "src" / "parser.c"
    if parser_c.exists():
        parser_c.unlink()
    args = _run_abi_args(grammar_dir, ts_version, abi15=abi15)
    env = _abi_args_env(abi15=abi15)
    env["PATH"] = f"{ts_bin.parent}:{env.get('PATH', '')}"
    proc = subprocess.run(
        ["tree-sitter", "generate", *args.split()],
        cwd=grammar_dir,
        capture_output=True,
        text=True,
        env=env,
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    match = LANGUAGE_VERSION_RE.search(parser_c.read_text())
    return match.group(1) if match else None


@pytest.fixture
def grammar_dirs(tmp_path):
    """Provide grammar directories with and without tree-sitter.json."""
    with_json = tmp_path / "with-json"
    without_json = tmp_path / "without-json"
    with_json.mkdir()
    without_json.mkdir()
    (with_json / "tree-sitter.json").write_text("{}")
    return with_json, without_json


@pytest.mark.parametrize(
    "has_json,ts_version,abi15,expected",
    [
        (True, "0.26.3", "1", "--abi=15"),
        (True, "0.26.3", None, "--abi=14"),
        (True, "0.26.3", "0", "--abi=14"),
        (True, "0.26.3", "false", "--abi=14"),
        (False, "0.26.3", "1", "--abi=14"),
        (False, "0.24.0", None, "--abi=14"),
        (False, "0.20.8", None, "--no-bindings"),
    ],
    ids=[
        "json+abi15-on",
        "json+abi15-unset",
        "json+abi15=0",
        "json+abi15=false",
        "no-json+abi15-on",
        "no-json-0.24.0",
        "no-json-0.20.8",
    ],
)
def test_generate_abi_args(grammar_dirs, has_json, ts_version, abi15, expected):
    """generate_abi_args returns the expected flag for each input combination."""
    with_json, without_json = grammar_dirs
    grammar_dir = with_json if has_json else without_json
    assert _run_abi_args(grammar_dir, ts_version, abi15=abi15) == expected


@pytest.mark.parametrize(
    "has_json,ts_version,abi15,expected",
    [
        (True, "0.26.3", "1", "--abi=15"),
        (True, "0.26.3", None, "--abi=14"),
        (False, "0.20.8", None, "--no-bindings"),
    ],
)
def test_generate_abi_args_cli(grammar_dirs, has_json, ts_version, abi15, expected):
    """The ts-generate-abi-args CLI wrapper matches the Python API."""
    with_json, without_json = grammar_dirs
    grammar_dir = with_json if has_json else without_json
    assert _run_abi_args_cli(grammar_dir, ts_version, abi15=abi15) == expected


def test_generate_abi_args_warns_on_abi15_without_json(grammar_dirs):
    """ABI 15 requested without a tree-sitter.json warns and falls back."""
    _, without_json = grammar_dirs
    warn = io.StringIO()
    result = generate_abi_args(
        without_json, "0.26.3", env=_abi_args_env(abi15="1"), warn=warn
    )
    assert result == "--abi=14"
    message = warn.getvalue()
    assert "SEMGREP_ENABLE_ABI15" in message
    assert "tree-sitter.json" in message
    assert "--abi=14" in message


def test_generate_abi_args_no_warning_when_satisfied(grammar_dirs):
    """No warning when ABI 15 is unset or the grammar has a tree-sitter.json."""
    with_json, without_json = grammar_dirs
    for grammar_dir, abi15 in [
        (with_json, "1"),
        (with_json, None),
        (without_json, None),
    ]:
        warn = io.StringIO()
        generate_abi_args(
            grammar_dir, "0.26.3", env=_abi_args_env(abi15=abi15), warn=warn
        )
        assert warn.getvalue() == ""


def test_generate_abi_args_abi15_requires_recent_tree_sitter(grammar_dirs):
    """ABI 15 with an old pinned tree-sitter version is rejected."""
    with_json, _ = grammar_dirs
    with pytest.raises(GenerateAbiError, match="requires tree-sitter >= 0.25.0"):
        generate_abi_args(with_json, "0.22.6", env=_abi_args_env(abi15="1"))


@pytest.mark.parametrize("args", [[], ["dir"]])
def test_generate_abi_args_cli_usage_errors(args):
    """ts-generate-abi-args returns exit code 2 (argparse) on the wrong number of arguments."""
    proc = subprocess.run([ABI_ARGS_SCRIPT, *args], capture_output=True, text=True)
    assert proc.returncode == 2


def test_abi15_env_var_flips_generated_language_version(tmp_path):
    """SEMGREP_ENABLE_ABI15 changes LANGUAGE_VERSION in generated parser.c."""
    ts_bin = _default_tree_sitter_bin()
    if ts_bin is None:
        pytest.fail(
            "tree-sitter >= 0.25.0 not installed "
            "(run make ots-test-python from the Semgrep root to provision the pinned CLIs)"
        )
    ts_version = _reported_version(ts_bin)
    if ts_version is None or not version_at_least(ts_version, "0.25.0"):
        pytest.fail(f"tree-sitter {ts_version!r} at {ts_bin} does not support ABI 15")
    if shutil.which("node") is None:
        pytest.fail("node not available (tree-sitter generate needs it)")

    grammar_dir = tmp_path / "grammar"
    grammar_dir.mkdir()
    (grammar_dir / "grammar.js").write_text(GRAMMAR_JS)
    (grammar_dir / "tree-sitter.json").write_text(TREE_SITTER_JSON)

    abi_off = _generate_and_read_language_version(
        grammar_dir, ts_bin, ts_version, abi15=None
    )
    abi_on = _generate_and_read_language_version(
        grammar_dir, ts_bin, ts_version, abi15="1"
    )

    assert abi_off == "14"
    assert abi_on == "15"
    assert abi_off != abi_on
