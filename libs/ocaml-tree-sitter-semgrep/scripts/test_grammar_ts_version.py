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
"""Verify each grammar uses the tree-sitter version from upstream-grammars.json."""
import re
import shutil
import subprocess
from pathlib import Path

import pytest
from grammar_registry import load
from ts_versions import list_pinned_versions
from ts_versions import version_for_grammar_dir
from ts_versions import version_for_lang

SCRIPTS_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPTS_DIR.parent
LANG_DIR = REPO_ROOT / "lang"
SRC = LANG_DIR / "semgrep-grammars" / "src"

VERSION_RE = re.compile(r"\d+\.\d+\.\d+")
REPORTED_RE = re.compile(r"\(tree-sitter (\d+\.\d+\.\d+)\)")

# Dialect names exercised by test-lang / Makefile but not registry keys.
_EXTRA_NAMES = ("soql", "sosl", "apex", "tsx", "php-only", "cfquery", "cfscript")


def _all_listed_names():
    reg = load(LANG_DIR)
    names = set(reg.keys())
    names.update(_EXTRA_NAMES)
    return sorted(names)


def _grammar_dirs():
    dirs = []
    for pkg in sorted(SRC.glob("semgrep-*")):
        if pkg.is_dir():
            dirs.extend(sorted(gj.parent for gj in pkg.rglob("grammar.js")))
    return dirs


def _binary(version):
    return REPO_ROOT / "core" / f"tree-sitter-{version}" / "bin" / "tree-sitter"


VERSIONS = list_pinned_versions(LANG_DIR)
GRAMMAR_DIRS = _grammar_dirs()


def test_versions_are_discovered():
    assert VERSIONS, "no tree-sitter versions in upstream-grammars.json"


@pytest.mark.parametrize("name", _all_listed_names())
def test_pin_is_unique_and_resolves(name):
    version_for_lang(name, LANG_DIR)


@pytest.mark.parametrize(
    "grammar_dir", GRAMMAR_DIRS, ids=lambda d: str(d.relative_to(SRC))
)
def test_grammar_dir_resolves_to_a_declared_version(grammar_dir):
    version = version_for_grammar_dir(grammar_dir, LANG_DIR)
    assert version in VERSIONS


@pytest.mark.parametrize("version", VERSIONS)
def test_installed_binary_reports_its_version(version):
    binary = _binary(version)
    if not binary.exists():
        pytest.fail(f"tree-sitter {version} not installed at {binary}")
    out = subprocess.run([binary, "--version"], capture_output=True, text=True).stdout
    m = VERSION_RE.search(out)
    assert (
        m and m.group(0) == version
    ), f"{binary} reports {out.strip()!r}, expected {version}"


@pytest.mark.parametrize("version", VERSIONS)
def test_generation_uses_the_pinned_version(version):
    if not _binary(version).exists():
        pytest.fail(f"tree-sitter {version} not installed")
    if not shutil.which("node"):
        pytest.fail("node not available (tree-sitter generate needs it)")

    package = None
    for grammar_dir in GRAMMAR_DIRS:
        if version_for_grammar_dir(grammar_dir, LANG_DIR) == version:
            package = SRC / grammar_dir.relative_to(SRC).parts[0]
            break
    if package is None:
        pytest.fail(f"no grammar pinned to {version}")

    for gj in package.rglob("grammar.js"):
        gj.touch()
    proc = subprocess.run(
        ["make", "-C", str(package), "build"], capture_output=True, text=True
    )
    reported = sorted(set(REPORTED_RE.findall(proc.stdout + proc.stderr)))

    wrong = [r for r in reported if r != version]
    assert not wrong, (
        f"{package.name} was generated with tree-sitter {wrong}, expected {version}\n"
        + proc.stdout[-2000:]
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert version in reported, proc.stdout + proc.stderr
