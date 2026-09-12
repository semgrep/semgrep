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
"""Generated parser builds use the repository's OCaml libraries."""
from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

import pytest

OTS = Path(__file__).resolve().parent.parent


@pytest.mark.parametrize("nested", [False, True])
@pytest.mark.parametrize("symlink", [False, True])
def test_build_uses_workspace_libraries(tmp_path, nested, symlink):
    workspace = tmp_path / "checkout"
    oss = workspace / "OSS" if nested else workspace
    ots = oss / "libs/ocaml-tree-sitter-semgrep"
    core_scripts = ots / "core/scripts"
    core_scripts.mkdir(parents=True)
    shutil.copy(OTS / "core/scripts/build-ocaml", core_scripts / "build-ocaml")
    shutil.copy(OTS.parent.parent / "tree-sitter-config.sh", oss)
    scripts = ots / "scripts"
    scripts.mkdir()
    (scripts / "build-ocaml").symlink_to("../core/scripts/build-ocaml")
    lang = ots / "lang/toy"
    (lang / "ocaml-src").mkdir(parents=True)
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    dune = bin_dir / "dune"
    dune.write_text('#!/bin/sh\nprintf "%s\\n" "$OCAMLPATH" "$PWD" "$@"\n')
    dune.chmod(0o755)
    entry = scripts if symlink else core_scripts
    proc = subprocess.run(
        [entry / "build-ocaml"],
        cwd=lang,
        env={
            **os.environ,
            "PATH": f"{bin_dir}:{os.environ['PATH']}",
            "OCAMLPATH": "/existing",
        },
        check=True,
        capture_output=True,
        text=True,
    )
    assert proc.stdout.splitlines() == [
        f"{workspace.resolve()}/_build/install/default/lib:/existing",
        str((lang / "ocaml-src").resolve()),
        "build",
        "--root",
        ".",
    ]


def test_grammar_clean_preserves_scanner(tmp_path):
    src = tmp_path / "src"
    src.mkdir()
    for name in ["scanner.c", "parser.c", "grammar.json", "node-types.json"]:
        (src / name).write_text(name)
    subprocess.run(
        ["make", "-f", str(OTS / "lang/semgrep-grammars/src/Makefile.common"), "clean"],
        cwd=tmp_path,
        check=True,
    )
    assert sorted(p.name for p in src.iterdir()) == ["scanner.c"]
