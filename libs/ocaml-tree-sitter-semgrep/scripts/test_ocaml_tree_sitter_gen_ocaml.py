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
"""Regression tests for the OCaml tree-sitter C binding generator."""
import shutil
import subprocess
from pathlib import Path


SCRIPTS_DIR = Path(__file__).resolve().parent
GENERATOR = SCRIPTS_DIR / "ocaml-tree-sitter-gen-ocaml"


def test_generated_binding_uses_strict_c_function_forms(tmp_path: Path) -> None:
    root = tmp_path / "ocaml-tree-sitter-semgrep"
    scripts_dir = root / "scripts"
    core_bin = root / "core" / "bin"
    src_dir = tmp_path / "src"
    dst_dir = tmp_path / "out"

    scripts_dir.mkdir(parents=True)
    core_bin.mkdir(parents=True)
    (src_dir / "tree_sitter").mkdir(parents=True)

    generator = scripts_dir / GENERATOR.name
    shutil.copy2(GENERATOR, generator)

    fake_ocaml_tree_sitter = core_bin / "ocaml-tree-sitter"
    fake_ocaml_tree_sitter.write_text('#!/bin/sh\nmkdir -p "$5/bin"\n')
    fake_ocaml_tree_sitter.chmod(0o755)

    (src_dir / "grammar.json").write_text("{}\n")
    (src_dir / "parser.c").write_text("/* parser fixture */\n")
    (src_dir / "tree_sitter" / "parser.h").write_text("/* header fixture */\n")

    result = subprocess.run(
        [
            generator,
            "--lang",
            "demo",
            "--src",
            src_dir,
            "--dst",
            dst_dir,
        ],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr

    bindings = (dst_dir / "lib" / "bindings.c").read_text()
    assert "const TSLanguage *tree_sitter_demo(void);" in bindings
    assert "TSLanguage *tree_sitter_demo();" not in bindings
    assert "CAMLprim value octs_create_parser_demo(value unit) {" in bindings
    assert "  (void)unit;\n" in bindings
    assert "CAMLreturn(v);\n}" in bindings
    assert "CAMLreturn(v);\n};" not in bindings
