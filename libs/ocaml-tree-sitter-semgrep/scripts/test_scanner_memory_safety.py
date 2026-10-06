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
"""Run vendored external scanners under AddressSanitizer.

Each case compiles a small C harness together with a scanner.c we ship,
puts the scanner in a state that used to write or read outside its
allocation, and fails if AddressSanitizer or UBSan reports an error.

The harnesses use scanner internals (struct and helper names). If upstream
renames them, the harness stops compiling and the test fails; update the
harness to reach the same state.
"""
from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

import pytest

OTS = Path(__file__).resolve().parent.parent
OSS = OTS.parent.parent
LANGUAGES = OSS / "languages"
WRAPPERS = OTS / "lang/semgrep-grammars/src"

RUBY_LIB = LANGUAGES / "ruby/tree-sitter/semgrep-ruby/lib"
PYTHON_LIB = LANGUAGES / "python/tree-sitter/semgrep-python/lib"
SWIFT_LIB = LANGUAGES / "swift/tree-sitter/semgrep-swift/lib"

# One open heredoc with a 1019-byte word. The old size check allowed it, but
# the serializer wrote 3 + 1 + 1019 bytes after 2 header bytes: 1025 bytes
# into the 1024-byte buffer.
RUBY_HARNESS = """
int main(void) {
  Scanner *s = tree_sitter_ruby_external_scanner_create();
  Heredoc h = {0};
  h.word = (String)array_new();
  for (int i = 0; i < 1019; i++) array_push(&h.word, 'A');
  array_push(&s->open_heredocs, h);
  char *buf = malloc(TREE_SITTER_SERIALIZATION_BUFFER_SIZE);
  tree_sitter_ruby_external_scanner_serialize(s, buf);
  free(buf);
  tree_sitter_ruby_external_scanner_destroy(s);
  return 0;
}
"""

# One open string delimiter plus 600 indent levels. The old loop checked
# `size < 1024` but wrote 2 bytes per step, so its last write hit byte 1024.
PYTHON_HARNESS = """
int main(void) {
  Scanner *s = tree_sitter_python_external_scanner_create();
  array_push(&s->delimiters, new_delimiter());
  for (int i = 1; i <= 600; i++) array_push(&s->indents, (uint16_t)i);
  char *buf = malloc(TREE_SITTER_SERIALIZATION_BUFFER_SIZE);
  tree_sitter_python_external_scanner_serialize(s, buf);
  free(buf);
  tree_sitter_python_external_scanner_destroy(s);
  return 0;
}
"""

# create() used calloc(0, ...), and reset() then wrote the 4-byte state.
SWIFT_HARNESS = """
int main(void) {
  void *s = tree_sitter_swift_external_scanner_create();
  tree_sitter_swift_external_scanner_reset(s);
  tree_sitter_swift_external_scanner_destroy(s);
  return 0;
}
"""

# (scanner.c to test, directory holding its tree_sitter/ headers, harness)
CASES = [
    pytest.param(RUBY_LIB / "scanner.c", RUBY_LIB, RUBY_HARNESS, id="ruby"),
    # Semgrep's own Ruby scanner, which regen copies into RUBY_LIB. Its
    # directory has no headers, so borrow the vendored ones.
    pytest.param(
        WRAPPERS / "semgrep-ruby/src/scanner.c",
        RUBY_LIB,
        RUBY_HARNESS,
        id="ruby-wrapper",
    ),
    pytest.param(PYTHON_LIB / "scanner.c", PYTHON_LIB, PYTHON_HARNESS, id="python"),
    pytest.param(SWIFT_LIB / "scanner.c", SWIFT_LIB, SWIFT_HARNESS, id="swift"),
]


def c_compiler() -> str:
    """Return a C compiler that supports AddressSanitizer."""
    for candidate in (os.environ.get("CC"), "clang", "gcc", "cc"):
        if candidate and shutil.which(candidate):
            return candidate
    pytest.fail("no C compiler found; set CC or install clang or gcc")


@pytest.mark.parametrize("scanner,include_dir,harness", CASES)
def test_scanner_stays_in_bounds(tmp_path, scanner, include_dir, harness):
    source = tmp_path / "harness.c"
    source.write_text(f'#include "{scanner}"\n{harness}')
    binary = tmp_path / "harness"
    compile_cmd = [
        c_compiler(),
        "-g",
        "-O0",
        "-w",
        "-fsanitize=address,undefined",
        "-fno-sanitize-recover=all",
        "-fno-omit-frame-pointer",
        f"-I{include_dir}",
        str(source),
        "-o",
        str(binary),
    ]
    built = subprocess.run(compile_cmd, capture_output=True, text=True)
    assert built.returncode == 0, (
        f"harness for {scanner} did not compile (did upstream rename scanner "
        f"internals?):\n{built.stderr}"
    )
    env = {**os.environ, "ASAN_OPTIONS": "detect_leaks=0:abort_on_error=0"}
    ran = subprocess.run([str(binary)], capture_output=True, text=True, env=env)
    assert ran.returncode == 0, (
        f"{scanner} accessed memory outside its allocation (SECENG-77):\n"
        f"{ran.stderr}"
    )
