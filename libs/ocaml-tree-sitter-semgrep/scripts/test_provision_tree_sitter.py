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
"""Exercise CLI downloads without network access or runtime libraries."""
import gzip
import os
import shutil
import subprocess
from pathlib import Path

import pytest

PROVISION = Path(__file__).resolve().parents[1] / "core/scripts/provision-tree-sitter"
VERSION = "0.22.6"


@pytest.fixture
def provision(tmp_path):
    scripts = tmp_path / "core/scripts"
    scripts.mkdir(parents=True)
    script = scripts / PROVISION.name
    shutil.copy2(PROVISION, script)
    tools = tmp_path / "tools"
    tools.mkdir()
    curl = tools / "curl"
    curl.write_text('#!/bin/sh\ncat "$DOWNLOAD"\nexit "${CURL_STATUS:-0}"\n')
    curl.chmod(0o755)
    download = tmp_path / "cli.gz"
    download.write_bytes(gzip.compress(b"#!/bin/sh\necho 'tree-sitter 0.22.6'\n"))
    env = {
        **os.environ,
        "PATH": f"{tools}:{os.environ['PATH']}",
        "DOWNLOAD": str(download),
    }
    env.pop("FORCE", None)
    binary = tmp_path / f"core/tree-sitter-{VERSION}/bin/tree-sitter"

    def run(**overrides):
        return subprocess.run(
            [script, VERSION], env={**env, **overrides}, capture_output=True, text=True
        )

    return run, binary, download


def test_cold_download_and_offline_cache(provision):
    run, binary, download = provision
    result = run()
    assert result.returncode == 0, result.stderr
    assert (
        subprocess.check_output([binary, "--version"], text=True).strip()
        == f"tree-sitter {VERSION}"
    )
    assert sorted(p.name for p in binary.parent.parent.iterdir()) == ["bin"]
    download.unlink()
    assert run().returncode == 0


@pytest.mark.parametrize("failure", ["http", "gzip", "version"])
@pytest.mark.parametrize("cached", [False, True])
def test_failed_download_preserves_cache(provision, failure, cached):
    run, binary, download = provision
    if cached:
        assert run().returncode == 0
        original = binary.read_bytes()
    if failure == "gzip":
        download.write_bytes(b"truncated archive")
    elif failure == "version":
        download.write_bytes(gzip.compress(b"#!/bin/sh\necho 'tree-sitter 0.0.0'\n"))
    result = run(FORCE="1", CURL_STATUS="22" if failure == "http" else "0")
    assert result.returncode != 0
    if cached:
        assert binary.read_bytes() == original
    else:
        assert not binary.exists()
    assert not list(binary.parent.glob(".download.*"))
