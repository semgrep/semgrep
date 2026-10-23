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
import importlib.resources
import os
import platform
import shutil
import sys

import pytest


@pytest.mark.quick
def test_missing_core_on_intel_macos_explains_alternatives(monkeypatch) -> None:
    # Importing entrypoint appends to PATH; let monkeypatch restore it.
    monkeypatch.setenv("PATH", os.environ.get("PATH", ""))
    entrypoint = importlib.import_module("semgrep.console_scripts.entrypoint")
    monkeypatch.setattr(sys, "platform", "darwin")
    monkeypatch.setattr(platform, "machine", lambda: "x86_64")

    def no_packaged_binary(*_args):
        raise FileNotFoundError

    monkeypatch.setattr(importlib.resources, "path", no_packaged_binary)
    monkeypatch.setattr(shutil, "which", lambda _name: None)

    with pytest.raises(entrypoint.CoreNotFound) as excinfo:
        entrypoint.find_semgrep_core_path()

    assert "Intel macOS" in str(excinfo.value)
    assert "semgrep/semgrep Docker image" in str(excinfo.value)
