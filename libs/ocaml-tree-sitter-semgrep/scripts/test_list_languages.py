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
"""Unit tests for scripts/list-languages."""
from __future__ import annotations

import json
import subprocess
from pathlib import Path

SCRIPTS_DIR = Path(__file__).resolve().parent
SCRIPT = SCRIPTS_DIR / "list-languages"


def test_list_languages_excludes_nested_sub_dialects():
    """Lists cfml/sfapex but not the sub-dialects nested inside their dirs."""
    proc = subprocess.run([SCRIPT], capture_output=True, text=True, check=True)
    langs = proc.stdout.splitlines()
    assert {"cfml", "sfapex"} <= set(langs)
    assert not {"cfquery", "cfscript", "soql", "sosl"} & set(langs)

    json_proc = subprocess.run(
        [SCRIPT, "--json"], capture_output=True, text=True, check=True
    )
    assert json.loads(json_proc.stdout) == langs
