#
# Copyright (c) 2021-2025 Semgrep Inc.
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
import re
import subprocess

import pytest
from tests.semgrep_runner import mk_semgrep_base_command


@pytest.mark.kinda_slow
def test_version():
    cmd = mk_semgrep_base_command("--version", ["--disable-version-check"])
    result = subprocess.check_output(
        cmd,
        encoding="utf-8",
    )

    assert re.match(r"\d+\.\d+\.\d+", result)
