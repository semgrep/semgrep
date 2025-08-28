#
# Copyright (c) 2022-2024 Semgrep Inc.
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
import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_last_log_exists(run_semgrep_in_tmp: RunSemgrep, tmp_path):
    log_dest = tmp_path / "foo" / "bar" / "last.log"
    run_semgrep_in_tmp("rules/eqeq.yaml", env={"SEMGREP_LOG_FILE": str(log_dest)})

    log = log_dest.read_text()
    assert "- DEBUG -" in log
    assert "- INFO -" in log
    assert "- VERBOSE -" in log
