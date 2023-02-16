from __future__ import annotations

import pytest

from semgrep.cli import cli
from tests.semgrep_runner import SemgrepRunner


@pytest.mark.quick()
@pytest.mark.parametrize("help_flag", ["--help", "-h"])
def test_help_text(tmp_path, snapshot, help_flag):
    """
    Test to make sure top level help text doesn't change unintentionally
    """
    runner = SemgrepRunner(
        env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")}
    )
    result = runner.invoke(cli, [help_flag], env={})
    snapshot.assert_match(result.output, "help.txt")
