import pytest
from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME


@pytest.mark.quick
@pytest.mark.parametrize("help_flag", ["--help", "-h"])
def test_help_text(tmp_path, snapshot, help_flag):
    """
    Test to make sure top level help text doesn't change unintentionally
    """
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
        }
    )
    result = runner.invoke(cli, [help_flag], env={})
    snapshot.assert_match(result.output, "help.txt")
