from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME


def test_login(tmp_path):
    runner = CliRunner()

    # Login
    result = runner.invoke(
        cli, ["login"], input="key123", env={SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path)}
    )
    assert result.exit_code == 0
    assert "Valid API Token saved in" in result.output

    # Login should fail on second call
    result = runner.invoke(
        cli, ["login"], env={SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path)}
    )
    assert result.exit_code == 1
    assert "API token already exists in" in result.output

    # Clear login
    result = runner.invoke(
        cli, ["logout"], env={SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path)}
    )
    assert result.exit_code == 0
    assert result.output == "logged out\n"

    # Logout twice should work
    result = runner.invoke(
        cli, ["logout"], env={SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path)}
    )
    assert result.exit_code == 0
    assert result.output == "logged out\n"
