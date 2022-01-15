from unittest.mock import patch

from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.commands.login import Authentication
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME


def test_login(tmp_path):
    runner = CliRunner(env={SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path)})

    # Patch Token Validation:
    with patch.object(Authentication, "is_valid_token", return_value=True):
        # Logout
        result = runner.invoke(
            cli,
            ["logout"],
        )
        assert result.exit_code == 0
        assert result.output == "logged out\n"

        # Login
        result = runner.invoke(
            cli,
            ["login"],
            input="key123",
        )
        print(result.output)
        assert result.exit_code == 0
        assert "Valid API Token saved in" in result.output

        # Login should fail on second call
        result = runner.invoke(
            cli,
            ["login"],
        )
        assert result.exit_code == 1
        assert "API token already exists in" in result.output

        # Clear login
        result = runner.invoke(
            cli,
            ["logout"],
        )
        assert result.exit_code == 0
        assert result.output == "logged out\n"

        # Logout twice should work
        result = runner.invoke(
            cli,
            ["logout"],
        )
        assert result.exit_code == 0
        assert result.output == "logged out\n"

        # Check registry config works while logged in
        # Login
        result = runner.invoke(
            cli,
            ["login"],
            input="key123",
        )
        print(result.output)
        assert result.exit_code == 0
        assert "Valid API Token saved in" in result.output

        # Run p/ci
        result = runner.invoke(
            cli,
            ["--config", "r/python.lang.correctness.useless-eqeq.useless-eqeq"],
        )
        assert result.exit_code == 0

        # Run policy with bad token -> no associated deployment_id
        result = runner.invoke(cli, ["--config", "policy"])
        assert result.exit_code == 1
        assert "Invalid API Key" in result.output

        with patch.object(Authentication, "get_deployment_id", return_value=1):
            # Run policy without SEMGREP_REPO_NAME
            result = runner.invoke(
                cli,
                ["--config", "policy"],
            )
            assert result.exit_code == 1
            assert "Need to set env var SEMGREP_REPO_NAME" in result.output

            # Run policy. Check that request is made to correct deployment + org/repo
            result = runner.invoke(
                cli, ["--config", "policy"], env={"SEMGREP_REPO_NAME": "org/repo"}
            )
            assert result.exit_code == 1
            assert (
                "https://semgrep.dev/api/agent/deployment/1/repos/org/repo/rules.yaml"
                in result.output
            )
