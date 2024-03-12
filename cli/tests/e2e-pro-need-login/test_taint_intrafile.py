import pytest
from tests.fixtures import RunSemgrep
from tests.semgrep_runner import SemgrepRunner

from semgrep.cli import cli
from semgrep.constants import OutputFormat


def fake_logout_login(tmp_path, mocker):
    # Fake the login, so that we can run --pro_intrafile
    runner = SemgrepRunner(
        env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")},
        use_click_runner=True,
    )
    fake_key = "key123"

    # Patch Token Validation:
    mocker.patch(
        "semgrep.app.auth.get_deployment_from_token", return_value="deployment_name"
    )

    # Logout
    result = runner.invoke(cli, subcommand="logout", args=[])
    assert result.exit_code == 0
    assert result.output.startswith("Logged out")

    # Login with env token
    result = runner.invoke(
        cli,
        subcommand="login",
        args=[],
        env={"SEMGREP_APP_TOKEN": fake_key},
    )
    assert result.exit_code == 0
    assert result.output.startswith("Saved login token")
    assert "<redacted>" in result.output


@pytest.mark.kinda_slow
@pytest.mark.osemfail
@pytest.mark.parametrize(
    "rule,target",
    [("rules/taint_intrafile.yaml", "taint/taint_intrafile.py")],
)
def test_taint_intrafile(
    run_semgrep_in_tmp: RunSemgrep, tmp_path, mocker, snapshot, rule, target
):
    fake_logout_login(tmp_path, mocker)

    # Test
    semgrep_result = run_semgrep_in_tmp(
        rule,
        target_name=target,
        output_format=OutputFormat.TEXT,
        options=["--pro-intrafile"],
    )

    # Hack: results may include trailing whitespaces, but pre-commit
    # and CI force us to remove them.
    expected = semgrep_result.stdout.rstrip()

    snapshot.assert_match(
        expected,
        "results.txt",
    )
