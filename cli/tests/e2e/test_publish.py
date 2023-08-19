from pathlib import Path

import pytest
from tests.conftest import TESTS_PATH
from tests.semgrep_runner import SemgrepRunner

from semgrep.cli import cli


@pytest.mark.kinda_slow
def test_publish(tmp_path, mocker):
    runner = SemgrepRunner(
        env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")},
        use_click_runner=True,
    )

    tests_path = Path(TESTS_PATH / "e2e" / "targets" / "semgrep-publish" / "valid")
    valid_target = str(tests_path.resolve())
    valid_single_file_target = str((tests_path / "valid1.yaml").resolve())

    result = runner.invoke(
        cli,
        ["logout"],
    )
    assert result.exit_code == 0

    # should require login
    result = runner.invoke(
        cli,
        ["publish", valid_target],
    )
    print(result.output)
    assert result.exit_code == 2
    assert result.output == "run `semgrep login` before using upload\n"

    mocker.patch(
        "semgrep.app.auth.get_deployment_from_token", return_value="deployment_name"
    )

    # log back in
    result = runner.invoke(cli, ["login"], env={"SEMGREP_APP_TOKEN": "fakeapitoken"})
    assert result.exit_code == 0

    # fails if no rule specified
    result = runner.invoke(
        cli,
        ["publish"],
    )
    assert result.exit_code == 2

    # fails if invalid rule specified
    result = runner.invoke(
        cli,
        [
            "publish",
            str(
                Path(
                    TESTS_PATH / "e2e" / "targets" / "semgrep-publish" / "invalid"
                ).resolve()
            ),
        ],
    )
    assert result.exit_code == 2
    assert "Invalid rule definition:" in result.output

    # fails if a yaml with more than one rule is specified
    result = runner.invoke(
        cli,
        [
            "publish",
            str(
                Path(
                    TESTS_PATH / "e2e" / "targets" / "semgrep-publish" / "multirule"
                ).resolve()
            ),
        ],
    )
    assert result.exit_code == 2
    assert (
        "Rule contains more than one rule: only yaml files with a single can be published"
        in result.output
    )

    # fails if --visibility=public without --rule-id
    result = runner.invoke(
        cli,
        ["publish", "--visibility=public", valid_target],
    )
    assert result.exit_code == 2
    assert (
        "Only one public rule can be uploaded at a time: specify a single Semgrep rule"
        in result.output
    )

    result = runner.invoke(
        cli,
        ["publish", "--visibility=public", valid_single_file_target],
    )
    assert result.exit_code == 2
    assert "--visibility=public requires --registry-id" in result.output
