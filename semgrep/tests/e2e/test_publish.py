from pathlib import Path
from unittest.mock import patch

import pytest
from click.testing import CliRunner

from semgrep.app import auth
from semgrep.cli import cli
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME
from tests.conftest import TESTS_PATH


@pytest.mark.kinda_slow
def test_publish(tmp_path):
    runner = CliRunner(env={SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path)})

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

    # Patch Token Validation:
    with patch.object(auth, "is_valid_token", return_value=True):

        # log back in
        result = runner.invoke(
            cli, ["login"], env={auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "fakeapitoken"}
        )
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
