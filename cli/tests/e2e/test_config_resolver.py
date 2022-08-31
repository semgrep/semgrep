from textwrap import dedent

import pytest
from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.config_resolver import ConfigLoader


@pytest.mark.quick
def test_new_feature_registry_config(monkeypatch, snapshot, mocker, tmp_path):
    file_content = dedent(
        """
        rules:
        - id: eqeq-bad
          pattern-new-feature: $X == $X
          message: "useless comparison"
          languages: [python]
          severity: ERROR
        """
    ).lstrip()
    mocker.patch.object(ConfigLoader, "_make_config_request", return_value=file_content)

    runner = CliRunner(
        env={
            "SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml"),
            "SEMGREP_APP_TOKEN": "",
        }
    )
    result = runner.invoke(cli, ["scan", "--config", "p/ci"])
    snapshot.assert_match(result.output, "output.txt")
