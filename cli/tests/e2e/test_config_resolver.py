from textwrap import dedent

import pytest
from tests.semgrep_runner import SemgrepRunner

from semgrep.cli import cli
from semgrep.config_resolver import ConfigFile
from semgrep.config_resolver import ConfigLoader


@pytest.mark.quick
def test_new_feature_registry_config(monkeypatch, snapshot, mocker, tmp_path):
    config_file = ConfigFile(
        None,
        dedent(
            """
            rules:
            - id: eqeq-bad
              pattern-new-feature: $X == $X
              message: "useless comparison"
              languages: [python]
              severity: ERROR
            """
        ).lstrip(),
        "https://semgrep.dev/p/ci",
    )
    mocker.patch.object(
        ConfigLoader, "_download_config_from_url", return_value=config_file
    )

    runner = SemgrepRunner(
        env={
            "SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml"),
            "SEMGREP_APP_TOKEN": "",
        }
    )
    result = runner.invoke(cli, ["scan", "--config", "p/ci"])
    snapshot.assert_match(result.output, "output.txt")
