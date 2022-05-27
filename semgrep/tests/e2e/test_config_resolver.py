from textwrap import dedent
from semgrep.app import auth
from click.testing import CliRunner
from semgrep.cli import cli
from semgrep.config_resolver import ConfigPath
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME

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
    mocker.patch.object(ConfigPath, "_make_config_request", return_value=file_content)

    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
            auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME: "",
        }
    )
    result = runner.invoke(cli, ["scan", "--config", "p/ci"], env={})
    snapshot.assert_match(result.output, "output.txt")