from textwrap import dedent

import pytest
from tests.semgrep_runner import SemgrepRunner

from semgrep.cli import cli
from semgrep.config_resolver import ConfigFile
from semgrep.config_resolver import ConfigLoader
from semgrep.error import SemgrepError


# What is this test really for? The old output was an error due the field
# 'pattern-new-feature' being unknown. The new output is another error
# about a missing 'pattern' or similar field.
@pytest.mark.quick
@pytest.mark.osemfail
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
        },
        use_click_runner=True,
    )
    result = runner.invoke(cli, ["scan", "--config", "p/ci"])
    snapshot.assert_match(result.output, "output.txt")


@pytest.mark.quick
@pytest.mark.osemfail
def test_fallback_config_works(monkeypatch, snapshot, mocker, tmp_path):
    config_file = ConfigFile(
        None,
        "rules: []",
        "https://semgrep.dev/p/ci",
    )
    patched_download = mocker.patch.object(
        ConfigLoader,
        "_download_config_from_url",
        side_effect=[
            SemgrepError(
                f"Failed to download configuration. HTTP 500 when fetching URL"
            ),
            config_file,
        ],
    )

    runner = SemgrepRunner(
        env={
            "SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml"),
            "SEMGREP_APP_TOKEN": "",
        },
        use_click_runner=True,
    )
    result = runner.invoke(cli, ["scan", "--debug", "--config", "supply-chain"])

    assert "https://semgrep.dev/api" in patched_download.call_args_list[0].args[0]
    assert (
        "https://fail-open.prod.semgrep.dev/"
        in patched_download.call_args_list[1].args[0]
    )
    assert "loaded 1 configs" in result.stdout
