import json
from textwrap import dedent

import pytest
from tests.semgrep_runner import SemgrepRunner

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.cli import cli
from semgrep.config_resolver import ConfigFile
from semgrep.config_resolver import ConfigLoader


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
    result = runner.invoke(cli, subcommand="scan", args=["--config", "p/ci"])
    snapshot.assert_match(result.output, "output.txt")


def json_callback(request, context):
    body = json.loads(request.text)
    requested_products = body["scan_metadata"]["requested_products"]
    enabled_products = [out.Product.from_json(p) for p in requested_products]

    scan_info = out.ScanInfo(
        enabled_products=enabled_products,
        deployment_id=1,
        deployment_name="r2c",
    )
    scan_config = out.ScanConfiguration(rules=out.RawJson(value={"rules": []}))
    engine_config = out.EngineConfiguration()

    return out.ScanResponse(
        info=scan_info,
        config=scan_config,
        engine_params=engine_config,
    ).to_json()


@pytest.mark.quick
@pytest.mark.osemfail
def test_fallback_config_works(requests_mock, mocker, tmp_path):
    requests_mock.post("https://semgrep.dev/api/cli/scans", status_code=401)

    patched_fallback_download = mocker.patch.object(
        ConfigLoader,
        "_download_config_from_url",
        return_value=ConfigFile(
            None,
            "rules: []",
            "https://semgrep.dev/p/ci",
        ),
    )

    runner = SemgrepRunner(
        env={
            "SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml"),
            "SEMGREP_APP_TOKEN": "",
        },
        use_click_runner=True,
    )
    result = runner.invoke(
        cli, subcommand="scan", args=["--debug", "--config", "supply-chain"]
    )

    assert (
        "https://fail-open.prod.semgrep.dev/"
        in patched_fallback_download.call_args_list[0].args[0]
    )
    assert "loaded 1 configs" in result.stdout


@pytest.mark.parametrize(
    ("cli_option"),
    [
        "supply-chain",
        "secrets",
        "code",
        "code,secrets",
    ],
)
@pytest.mark.quick
@pytest.mark.osemfail
def test_cloud_platform_scan_config(requests_mock, cli_option, tmp_path):
    requests_mock.post("https://semgrep.dev/api/cli/scans", json=json_callback)

    runner = SemgrepRunner(
        env={
            "SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml"),
            "SEMGREP_APP_TOKEN": "",
            "SEMGREP_REPO_NAME": "test-repo",
        },
        use_click_runner=True,
    )
    result = runner.invoke(
        cli, subcommand="scan", args=["--debug", "--config", cli_option]
    )

    assert "loaded 1 configs" in result.stdout
