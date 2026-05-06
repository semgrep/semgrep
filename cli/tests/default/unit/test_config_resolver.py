#
# Copyright (c) 2023-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
from uuid import uuid4

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import __VERSION__
from semgrep.config_resolver import ConfigFile
from semgrep.config_resolver import ConfigLoader
from semgrep.config_resolver import ConfigType
from semgrep.config_resolver import legacy_url_for_scan
from semgrep.config_resolver import parse_config_string
from semgrep.config_resolver import PRODUCT_NAMES
from semgrep.constants import DEFAULT_SEMGREP_APP_CONFIG_URL
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import SemgrepError
from semgrep.rule_lang import RpcValidationError
from semgrep.state import SemgrepState

FAKE_USER_AGENT = "user-agent"
API_URL = "https://semgrep.dev"


@pytest.fixture
def mock_env(monkeypatch):
    # Set the environment variable
    monkeypatch.setenv("SEMGREP_REPO_NAME", "test_repo")


@pytest.fixture
def mocked_state(mocker):
    mocked = mocker.MagicMock()
    mocked.local_scan_id = uuid4()
    mocked.env.semgrep_url = API_URL
    mocker.patch("semgrep.config_resolver.get_state", return_value=mocked)
    return mocked


@pytest.fixture
def mocked_rpc_validation_error(mocker):
    mocker.patch(
        "semgrep.rule_lang.run_rpc_validate_exn",
        side_effect=RpcValidationError(
            out.CoreError(
                error_type=out.ErrorType(value=out.InvalidYaml()),
                severity=out.ErrorSeverity(value=out.Warning_()),
                message="mock validation failure",
                details=None,
                location=None,
                rule_id=None,
            )
        ),
    )


@pytest.mark.parametrize(
    "product",
    ["code", "secrets", "supply-chain", "code,secrets", "policy"],
)
class TestConfigLoaderForProducts:
    @pytest.fixture
    def config_loader(self, product) -> ConfigLoader:
        return ConfigLoader(product)

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test_init(self, config_loader, product):
        assert config_loader._origin == ConfigType.SEMGREP_CLOUD_PLATFORM
        assert config_loader._config_path == product
        assert config_loader._supports_fallback_config is True

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test_load_config(self, config_loader, mocker):
        config_file = ConfigFile(
            None,
            "rules: []",
            "https://semgrep.dev/c/p/ci",
        )

        patched_fetch = mocker.patch.object(
            ConfigLoader,
            "_fetch_semgrep_cloud_platform_scan_config",
            return_value=config_file,
        )

        config = config_loader.load_config()

        assert config[0] == config_file
        assert patched_fetch.call_count == 1

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test__fetch_semgrep_cloud_platform_scan_config(
        self,
        config_loader,
        mocker,
        mock_env,
    ):
        config_file = ConfigFile(
            None,
            "rules: []",
            "https://semgrep.dev/c/p/ci",
        )

        patched_download = mocker.patch.object(
            ConfigLoader,
            "_download_semgrep_cloud_platform_scan_config",
            return_value=config_file,
        )

        config = config_loader._fetch_semgrep_cloud_platform_scan_config()

        products = [
            out.Product.from_json(PRODUCT_NAMES[p])
            for p in config_loader._config_path.split(",")
        ]

        assert config == config_file
        assert patched_download.call_count == 1
        assert (
            patched_download.call_args[0][0].scan_metadata.requested_products
            == products
        )

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test__fetch_semgrep_cloud_platform_scan_config__fallback(
        self, config_loader, mocker, mock_env
    ):
        config_file = ConfigFile(
            None,
            "rules: []",
            "https://semgrep.dev/c/p/ci",
        )

        patched_download = mocker.patch.object(
            ConfigLoader,
            "_download_semgrep_cloud_platform_scan_config",
            side_effect=[
                SemgrepError(
                    "Failed to download configuration. HTTP 500 when fetching URL"
                ),
            ],
        )

        patched_fallback_download = mocker.patch.object(
            ConfigLoader,
            "_download_semgrep_cloud_platform_fallback_scan_config",
            return_value=config_file,
        )

        config = config_loader._fetch_semgrep_cloud_platform_scan_config()

        assert config == config_file
        assert patched_download.call_count == 1
        assert patched_fallback_download.call_count == 1

    @pytest.fixture
    def mocked_scan_request(self, config_loader: ConfigLoader) -> out.ScanRequest:
        products = [
            out.Product.from_json(PRODUCT_NAMES[p])
            for p in config_loader._config_path.split(",")
        ]

        request = out.ScanRequest(
            scan_metadata=out.ScanMetadata(
                cli_version=out.Version(__VERSION__),
                unique_id=out.Uuid(str(uuid4())),
                requested_products=products,
                dry_run=True,
            ),
            project_metadata=config_loader._project_metadata_for_standalone_scan(
                require_repo_name=False
            ),
        )

        return request

    @pytest.fixture
    def mocked_scan_response(self, config_loader) -> out.ScanResponse:
        scan_info = out.ScanInfo(
            enabled_products=[
                out.Product.from_json(PRODUCT_NAMES[p])
                for p in config_loader._config_path.split(",")
            ],
            deployment_id=1,
            deployment_name="r2c",
        )
        scan_config = out.ScanConfiguration(rules=out.RawJson(value={}))
        engine_config = out.EngineConfiguration()

        return out.ScanResponse(
            info=scan_info,
            config=scan_config,
            engine_params=engine_config,
        )

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test__download_semgrep_cloud_platform_scan_config_success(
        self,
        config_loader: ConfigLoader,
        mocked_scan_request: out.ScanRequest,
        mocked_scan_response: out.ScanResponse,
        requests_mock,
    ):
        requests_mock.post(
            "https://semgrep.dev/api/cli/scans", json=mocked_scan_response.to_json()
        )

        config = config_loader._download_semgrep_cloud_platform_scan_config(
            mocked_scan_request
        )

        assert config.config_id is None
        assert config.config_path == f"{API_URL}/api/cli/scans"
        assert config.contents == "{}"

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test__download_semgrep_cloud_platform_scan_config_unauthorized(
        self,
        config_loader: ConfigLoader,
        mocked_scan_request: out.ScanRequest,
        requests_mock,
    ):
        requests_mock.post(
            "https://semgrep.dev/api/cli/scans",
            status_code=401,
        )

        with pytest.raises(SemgrepError) as exc:
            config_loader._download_semgrep_cloud_platform_scan_config(
                mocked_scan_request
            )

        assert "Invalid API Key" in str(exc.value)

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test__project_metadata_for_standalone_scan(
        self, config_loader: ConfigLoader, monkeypatch
    ):
        monkeypatch.setenv("SEMGREP_REPO_NAME", "test_repo")
        metadata = config_loader._project_metadata_for_standalone_scan(
            require_repo_name=True
        )
        assert isinstance(metadata, out.ProjectMetadata)
        assert metadata.repository == "test_repo"

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test__project_metadata_for_standalone_scan__no_repo_throws(
        self, config_loader: ConfigLoader, monkeypatch
    ):
        monkeypatch.delenv("SEMGREP_REPO_NAME", raising=False)
        with pytest.raises(SemgrepError):
            config_loader._project_metadata_for_standalone_scan(require_repo_name=True)

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test__project_metadata_for_standalone_scan__no_repo_success(
        self, config_loader: ConfigLoader, monkeypatch
    ):
        monkeypatch.delenv("SEMGREP_REPO_NAME", raising=False)
        metadata = config_loader._project_metadata_for_standalone_scan(
            require_repo_name=False
        )
        assert metadata.repository == "unknown"


@pytest.mark.quick
@pytest.mark.osemfail
@pytest.mark.parametrize(
    "extra_params, repo_name, expected_url",
    [
        (
            {},
            None,
            f"{API_URL}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?dry_run=True&full_scan=True&semgrep_version={__VERSION__}",
        ),
        (
            {"sca": True},
            None,
            f"{API_URL}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?dry_run=True&full_scan=True&semgrep_version={__VERSION__}&sca=True",
        ),
        (
            {},
            "example_repo",
            f"{API_URL}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?dry_run=True&full_scan=True&semgrep_version={__VERSION__}&repo_name=example_repo",
        ),
    ],
)
def test_legacy_url_for_scan(
    mocked_state: SemgrepState, mocker, extra_params, repo_name, expected_url
):
    if repo_name:
        mocker.patch("os.environ", {"SEMGREP_REPO_NAME": repo_name})

    assert legacy_url_for_scan(extra_params) == expected_url


@pytest.mark.quick
@pytest.mark.osemfail
def test_parse_config_string_jsonschema_fallback(mocked_rpc_validation_error):
    """
    Test that when RPC validation fails the fallback to jsonschema validation works correctly.
    """

    rule_config = """{
        "rules": [
            {
                "id": "test-rule",
                "message": "Test rule with emoji 🔥",
                "languages": ["python"],
                "severity": "WARNING",
                "pattern": "$X"
            }
        ]
    }"""

    result = parse_config_string("test", rule_config, None)
    assert len(result.rules) == 1
    assert result.rules[0].id == "test-rule"
    assert len(result.errors) == 0


@pytest.mark.quick
@pytest.mark.osemfail
def test_parse_config_string_as_rules_no_surrogate_pairs_in_rules_file(mocker):
    """
    Rules whose original source contains characters above U+FFFF (e.g. emoji)
    must reach semgrep-core in a form its parser can handle. For JSON input,
    we write the content with a .json suffix so semgrep-core's JSON parser
    (which handles \\uXXXX surrogate escapes natively) runs. For YAML input,
    we write it with a .yaml suffix so the YAML parser runs (which reads the
    raw UTF-8 bytes). Either way, RPC validation should succeed without a
    fallback to Python's jsonschema.
    """
    import semgrep.rule_lang

    spy = mocker.spy(semgrep.rule_lang, "run_rpc_validate_exn")

    # Input contains a surrogate pair (valid JSON for U+1F6AB 🚫)
    rule_config = """{
        "rules": [
            {
                "id": "emoji-rule",
                "message": "Test rule with non-BMP character",
                "languages": ["generic"],
                "severity": "WARNING",
                "pattern-regex": "(?:\\u274c|\\ud83d\\udeab|foo)"
            }
        ]
    }"""

    result = parse_config_string("test-config", rule_config, "rules.json")

    assert len(result.rules) == 1
    assert result.rules[0].id == "emoji-rule"
    assert len(result.errors) == 0
    # Verify semgrep-core accepted the rules file directly, rather than
    # falling back to Python's jsonschema validation.
    spy.assert_called_once()
    assert spy.spy_exception is None


@pytest.mark.quick
@pytest.mark.osemfail
def test_yaml_schema_error_points_to_correct_rule(mocked_rpc_validation_error):
    """Schema validation errors for YAML rules should point at the failing
    rule's location in the YAML file, not at 'a: b' / None."""

    yaml_contents = """\
rules:
  - id: missing-severity
    pattern: $X
    message: oops
    languages: [python]
"""

    with pytest.raises(InvalidRuleSchemaError) as exc_info:
        parse_config_string("test-config", yaml_contents, "rules.yaml")

    err = exc_info.value
    assert len(err.spans) == 1
    span = err.spans[0]
    assert span.file == "rules.yaml"
    # The span should point at the rule node (line 2: "- id: missing-severity")
    assert span.start.line == 2


@pytest.mark.quick
@pytest.mark.osemfail
def test_yaml_schema_error_picks_correct_rule_among_many(mocked_rpc_validation_error):
    """When multiple rules exist, the error span should point at the specific
    rule that failed validation, not the first one."""

    yaml_contents = """\
rules:
  - id: good-rule
    pattern: $X == $X
    message: dupe
    languages: [python]
    severity: WARNING
  - id: bad-rule
    pattern: $Y
    message: oops
    languages: [python]
"""

    with pytest.raises(InvalidRuleSchemaError) as exc_info:
        parse_config_string("test-config", yaml_contents, "rules.yaml")

    span = exc_info.value.spans[0]
    assert span.file == "rules.yaml"
    # bad-rule starts at line 7
    assert span.start.line == 7


@pytest.mark.quick
@pytest.mark.osemfail
def test_json_schema_error_shows_filename(mocked_rpc_validation_error):
    """JSON config schema errors should show the correct filename even though
    there are no YAML spans."""

    json_contents = '{"rules": [{"id": "bad", "pattern": "$X", "message": "m", "languages": ["python"]}]}'

    with pytest.raises(InvalidRuleSchemaError) as exc_info:
        parse_config_string("test-config", json_contents, "rules.json")

    span = exc_info.value.spans[0]
    assert span.file == "rules.json"
    # No rule_spans for JSON, falls back to whole-file span at line 1
    assert span.start.line == 1


@pytest.mark.quick
@pytest.mark.osemfail
def test_yaml_schema_error_message_content(mocked_rpc_validation_error):
    """The error message itself should still describe the actual problem."""

    yaml_contents = """\
rules:
  - id: no-severity
    pattern: $X
    message: oops
    languages: [python]
"""

    with pytest.raises(InvalidRuleSchemaError) as exc_info:
        parse_config_string("test-config", yaml_contents, "rules.yaml")

    assert exc_info.value.long_msg is not None
    assert "severity" in exc_info.value.long_msg.lower()
