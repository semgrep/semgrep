from uuid import uuid4

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import __VERSION__
from semgrep.config_resolver import ConfigFile
from semgrep.config_resolver import ConfigLoader
from semgrep.config_resolver import ConfigType
from semgrep.config_resolver import legacy_url_for_scan
from semgrep.config_resolver import PRODUCT_NAMES
from semgrep.constants import DEFAULT_SEMGREP_APP_CONFIG_URL
from semgrep.error import SemgrepError
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
            "https://semgrep.dev/p/ci",
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
            "https://semgrep.dev/p/ci",
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
            "https://semgrep.dev/p/ci",
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
