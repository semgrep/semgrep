from uuid import uuid4

import pytest
import requests

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import __VERSION__
from semgrep.config_resolver import ConfigFile
from semgrep.config_resolver import ConfigLoader
from semgrep.config_resolver import ConfigType
from semgrep.config_resolver import PRODUCT_NAMES
from semgrep.error import SemgrepError
from semgrep.state import SemgrepState

FAKE_USER_AGENT = "user-agent"
API_URL = "https://semgrep.dev"


@pytest.mark.parametrize(
    "product",
    [
        "code",
        "secrets",
        "supply-chain",
        "code,secrets",
    ],
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
    def test__fetch_semgrep_cloud_platform_scan_config(self, config_loader, mocker):
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
        self, config_loader, mocker
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
    def mocked_state(self, mocker):
        mocked = mocker.MagicMock()
        mocked.request_id = uuid4()
        mocked.env.semgrep_url = API_URL
        mocker.patch("semgrep.config_resolver.get_state", return_value=mocked)
        return mocked

    @pytest.fixture
    def mocked_cloud_platform_request(
        self, mocked_state: SemgrepState, config_loader: ConfigLoader
    ) -> out.ScanRequest:
        products = [
            out.Product.from_json(PRODUCT_NAMES[p])
            for p in config_loader._config_path.split(",")
        ]

        request = out.ScanRequest(
            meta=out.RawJson({}),
            scan_metadata=out.ScanMetadata(
                cli_version=out.Version(__VERSION__),
                unique_id=out.Uuid(str(mocked_state.request_id)),
                requested_products=products,
                dry_run=True,
            ),
            project_metadata=config_loader._project_metadata_for_standalone_scan(),
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
        mocked_state,
        mocked_cloud_platform_request: out.ScanRequest,
        mocked_scan_response: out.ScanResponse,
        mocker,
    ):
        mock_response = mocker.MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = mocked_scan_response.to_json()
        mocked_state.app_session.post.return_value = mock_response

        config = config_loader._download_semgrep_cloud_platform_scan_config(
            mocked_cloud_platform_request
        )

        assert config.config_id is None
        assert config.config_path == f"{API_URL}/api/cli/scans"
        assert config.contents == "{}"

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test__download_semgrep_cloud_platform_scan_config_unauthorized(
        self,
        config_loader: ConfigLoader,
        mocked_state,
        mocked_cloud_platform_request: out.ScanRequest,
        mocker,
    ):
        mock_response = mocker.MagicMock()
        mock_response.status_code = 401
        mocked_state.app_session.post.return_value = mock_response

        with pytest.raises(SemgrepError) as exc:
            config_loader._download_semgrep_cloud_platform_scan_config(
                mocked_cloud_platform_request
            )

        assert "Invalid API Key" in str(exc.value)

    @pytest.mark.quick
    @pytest.mark.osemfail
    def test_general_request_exception(
        self,
        mocked_state,
        config_loader: ConfigLoader,
        mocked_cloud_platform_request: out.ScanRequest,
        mocker,
    ):
        # Setup mock response to raise RequestException
        mock_response = mocker.MagicMock()
        mock_response.raise_for_status.side_effect = requests.RequestException("Error")
        mocked_state.app_session.post.return_value = mock_response

        with pytest.raises(Exception) as exc:
            config_loader._download_semgrep_cloud_platform_scan_config(
                mocked_cloud_platform_request
            )

        assert "API server at" in str(exc.value)
