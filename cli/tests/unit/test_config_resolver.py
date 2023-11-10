import pytest
from semgrep.config_resolver import ConfigLoader, ConfigType


class TestConfigLoader:
    @pytest.mark.parametrize(
        ("product"),
        [
            "code",
            "secrets",
            "supply-chain",
        ],
    )
    @pytest.mark.quick
    def test_product_names(self, product):
        config_loader = ConfigLoader(product)

        assert config_loader._origin == ConfigType.SEMGREP_CLOUD_PLATFORM
        assert config_loader._config_path == product
        assert config_loader._supports_fallback_config is True
