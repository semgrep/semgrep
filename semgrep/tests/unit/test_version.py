import pytest

from semgrep.app import version


@pytest.mark.quick
def test_version_check_caching(tmp_path, mocker):
    tmp_cache_path = tmp_path / "semgrep_version"

    fetch_mock = mocker.patch.object(
        version, "_fetch_latest_version", return_value={"version": "1.2.3"}
    )

    version.version_check(tmp_cache_path)
    version.version_check(tmp_cache_path)

    assert fetch_mock.call_count == 1
