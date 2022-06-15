import pytest

from semgrep.app import version


@pytest.mark.quick
def test_version_check_caching(tmp_path, mocker, monkeypatch):
    tmp_cache_path = tmp_path / "semgrep_version"

    monkeypatch.setenv("SEMGREP_VERSION_CACHE_PATH", str(tmp_cache_path))
    fetch_mock = mocker.patch.object(
        version, "_fetch_latest_version", return_value={"version": "1.2.3"}
    )

    version.version_check()
    version.version_check()

    assert fetch_mock.call_count == 1
