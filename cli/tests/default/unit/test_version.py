#
# Copyright (c) 2020-2024 Semgrep Inc.
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
