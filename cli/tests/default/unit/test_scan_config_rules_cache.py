#
# Copyright (c) 2026 Semgrep Inc.
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
import json
import uuid
import zlib
from time import time

import pytest

import semgrep.app.scan_config_rules_cache as scan_config_rules_cache
import semgrep.semgrep_interfaces.semgrep_output_v1 as out

pytestmark = [pytest.mark.quick, pytest.mark.no_semgrep_cli]

SEMGREP_URL = "https://semgrep.dev"
DEPLOYMENT_ID = 1
CACHE = scan_config_rules_cache.SCAN_CONFIG_RULES_CACHE


@pytest.fixture(autouse=True)
def isolate_rules_cache_dir(monkeypatch, tmp_path):
    monkeypatch.setattr(scan_config_rules_cache, "RULES_CACHE_DIR", tmp_path)


def _setup(
    mocker,
    repository: str = "repo",
    semgrep_url: str = SEMGREP_URL,
    deployment_id: int = DEPLOYMENT_ID,
):
    project_metadata = mocker.MagicMock()
    project_metadata.to_json.return_value = {"repository": repository}

    ci_config = mocker.MagicMock()
    ci_config.to_json.return_value = {"ignored_paths": []}

    project_config = mocker.MagicMock()
    project_config.to_CiConfigFromRepo.return_value = ci_config

    scan_metadata = out.ScanMetadata(
        cli_version=out.Version("0.0.0"),
        unique_id=out.Uuid(str(uuid.uuid4())),
        requested_products=[],
        dry_run=False,
    )

    return scan_metadata, semgrep_url, deployment_id, project_metadata, project_config


def _get_meta_path(cache_path):
    return cache_path.with_suffix("").with_suffix(".meta.json")


def _get_cache_key(setup):
    return CACHE._get_cache_key(*setup)


def _get_cache_path(setup):
    cache_path = _maybe_get_cache_path(setup, cache_rules=True)
    assert cache_path is not None
    return cache_path


def _maybe_get_cache_path(
    setup,
    *,
    cache_rules=True,
):
    return CACHE._get_cache_path(*setup, cache_rules=cache_rules)


def _get_cached_rules(
    setup,
    *,
    cache_rules=True,
):
    return CACHE.get_cached_rules(*setup, cache_rules=cache_rules)


def _write_cached_rules(
    rules,
    setup,
    *,
    cache_rules=True,
):
    CACHE.write_cached_rules(rules, *setup, cache_rules=cache_rules)


def _write_cache_files(
    rules_bytes,
    setup,
    *,
    meta=None,
):
    cache_path = _get_cache_path(setup)
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    cache_path.write_bytes(rules_bytes)
    if meta is None:
        meta = json.dumps({"expires_at": time() + 60})
    if meta is not False:
        _get_meta_path(cache_path).write_text(meta)


def _assert_cache_miss(setup):
    assert _get_cached_rules(setup) is None


def _assert_cache_hit(rules, setup):
    assert _get_cached_rules(setup) == rules


def test_cache_key_excludes_scan_unique_id(mocker):
    assert _get_cache_key(_setup(mocker)) == _get_cache_key(_setup(mocker))


def test_cache_key_is_short(mocker):
    # Keep local cache filenames readable; collisions only cause a best-effort
    # stale rule cache hit, which expires quickly and falls back to rule download.
    assert len(_get_cache_key(_setup(mocker))) == 8


def test_cache_key_uses_project_metadata(mocker):
    assert _get_cache_key(_setup(mocker, repository="repo-a")) != _get_cache_key(
        _setup(mocker, repository="repo-b")
    )


def test_cache_key_uses_semgrep_url(mocker):
    assert _get_cache_key(_setup(mocker)) != _get_cache_key(
        _setup(mocker, semgrep_url="https://localhost:3000")
    )


def test_cache_key_uses_deployment_id(mocker):
    assert _get_cache_key(_setup(mocker, deployment_id=1)) != _get_cache_key(
        _setup(mocker, deployment_id=2)
    )


@pytest.mark.parametrize("deployment_id", [2, 3])
def test_get_does_not_read_cache_for_different_deployment(mocker, deployment_id):
    rules = out.RawJson({"rules": [{"id": "cached-rule"}]})
    _write_cached_rules(rules, _setup(mocker, deployment_id=1))

    _assert_cache_miss(_setup(mocker, deployment_id=deployment_id))


def test_write_and_get_reads_fresh_cache_file(mocker):
    setup = _setup(mocker)
    rules = out.RawJson({"rules": [{"id": "cached-rule"}]})

    _write_cached_rules(rules, setup)

    cache_path = _get_cache_path(setup)
    assert cache_path.name.endswith(".json.zlib")
    assert cache_path.read_bytes() != rules.to_json_string().encode("utf-8")
    assert (
        zlib.decompress(cache_path.read_bytes()).decode("utf-8")
        == rules.to_json_string()
    )
    assert _get_meta_path(cache_path).exists()
    _assert_cache_hit(rules, setup)


def test_get_ignores_expired_cache_file(mocker, monkeypatch):
    setup = _setup(mocker)
    monkeypatch.setattr(scan_config_rules_cache, "RULES_CACHE_TTL_SECONDS", -1)
    rules = out.RawJson({"rules": [{"id": "cached-rule"}]})

    _write_cached_rules(rules, setup)

    _assert_cache_miss(setup)


def test_get_ignores_bad_cache_shape(mocker):
    setup = _setup(mocker)
    _write_cache_files(
        zlib.compress(json.dumps({"rules": {}}).encode("utf-8")),
        setup,
    )
    _assert_cache_miss(setup)


def test_get_ignores_bad_json(mocker):
    setup = _setup(mocker)
    _write_cache_files(
        zlib.compress(b"{"),
        setup,
    )
    _assert_cache_miss(setup)


def test_get_ignores_invalid_zlib(mocker):
    setup = _setup(mocker)
    _write_cache_files(
        b"not zlib",
        setup,
    )
    _assert_cache_miss(setup)


@pytest.mark.parametrize(
    "meta",
    [
        pytest.param(None, id="missing-meta"),
        pytest.param("{", id="bad-meta-json"),
        pytest.param(json.dumps({}), id="missing-expires-at"),
        pytest.param(json.dumps({"expires_at": "soon"}), id="bad-expires-at"),
    ],
)
def test_get_ignores_bad_metadata(mocker, meta):
    setup = _setup(mocker)
    _write_cache_files(
        zlib.compress(json.dumps({"rules": [{"id": "cached-rule"}]}).encode("utf-8")),
        setup,
        meta=False if meta is None else meta,
    )
    _assert_cache_miss(setup)


def test_write_overwrites_corrupt_cache_file(mocker):
    setup = _setup(mocker)
    _write_cache_files(
        zlib.compress(b"{"),
        setup,
    )
    _assert_cache_miss(setup)

    rules = out.RawJson({"rules": [{"id": "cached-rule"}]})
    _write_cached_rules(rules, setup)

    _assert_cache_hit(rules, setup)


def test_write_ignores_filesystem_errors(mocker, monkeypatch, tmp_path):
    setup = _setup(mocker)
    cache_dir = tmp_path / ".semgrep_rules"
    cache_dir.write_text("not a directory")
    monkeypatch.setattr(scan_config_rules_cache, "RULES_CACHE_DIR", cache_dir)
    rules = out.RawJson({"rules": [{"id": "cached-rule"}]})

    _write_cached_rules(rules, setup)

    assert cache_dir.is_file()
    _assert_cache_miss(setup)


def test_rules_file_without_metadata_is_not_read_after_metadata_write_fails(
    mocker, monkeypatch
):
    setup = _setup(mocker)
    cache_path = _get_cache_path(setup)
    meta_path = _get_meta_path(cache_path)
    original_write_text = type(meta_path).write_text

    def fail_meta_write(path, *args, **kwargs):
        if path == meta_path:
            raise OSError("failed to write metadata")
        return original_write_text(path, *args, **kwargs)

    monkeypatch.setattr(type(meta_path), "write_text", fail_meta_write)
    rules = out.RawJson({"rules": [{"id": "cached-rule"}]})

    _write_cached_rules(rules, setup)

    assert cache_path.exists()
    assert not meta_path.exists()
    _assert_cache_miss(setup)


def test_disabled_cache_does_not_read_or_write(mocker, monkeypatch, tmp_path):
    setup = _setup(mocker)
    cache_dir = tmp_path / ".semgrep_rules"
    monkeypatch.setattr(scan_config_rules_cache, "RULES_CACHE_DIR", cache_dir)
    rules = out.RawJson({"rules": [{"id": "cached-rule"}]})

    _write_cached_rules(
        rules,
        setup,
        cache_rules=False,
    )

    assert _maybe_get_cache_path(setup, cache_rules=False) is None
    assert _get_cached_rules(setup, cache_rules=False) is None
    assert not cache_dir.exists()
