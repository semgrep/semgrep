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
# Testing src/semgrep/settings.py
import uuid

import pytest
from ruamel.yaml import YAML

from semgrep.settings import generate_anonymous_user_id
from semgrep.settings import Settings

yaml = YAML()


@pytest.fixture
def settings_path(tmp_path, monkeypatch):
    path = tmp_path / "settings.yml"
    monkeypatch.setenv("SEMGREP_SETTINGS_FILE", str(path))
    return path


@pytest.mark.quick
def test_env_sourced_token_is_not_persisted(settings_path, monkeypatch):
    env_token = "env-token-abc"
    monkeypatch.setenv("SEMGREP_APP_TOKEN", env_token)

    Settings()

    on_disk = yaml.load(settings_path.read_text())
    assert "api_token" not in on_disk
    # anonymous_user_id is still derived (deterministically) from the env token
    assert on_disk["anonymous_user_id"] == generate_anonymous_user_id(env_token)


@pytest.mark.quick
def test_login_persisted_token_survives_env_set_run(settings_path, monkeypatch):
    login_token = "login-token-xyz"
    with settings_path.open("w") as fd:
        yaml.dump(
            {
                "has_shown_metrics_notification": True,
                "api_token": login_token,
                "anonymous_user_id": str(uuid.uuid4()),
            },
            fd,
        )

    monkeypatch.setenv("SEMGREP_APP_TOKEN", "a-different-env-token")
    Settings()

    on_disk = yaml.load(settings_path.read_text())
    assert on_disk["api_token"] == login_token


@pytest.mark.quick
def test_no_token_means_no_api_token_persisted(settings_path, monkeypatch):
    monkeypatch.delenv("SEMGREP_APP_TOKEN", raising=False)

    Settings()

    on_disk = yaml.load(settings_path.read_text())
    assert "api_token" not in on_disk
    assert "anonymous_user_id" in on_disk
