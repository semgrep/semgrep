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
# Testing src/semgrep/git.py
import os
import subprocess

import pytest

from semgrep.error import SemgrepError
from semgrep.git import _scrubbed_git_local_env
from semgrep.git import git_check_output


class TestScrubbedGitLocalEnv:
    @pytest.mark.quick
    def test_removes_known_vars_inside_block(self, monkeypatch):
        monkeypatch.setenv("GIT_INDEX_FILE", ".git/index")
        monkeypatch.setenv("GIT_DIR", ".git")
        monkeypatch.setenv("GIT_WORK_TREE", ".")
        with _scrubbed_git_local_env():
            assert "GIT_INDEX_FILE" not in os.environ
            assert "GIT_DIR" not in os.environ
            assert "GIT_WORK_TREE" not in os.environ

    @pytest.mark.quick
    def test_restores_vars_after_block(self, monkeypatch):
        monkeypatch.setenv("GIT_INDEX_FILE", ".git/index")
        monkeypatch.setenv("GIT_DIR", "/some/abs/.git")
        with _scrubbed_git_local_env():
            pass
        assert os.environ["GIT_INDEX_FILE"] == ".git/index"
        assert os.environ["GIT_DIR"] == "/some/abs/.git"

    @pytest.mark.quick
    def test_restores_vars_even_if_block_raises(self, monkeypatch):
        monkeypatch.setenv("GIT_INDEX_FILE", ".git/index")
        with pytest.raises(RuntimeError):
            with _scrubbed_git_local_env():
                raise RuntimeError("boom")
        assert os.environ["GIT_INDEX_FILE"] == ".git/index"

    @pytest.mark.quick
    def test_leaves_unrelated_env_untouched(self, monkeypatch):
        monkeypatch.setenv("GIT_INDEX_FILE", ".git/index")
        monkeypatch.setenv("GIT_AUTHOR_NAME", "Alice")
        monkeypatch.setenv("GIT_EXEC_PATH", "/usr/lib/git-core")
        with _scrubbed_git_local_env():
            assert "GIT_INDEX_FILE" not in os.environ
            assert os.environ["GIT_AUTHOR_NAME"] == "Alice"
            assert os.environ["GIT_EXEC_PATH"] == "/usr/lib/git-core"

    @pytest.mark.quick
    def test_no_op_when_no_vars_set(self, monkeypatch):
        for var in ("GIT_INDEX_FILE", "GIT_DIR", "GIT_WORK_TREE"):
            monkeypatch.delenv(var, raising=False)
        with _scrubbed_git_local_env():
            assert "GIT_INDEX_FILE" not in os.environ


class TestGitCheckOutputRedaction:
    @pytest.mark.quick
    def test_url_credentials_redacted_in_error_message(self, mocker):
        url = "https://gitlab-ci-token:glpat-LEAK_ME@gitlab.example.com/foo.git"
        mocker.patch(
            "subprocess.check_output",
            side_effect=subprocess.CalledProcessError(
                returncode=128,
                cmd=["git", "fetch", url, "main"],
                stderr="fatal: could not read Username for ...\n",
            ),
        )

        with pytest.raises(SemgrepError) as exc_info:
            git_check_output(["git", "fetch", url, "main"])

        message = str(exc_info.value)
        assert "glpat-LEAK_ME" not in message
        assert "gitlab-ci-token" not in message
        assert "<REDACTED>" in message
        assert "gitlab.example.com/foo.git" in message
        assert "git fetch" in message
