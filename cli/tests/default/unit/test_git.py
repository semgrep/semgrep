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
import subprocess

import pytest

from semgrep.error import SemgrepError
from semgrep.git import git_check_output


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
