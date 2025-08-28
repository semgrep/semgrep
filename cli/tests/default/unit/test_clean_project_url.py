#
# Copyright (c) 2024 Semgrep Inc.
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
from unittest.mock import patch

import pytest

from semgrep.git import clean_project_url
from semgrep.git import get_project_url


@pytest.mark.quick
def test_git_url_clean():
    assert (
        clean_project_url(
            "https://gitlab-ci-token:glcbt-64_wFuiRFQk9t841JHKQnAT@gitlab.company.world/app/test-case.git"
        )
        == "https://gitlab.company.world/app/test-case.git"
    )


@pytest.mark.quick
@patch("semgrep.git.clean_project_url")
def test_get_project_url(patched_clean_project_url):
    get_project_url()
    patched_clean_project_url.assert_called_once()
