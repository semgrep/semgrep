import pytest

from semgrep.git import get_project_url


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
@pytest.mark.osemfail
def test_git_url_clean(mocker):
    mocker.patch(
        "semgrep.git.git_check_output",
        return_value="https://gitlab-ci-token:glcbt-64_wFuiRFQk9t841JHKQnAT@gitlab.company.world/app/test-case.git",
    )

    assert get_project_url() == "https://gitlab.company.world/app/test-case.git"
