from semgrep.git import clean_project_url


def test_git_url_clean():
    assert (
        clean_project_url(
            "https://gitlab-ci-token:glcbt-64_wFuiRFQk9t841JHKQnAT@gitlab.company.world/app/test-case.git"
        )
        == "https://gitlab.company.world/app/test-case.git"
    )
