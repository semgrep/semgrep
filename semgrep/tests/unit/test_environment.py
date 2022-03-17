import subprocess

import pytest

from semgrep.meta import GitMeta

REPO_DIR_NAME = "project_name"
AUTHOR_EMAIL = "test_environment@test.r2c.dev"
AUTHOR_NAME = "Environment Test"
BRANCH_NAME = "some/branch-name"
COMMIT_MESSAGE = "some: commit message! foo"


@pytest.fixture
def git_tmp_path_with_commit(monkeypatch, tmp_path):
    """
    Initialize a git repo at a temporary directory and add single
    dummy commit
    """
    repo_base = tmp_path / REPO_DIR_NAME
    repo_base.mkdir()

    monkeypatch.chdir(repo_base)

    # Initialize State
    subprocess.run(
        ["git", "init"], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    subprocess.run(
        ["git", "config", "user.email", AUTHOR_EMAIL],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    subprocess.run(
        ["git", "config", "user.name", AUTHOR_NAME],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    subprocess.run(
        ["git", "checkout", "-B", BRANCH_NAME],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )

    foo = repo_base / "foo.py"
    foo.write_text(f"x = 1\n")
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    subprocess.run(
        ["git", "commit", "-m", COMMIT_MESSAGE],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )

    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    yield (repo_base, base_commit)


def test_local_git_repo(git_tmp_path_with_commit):
    """
    Reads env var if exists otherwise uses git to find branch name
    """
    repo_base, base_commit = git_tmp_path_with_commit

    assert GitMeta().to_dict() == {
        "repository": REPO_DIR_NAME,
        "repo_url": None,
        "branch": BRANCH_NAME,
        "ci_job_url": None,
        "commit": base_commit,
        "commit_author_email": AUTHOR_EMAIL,
        "commit_author_name": AUTHOR_NAME,
        "commit_author_username": None,
        "commit_author_image_url": None,
        "commit_title": COMMIT_MESSAGE,
        "on": "unknown",
        "pull_request_author_username": None,
        "pull_request_author_image_url": None,
        "pull_request_id": None,
        "pull_request_title": None,
        "scan_environment": "git",
        "is_full_scan": True,
    }


def test_github_git_repo(git_tmp_path_with_commit):
    repo_base, base_commit = git_tmp_path_with_commit
    # import json
    # print(json.dumps(GitMeta().to_dict(), indent=4))
    # print(json.dumps(GithubMeta().to_dict(), indent=4))
    # assert False
    assert GitMeta().to_dict() == {
        "repository": REPO_DIR_NAME,
        "repo_url": None,
        "branch": BRANCH_NAME,
        "ci_job_url": None,
        "commit": base_commit,
        "commit_author_email": AUTHOR_EMAIL,
        "commit_author_name": AUTHOR_NAME,
        "commit_author_username": None,
        "commit_author_image_url": None,
        "commit_title": COMMIT_MESSAGE,
        "on": "unknown",
        "pull_request_author_username": None,
        "pull_request_author_image_url": None,
        "pull_request_id": None,
        "pull_request_title": None,
        "scan_environment": "git",
        "is_full_scan": True,
    }
    # {
    #     "repository": "[unknown]",
    #     "repo_url": "https://github.com/[unknown]",
    #     "branch": null,
    #     "ci_job_url": null,
    #     "commit": base_commit,
    #     "commit_author_email": AUTHOR_EMAIL,
    #     "commit_author_name": AUTHOR_NAME,
    #     "commit_author_username": null,
    #     "commit_author_image_url": null,
    #     "commit_title": COMMIT_MESSAGE,
    #     "on": "unknown",
    #     "pull_request_author_username": null,
    #     "pull_request_author_image_url": null,
    #     "pull_request_id": null,
    #     "pull_request_title": null,
    #     "scan_environment": "github-actions",
    #     "is_full_scan": true,
    # }
