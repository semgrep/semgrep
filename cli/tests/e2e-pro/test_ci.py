##############################################################################
# Prelude
##############################################################################
# Testing 'semgrep ci' "end-to-end".
#
# TODO: most of the tests in this file rely on use_click_runner=True
# because of some mocking and monkeypatching. Thus, this is this not
# a real e2e test because cli/bin/semgrep is not invoked.
# Try to use environment variables instead of Python monkey patching
# so that those tests can also pass with osemgrep.
import json
import re
import shutil
import subprocess
import sys
from collections import defaultdict
from pathlib import Path
from textwrap import dedent
from typing import List

import pytest
from requests.exceptions import ConnectionError
from ruamel.yaml import YAML
from tests.conftest import make_semgrepconfig_file
from tests.conftest import str_containing
from tests.e2e.test_baseline import _git_commit
from tests.e2e.test_baseline import _git_merge
from tests.fixtures import RunSemgrep

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import __VERSION__
from semgrep.app.scans import ScanCompleteResult
from semgrep.app.scans import ScanHandler
from semgrep.engine import EngineType
from semgrep.error_handler import ErrorHandler
from semgrep.meta import GithubMeta
from semgrep.meta import GitlabMeta
from semgrep.meta import GitMeta
from semgrep.metrics import Metrics

##############################################################################
# Constants
##############################################################################

pytestmark = pytest.mark.kinda_slow

REPO_ORG_NAME = "org_name"
REPO_DIR_NAME = "project_name"
AUTHOR_EMAIL = "test_environment@test.r2c.dev"
AUTHOR_NAME = "Environment Test"
BRANCH_NAME = "some/branch-name"
MAIN_BRANCH_NAME = "main"
COMMIT_MESSAGE = "some: commit message! foo"
COMMIT_MESSAGE_2 = "Some other commit/ message"
REMOTE_REPO_URL = "git@github.com:example/fake.git"
DEPLOYMENT_ID = 33
BAD_CONFIG = dedent(
    """
    rules:
    - id: eqeq-bad
      message: "useless comparison"
      languages: [python]
      severity: ERROR
      foo: bar
"""
).lstrip()
FROZEN_ISOTIMESTAMP = "1970-01-01T00:00:00"


# To ensure our tests are as accurate as possible, lets try to autodetect what GITHUB_ vars
# the app code uses, so the tests can enforce the env is mocked appropriately.
_cli_src = (Path(__file__).parent.parent.parent / "src").resolve()
USED_GITHUB_VARS = set(
    subprocess.run(
        f"git grep --recurse-submodules -hPo 'GITHUB_[\\w_]*' {_cli_src}",
        shell=True,
        capture_output=True,
        check=True,
    )
    .stdout.decode()
    .strip()
    .split("\n")
) - {
    "GITHUB_TOKEN",  # not used in the cli, just passed to the backend
    "GITHUB_EVENT_PATH",  # TODO: mock this for more than just PR events
    "GITHUB_xxx",  # not used, just an example in comments
}

assert "GITHUB_ACTIONS" in USED_GITHUB_VARS  # ensure the parsing did something

# And then mock the baseline github env that is shared for all event types.
DEFAULT_GITHUB_VARS = {
    "GITHUB_ACTIONS": "true",
    "GITHUB_ACTOR": "some_test_username",
    "GITHUB_API_URL": "https://api.github.com",
    "GITHUB_REPOSITORY": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
    "GITHUB_RUN_ID": "35",
    "GITHUB_SERVER_URL": "https://github.com",
    "GITHUB_WORKSPACE": "/home/runner/work/actions-test/actions-test",
    "GITHUB_REPOSITORY_ID": "4",
    "GITHUB_REPOSITORY_OWNER_ID": "2",
}

##############################################################################
# Fixtures
##############################################################################


@pytest.fixture
def git_tmp_path_with_commit(monkeypatch, tmp_path, mocker):
    """
    Initialize a git repo at a temp directory with one dummy commit.
    """
    repo_base = tmp_path / REPO_DIR_NAME
    repo_base.mkdir()

    monkeypatch.chdir(repo_base)

    # Initialize State
    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "config", "user.email", AUTHOR_EMAIL],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "config", "user.name", AUTHOR_NAME],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "checkout", "-B", MAIN_BRANCH_NAME],
        check=True,
        capture_output=True,
    )

    foo = repo_base / "foo.py"
    foo.write_text(f"x = 1\n")

    unknown_ext = repo_base / "xyz.txt"
    unknown_ext.write_text("xyz")

    lockfile1 = repo_base / "poetry.lock"
    lockfile1.write_text(
        dedent(
            """\
    [[package]]
    name = "badlib"
    version = "99.99.99"
    description = "it's bad"
    category = "dev"
    optional = false
    python-versions = ">=3.7"

    [[package]]
    name = "mypy"
    version = "0.950"
    description = "Optional static typing for Python"
    category = "dev"
    optional = false
    python-versions = ">=3.6"

    [[package]]
    name = "python-dateutil"
    version = "2.8.2"
    description = "Extensions to the standard Python datetime module"
    category = "main"
    optional = false
    python-versions = "!=3.0.*,!=3.1.*,!=3.2.*,>=2.7"
    """
        )
    )

    lockfile2 = repo_base / "yarn.lock"
    lockfile2.write_text(
        dedent(
            """\
    # THIS IS AN AUTOGENERATED FILE. DO NOT EDIT THIS FILE DIRECTLY.
    # yarn lockfile v1


    lodash@4.17.20:
      version "4.17.20"
      resolved "https://registry.yarnpkg.com/lodash/-/lodash-4.17.18.tgz#5c5f072c5c02f386378dd3f6325b529376210427"
      integrity sha512-au4L1q0HKcaaa37qOdpWWhwzDnB/taYJfRiKULnaT+Ml9UaBIjJ2SOJMeLtSeeLT+zUdyFMm0+ts+j4eeuUpIA==
    """
        )
    )

    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", COMMIT_MESSAGE],
        check=True,
        capture_output=True,
    )

    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    subprocess.run(
        ["git", "checkout", "-B", BRANCH_NAME],
        check=True,
        capture_output=True,
    )

    shutil.copy(
        Path(__file__).parent.parent / "e2e" / "targets" / "ci" / "foo.py",
        repo_base / "foo.py",
    )
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", COMMIT_MESSAGE_2],
        check=True,
        capture_output=True,
    )
    head_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    mocker.patch.object(
        GitlabMeta, "_fetch_branch_get_merge_base", return_value=base_commit
    )

    mocker.patch.object(GithubMeta, "_shallow_fetch_branch", return_value=None)

    repo_copy_base = tmp_path / "checkout_project_name"
    repo_copy_base.mkdir()
    monkeypatch.chdir(repo_copy_base)
    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "remote", "add", "origin", repo_base],
        check=True,
        capture_output=True,
    )
    subprocess.run(["git", "fetch", "origin"])
    subprocess.run(["git", "checkout", f"{MAIN_BRANCH_NAME}"])
    subprocess.run(["git", "checkout", f"{BRANCH_NAME}"])
    subprocess.run(
        ["git", "config", "user.email", AUTHOR_EMAIL],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "config", "user.name", AUTHOR_NAME],
        check=True,
        capture_output=True,
    )

    yield (repo_copy_base, base_commit, head_commit)


@pytest.fixture
def scan_config():
    return dedent(
        """
        rules:
        - id: eqeq-bad
          pattern: $X == $X
          message: "useless comparison"
          languages: [python]
          severity: ERROR
          metadata:
            source: https://semgrep.dev/r/eqeq-bad
        - id: eqeq-five
          pattern: $X == 5
          message: "useless comparison to 5"
          languages: [python]
          severity: ERROR
          metadata:
            dev.semgrep.actions: []
            source: https://semgrep.dev/r/eqeq-five
            semgrep.dev:
                rule:
                    rule_id: "abcd"
                    version_id: "version1"
                    url: "https://semgrep.dev/r/python.eqeq-five"
                    shortlink: "https://sg.run/abcd"
                src: unchanged
          fix: $X == 2
        - id: eqeq-four
          pattern: $X == 4
          message: "useless comparison to 4"
          languages: [python]
          severity: ERROR
          metadata:
            dev.semgrep.actions: ["block"]
            source: https://semgrep.dev/r/eqeq-four
            semgrep.dev:
                rule:
                    rule_id: abce
                    version_id: version2
                    url: "https://semgrep.dev/r/python.eqeq-five"
                    shortlink: "https://sg.run/abcd"
                src: new-version
        - id: abceversion1
          pattern: $X == 4
          message: "useless comparison to 4 (old version)"
          languages: [python]
          severity: ERROR
          metadata:
            dev.semgrep.actions: []
            source: https://semgrep.dev/r/abceversion1
            semgrep.dev:
                rule:
                    rule_id: abce
                    version_id: version1
                    url: "https://semgrep.dev/r/python.eqeq-five"
                    shortlink: "https://sg.run/abcd"
                    rule_name: eqeq-four
                src: previous-scan
        - id: taint-test
          message: "unsafe use of danger"
          languages: [python]
          severity: WARNING
          mode: taint
          pattern-sources:
            - pattern: danger
          pattern-sinks:
            - pattern: sink($X)
          metadata:
            dev.semgrep.actions: ["block"]
            source: https://semgrep.dev/r/taint-test
            semgrep.dev:
                rule:
                    rule_id: abcf
                    version_id: version1
                    url: "https://semgrep.dev/r/python.eqeq-five"
                    shortlink: "https://sg.run/abcd"
                src: new-rule
        - id: supply-chain1
          message: "found a dependency"
          languages: [python]
          severity: ERROR
          r2c-internal-project-depends-on:
            namespace: pypi
            package: badlib
            version: == 99.99.99
          metadata:
            dev.semgrep.actions: []
            source: https://semgrep.dev/-/advisories/supply-chain1
            sca-kind: upgrade-only
        - id: supply-chain2
          message: "found a dependency"
          languages: [js]
          severity: ERROR
          r2c-internal-project-depends-on:
            namespace: npm
            package: badlib
            version: == 99.99.99
          metadata:
            dev.semgrep.actions: []
            source: https://semgrep.dev/-/advisories/supply-chain2
            sca-kind: upgrade-only
        - id: supply-chain3
          message: "found another dependency but its a bad one >:D"
          languages: [js]
          severity: ERROR
          r2c-internal-project-depends-on:
            namespace: npm
            package: verbadlib
            version: == 99.99.99
          metadata:
            dev.semgrep.actions: ["block"]
            source: https://semgrep.dev/-/advisories/supply-chain3
            sca-kind: reachable
        """
    ).lstrip()


@pytest.fixture(autouse=True)
def automocks(mocker, mock_ci_api):
    """
    Necessary patches to run `semgrep ci` tests
    """
    mocker.patch.object(
        GitMeta,
        "commit_timestamp",
        FROZEN_ISOTIMESTAMP,
    )


@pytest.fixture
def mocked_scan_id() -> int:
    return 12345


@pytest.fixture
def mocked_task_id() -> str:
    return "00000000-0000-0000-0000-000000000000"


@pytest.fixture
def enable_dependency_query() -> bool:
    return False


@pytest.fixture
def start_scan_mock(
    requests_mock, scan_config, mocked_scan_id, enable_dependency_query
):
    start_scan_response = out.ScanResponse.from_json(
        {
            "info": {
                **({"id": mocked_scan_id} if mocked_scan_id else {}),
                "enabled_products": ["sast", "sca"],
                "deployment_id": DEPLOYMENT_ID,
                "deployment_name": "org_name",
            },
            "config": {
                "rules": YAML(typ="safe").load(scan_config),
                "triage_ignored_syntactic_ids": ["f3b21c38bc22a1f1f870d49fc3a40244"],
                "triage_ignored_match_based_ids": [
                    "e536489e68267e16e71dd76a61e27815fd86a7e2417d96f8e0c43af48540a41d41e6acad52f7ccda83b5c6168dd5559cd49169617e3aac1b7ea091d8a20ebf12_0"
                ],
            },
            "engine_params": {
                "dependency_query": enable_dependency_query,
            },
        }
    )
    return requests_mock.post(
        "https://semgrep.dev/api/cli/scans", json=start_scan_response.to_json()
    )


@pytest.fixture
def upload_results_mock(requests_mock, mocked_scan_id, mocked_task_id):
    results_response = out.CiScanResultsResponse(errors=[], task_id=mocked_task_id)
    return requests_mock.post(
        f"https://semgrep.dev/api/agent/scans/{mocked_scan_id}/results",
        json=results_response.to_json(),
    )


@pytest.fixture
def complete_scan_mock(requests_mock, mocked_scan_id):
    complete_response = out.CiScanCompleteResponse(
        success=True, app_block_override=True, app_block_reason="Test Reason"
    )
    return requests_mock.post(
        f"https://semgrep.dev/api/agent/scans/{mocked_scan_id}/complete",
        json=complete_response.to_json(),
    )


@pytest.fixture
def mock_ci_api(start_scan_mock, upload_results_mock, complete_scan_mock):
    # just for easier access to all mocks in tests that want them.
    pass


@pytest.fixture(params=[True, False], ids=["autofix", "noautofix"])
def mock_autofix(request, mocker):
    mocker.patch.object(ScanHandler, "autofix", request.param)


##############################################################################
# The tests
##############################################################################


@pytest.mark.parametrize(
    "env",
    [
        {  # Local run with no CI env vars
            "SEMGREP_APP_TOKEN": "dummy",
            "SEMGREP_REPO_URL": REMOTE_REPO_URL,
        },
        {  # Github full scan
            "CI": "true",
            **DEFAULT_GITHUB_VARS,
            "GITHUB_EVENT_NAME": "push",
            "GITHUB_REF": f"refs/heads/{BRANCH_NAME}",
            "GITHUB_BASE_REF": "",
            "GITHUB_HEAD_REF": "",
        },
        {  # Github full scan with SEMGREP env vars set
            "CI": "true",
            **DEFAULT_GITHUB_VARS,
            "GITHUB_EVENT_NAME": "push",
            "GITHUB_REF": f"refs/heads/{BRANCH_NAME}",
            "GITHUB_BASE_REF": "",
            "GITHUB_HEAD_REF": "",
            "SEMGREP_REPO_NAME": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "SEMGREP_JOB_URL": "customjoburl.com",
            "SEMGREP_PR_ID": "312",  # should make the event_name `pull_request`
            "SEMGREP_PR_TITLE": "PR_TITLE",
            "SEMGREP_BRANCH": BRANCH_NAME,
        },
        {  # github but different server url - full scan
            "CI": "true",
            **DEFAULT_GITHUB_VARS,
            "GITHUB_EVENT_NAME": "push",
            "GITHUB_REF": f"refs/heads/{BRANCH_NAME}",
            "GITHUB_BASE_REF": "",
            "GITHUB_HEAD_REF": "",
            "GITHUB_SERVER_URL": "https://some.enterprise.url.com",
        },
        {  # Github PR
            "CI": "true",
            **DEFAULT_GITHUB_VARS,
            "GITHUB_EVENT_NAME": "pull_request",
            # Sent in metadata but no functionality change
            "GITHUB_REF": "refs/pull/123/merge",
            "GITHUB_BASE_REF": MAIN_BRANCH_NAME,
            "GITHUB_HEAD_REF": BRANCH_NAME,
        },
        {  # Github PR with additional project metadata
            "CI": "true",
            **DEFAULT_GITHUB_VARS,
            "GITHUB_EVENT_NAME": "pull_request",
            # Sent in metadata but no functionality change
            "GITHUB_REF": "refs/pull/123/merge",
            "GITHUB_BASE_REF": MAIN_BRANCH_NAME,
            "GITHUB_HEAD_REF": BRANCH_NAME,
            "SEMGREP_PROJECT_CONFIG": "tags:\n- tag1\n- tag_key:tag_val\n",
        },
        {  # Gitlab PR
            "CI": "true",
            "GITLAB_CI": "true",
            "CI_PROJECT_PATH": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "CI_PIPELINE_SOURCE": "merge_request_event",  # or push
            "CI_MERGE_REQUEST_TARGET_BRANCH_NAME": MAIN_BRANCH_NAME,
            # Sent in metadata but no actual functionality change
            "CI_MERGE_REQUEST_PROJECT_URL": "https://some.project.url.test.placeholder",
            "CI_JOB_TOKEN": "some-token-test-placeholder",
            "CI_COMMIT_REF_NAME": BRANCH_NAME,
            "CI_COMMIT_SHA": "unused-commit-test-placeholder",
            "CI_PROJECT_URL": "https://example.com/gitlab-org/gitlab-foss",
            "CI_JOB_URL": "https://gitlab.com/gitlab-examples/ci-debug-trace/-/jobs/379424655",
            "CI_MERGE_REQUEST_IID": "unused-iid-test-placeholder",
            "CI_MERGE_REQUEST_DIFF_BASE_SHA": "unused-commit-test-placeholder",
            "CI_MERGE_REQUEST_TITLE": "unused-merge-request-title-test-placeholder",
        },
        {  # Gitlab PR but with SEMGREP env vars set
            "CI": "true",
            "GITLAB_CI": "true",
            "SEMGREP_REPO_NAME": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "CI_PIPELINE_SOURCE": "merge_request_event",  # or push
            "CI_MERGE_REQUEST_TARGET_BRANCH_NAME": MAIN_BRANCH_NAME,
            # Sent in metadata but no actual functionality change
            "CI_MERGE_REQUEST_PROJECT_URL": "https://some.project.url.test.placeholder",
            "CI_JOB_TOKEN": "some-token-test-placeholder",
            "CI_COMMIT_REF_NAME": BRANCH_NAME,
            "SEMGREP_COMMIT": "unused-commit-test-placeholder",
            "SEMGREP_REPO_URL": "https://example.com/gitlab-org/gitlab-foss",
            "SEMGREP_JOB_URL": "https://gitlab.com/gitlab-examples/ci-debug-trace/-/jobs/379424655",
            "SEMGREP_PR_ID": "unused-iid-test-placeholder",
            "CI_MERGE_REQUEST_DIFF_BASE_SHA": "unused-commit-test-placeholder",
            "SEMGREP_PR_TITLE": "unused-merge-request-title-test-placeholder",
        },
        {  # Gitlab
            "CI": "true",
            "GITLAB_CI": "true",
            "CI_PROJECT_PATH": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "CI_PIPELINE_SOURCE": "push",
            # Sent in metadata but no actual functionality change
            "CI_JOB_TOKEN": "some-token-test-placeholder",
            "CI_COMMIT_REF_NAME": BRANCH_NAME,
            "CI_COMMIT_SHA": "unused-commit-test-placeholder",
            "CI_PROJECT_URL": "https://example.com/gitlab-org/gitlab-foss",
            "CI_JOB_URL": "https://gitlab.com/gitlab-examples/ci-debug-trace/-/jobs/379424655",
        },
        {  # Circle CI
            "CI": "true",
            "CIRCLECI": "true",
            "CIRCLE_PROJECT_USERNAME": REPO_DIR_NAME,
            "CIRCLE_PROJECT_REPONAME": REPO_DIR_NAME,
            "CIRCLE_REPOSITORY_URL": f"git@github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}.git",
            "CIRCLE_BRANCH": BRANCH_NAME,
            "CIRCLE_BUILD_URL": "https://circle.ci.build.url",
            "CIRCLE_PULL_REQUEST": f"https://github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}/pull/35",
        },
        {  # Circle CI, overwrite autodetected variables
            "CI": "true",
            "SEMGREP_REPO_NAME": "a/repo/name",
            "SEMGREP_REPO_URL": "https://random.url.org/some/path",
            "SEMGREP_BRANCH": "branch/some-other-branch-name",
            "SEMGREP_JOB_URL": "https://another.random.url.org/some/path",
            "SEMGREP_COMMIT": "<some_random_commit>",
            "SEMGREP_PR_ID": "99999",
            "CIRCLECI": "true",
            "CIRCLE_PROJECT_USERNAME": REPO_DIR_NAME,
            "CIRCLE_PROJECT_REPONAME": REPO_DIR_NAME,
            "CIRCLE_REPOSITORY_URL": f"git@github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}.git",
            "CIRCLE_BRANCH": BRANCH_NAME,
            "CIRCLE_BUILD_URL": "https://circle.ci.build.url",
            "CIRCLE_PULL_REQUEST": f"https://github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}/pull/35",
        },
        {  # Jenkins
            "JENKINS_URL": "some_url",
            "GIT_URL": "https://github.com/org/repo.git/",
            "GIT_BRANCH": BRANCH_NAME,
            "BUILD_URL": "https://jenkins.build.url",
        },
        {  # Jenkins, overwrite autodetected variables
            "JENKINS_URL": "some_url",
            "SEMGREP_REPO_NAME": "a/repo/name",
            "SEMGREP_REPO_URL": "https://random.url.org/some/path",
            "SEMGREP_BRANCH": "branch/some-other-branch-name",
            "SEMGREP_JOB_URL": "https://another.random.url.org/some/path",
            "SEMGREP_COMMIT": "<some_random_commit>",
            "GIT_URL": "https://github.com/org/repo.git/",
            "GIT_BRANCH": BRANCH_NAME,
            "BUILD_URL": "https://jenkins.build.url",
        },
        {  # Jenkins, not defined GIT_URL
            "JENKINS_URL": "some_url",
            "SEMGREP_REPO_URL": "https://random.url.org/some/path",
            "SEMGREP_REPO_NAME": "a/repo/name",
            "GIT_BRANCH": BRANCH_NAME,
            "BUILD_URL": "https://jenkins.build.url",
        },
        {  # Bitbucket
            "CI": "true",
            "BITBUCKET_BUILD_NUMBER": "hi",
            "BITBUCKET_REPO_FULL_NAME": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "BITBUCKET_GIT_HTTP_ORIGIN": f"http://bitbucket.org/{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "BITBUCKET_BRANCH": BRANCH_NAME,
            "BITBUCKET_PIPELINE_UUID": "a-uuid",
            "BITBUCKET_PR_ID": "35",
        },
        {  # Bitbucket, overwrite autodetected variables
            "CI": "true",
            "BITBUCKET_BUILD_NUMBER": "hi",
            "SEMGREP_REPO_NAME": "a/repo/name",
            "SEMGREP_REPO_URL": "https://random.url.org/some/path",
            "SEMGREP_BRANCH": "branch/some-other-branch-name",
            "SEMGREP_JOB_URL": "https://another.random.url.org/some/path",
            "SEMGREP_COMMIT": "<some_random_commit>",
            "SEMGREP_PR_ID": "99999",
            "BITBUCKET_REPO_FULL_NAME": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "BITBUCKET_GIT_HTTP_ORIGIN": f"http://bitbucket.org/{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "BITBUCKET_BRANCH": BRANCH_NAME,
            "BITBUCKET_PIPELINE_UUID": "a-uuid",
            "BITBUCKET_PR_ID": "35",
        },
        {  # Azure Pipelines
            "BUILD_BUILDID": "some_id",
            "BUILD_REPOSITORY_URI": f"https://github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}.git",
            "SYSTEM_PULLREQUEST_SOURCEBRANCH": BRANCH_NAME,
            "SYSTEM_TEAMFOUNDATIONSERVERURI": "https://azure.pipeline.url/",
            "SYSTEM_TEAMPROJECTID": "project_id",
            "SYSTEM_JOBID": "job_id",
            "SYSTEM_TASKINSTANCEID": "task_id",
            "SYSTEM_PULLREQUEST_PULLREQUESTNUMBER": "1234",
        },
        {  # Azure Pipelines, overwrite autodetected variables
            "BUILD_BUILDID": "some_id",
            "SEMGREP_REPO_NAME": "a/repo/name",
            "SEMGREP_REPO_URL": "https://random.url.org/some/path",
            "SEMGREP_BRANCH": "branch/some-other-branch-name",
            "SEMGREP_JOB_URL": "https://another.random.url.org/some/path",
            "SEMGREP_COMMIT": "<some_random_commit>",
            "SEMGREP_PR_ID": "34566",
            "BUILD_REPOSITORY_URI": f"https://github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}.git",
            "SYSTEM_PULLREQUEST_SOURCEBRANCH": BRANCH_NAME,
            "SYSTEM_TEAMFOUNDATIONSERVERURI": "https://azure.pipeline.url/",
            "SYSTEM_TEAMPROJECTID": "project_id",
            "SYSTEM_JOBID": "job_id",
            "SYSTEM_TASKINSTANCEID": "task_id",
        },
        {  # Buildkite
            "BUILDKITE": "true",
            "BUILDKITE_REPO": f"git@github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}.git",
            "BUILDKITE_BRANCH": BRANCH_NAME,
            "BUILDKITE_BUILD_URL": "https://buildkite.build.url/something",
            "BUILDKITE_JOB_ID": "42",
            "BUILDKITE_PULL_REQUEST": "35",
            "BUILDKITE_BUILD_AUTHOR": AUTHOR_NAME,
            "BUILDKITE_BUILD_AUTHOR_EMAIL": AUTHOR_EMAIL,
            "BUILDKITE_MESSAGE": COMMIT_MESSAGE,
        },
        {  # Buildkite, overwrite autodetected variables
            "BUILDKITE": "true",
            "SEMGREP_REPO_NAME": "a/repo/name",
            "SEMGREP_REPO_URL": "https://random.url.org/some/path",
            "SEMGREP_BRANCH": "branch/some-other-branch-name",
            "SEMGREP_JOB_URL": "https://another.random.url.org/some/path",
            "SEMGREP_COMMIT": "<some_random_commit>",
            "SEMGREP_PR_ID": "99999",
            "BUILDKITE_REPO": f"git@github.com/{REPO_DIR_NAME}/{REPO_DIR_NAME}.git",
            "BUILDKITE_BRANCH": BRANCH_NAME,
            "BUILDKITE_BUILD_URL": "https://buildkite.build.url/something",
            "BUILDKITE_JOB_ID": "42",
            "BUILDKITE_PULL_REQUEST": "35",
            "BUILDKITE_BUILD_AUTHOR": AUTHOR_NAME,
            "BUILDKITE_BUILD_AUTHOR_EMAIL": AUTHOR_EMAIL,
            "BUILDKITE_MESSAGE": COMMIT_MESSAGE,
        },
        {  # Travis CI
            "CI": "true",
            "TRAVIS": "true",
            "TRAVIS_REPO_SLUG": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "TRAVIS_PULL_REQUEST_BRANCH": BRANCH_NAME,
            "TRAVIS_JOB_WEB_URL": "https://travis.job.web.url/",
            "TRAVIS_PULL_REQUEST": "35",
            "TRAVIS_COMMIT_MESSAGE": COMMIT_MESSAGE,
        },
        {  # Travis CI
            "CI": "true",
            "TRAVIS": "true",
            "SEMGREP_REPO_NAME": "a/repo/name",
            "SEMGREP_REPO_URL": "https://random.url.org/some/path",
            "SEMGREP_BRANCH": "branch/some-other-branch-name",
            "SEMGREP_JOB_URL": "https://another.random.url.org/some/path",
            "SEMGREP_COMMIT": "<some_random_commit>",
            "SEMGREP_PR_ID": "99999",
            "TRAVIS_REPO_SLUG": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "TRAVIS_PULL_REQUEST_BRANCH": BRANCH_NAME,
            "TRAVIS_JOB_WEB_URL": "https://travis.job.web.url/",
            "TRAVIS_PULL_REQUEST": "35",
            "TRAVIS_COMMIT_MESSAGE": COMMIT_MESSAGE,
        },
        {  # Special SCM with org in path
            "CI": "true",
            "SEMGREP_REPO_NAME": f"{REPO_ORG_NAME}/{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "SEMGREP_REPO_URL": f"https://some.enterprise.url.com/{REPO_ORG_NAME}/{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            # Sent in metadata but no functionality change
            "SEMGREP_PR_ID": "35",
            "SEMGREP_BRANCH": BRANCH_NAME,
        },
        {  # URL that doesn't parse correctly
            "CI": "true",
            "SEMGREP_REPO_NAME": f"{REPO_ORG_NAME}/{REPO_DIR_NAME}/{REPO_DIR_NAME}",
            "SEMGREP_REPO_URL": "https://gitlab.net/foo.bar/a-b/a-b-c-d",
            # Sent in metadata but no functionality change
            "SEMGREP_PR_ID": "35",
            "SEMGREP_BRANCH": BRANCH_NAME,
        },
    ],
    ids=[
        "local",
        "github-push",
        "github-push-special-env-vars",
        "github-enterprise",
        "github-pr",
        "github-pr-semgrepconfig",
        "gitlab",
        "gitlab-special-env-vars",
        "gitlab-push",
        "circleci",
        "circleci-overwrite-autodetected-variables",
        "jenkins",
        "jenkins-overwrite-autodetected-variables",
        "jenkins-missing-vars",
        "bitbucket",
        "bitbucket-overwrite-autodetected-variables",
        "azure-pipelines",
        "azure-pipelines-overwrite-autodetected-variables",
        "buildkite",
        "buildkite-overwrite-autodetected-variables",
        "travis",
        "travis-overwrite-autodetected-variables",
        "self-hosted",
        "unparsable_url",
    ],
)
@pytest.mark.skipif(
    sys.version_info < (3, 8),
    reason="snapshotting mock call kwargs doesn't work on py3.7",
)
@pytest.mark.osemfail
def test_full_run(
    tmp_path,
    git_tmp_path_with_commit,
    snapshot,
    env,
    run_semgrep: RunSemgrep,
    mocker,
    mock_autofix,
    start_scan_mock,
    upload_results_mock,
    complete_scan_mock,
):
    repo_copy_base, base_commit, head_commit = git_tmp_path_with_commit

    # Set envvars that depend on commit hashes:
    if env.get("GITLAB_CI"):
        env["CI_COMMIT_SHA"] = head_commit
    if env.get("GITHUB_ACTIONS"):
        env["GITHUB_SHA"] = head_commit

        if env["GITHUB_EVENT_NAME"] == "pull_request":
            event = {
                "pull_request": {
                    "user": {
                        "login": "user-login",
                        "avatar_url": "some.user.avatar.com",
                    },
                    "head": {
                        "sha": head_commit,
                        "ref": BRANCH_NAME,
                        "number": "7",
                        "title": "placeholder-pr-title",
                        "repo": {"clone_url": "git://github.com/head/repo.git"},
                    },
                    "base": {
                        "sha": base_commit,
                        "ref": "main",
                        "repo": {"clone_url": "git://github.com/base/repo.git"},
                    },
                },
                "sender": {
                    "login": "test-username",
                    "avatar_url": "some.test.avatar.url.com",
                },
            }
            event_path = tmp_path / "event_path.json"
            event_path.write_text(json.dumps(event))
            env["GITHUB_EVENT_PATH"] = str(event_path)

        assert USED_GITHUB_VARS <= set(
            env.keys()
        ), f"not all github vars are set, missing: {USED_GITHUB_VARS - set(env.keys())}"

    if env.get("CIRCLECI"):
        env["CIRCLE_SHA1"] = head_commit
    if env.get("JENKINS_URL"):
        env["GIT_COMMIT"] = head_commit
    if env.get("BITBUCKET_BUILD_NUMBER"):
        env["BITBUCKET_COMMIT"] = head_commit
    if env.get("BUILD_BUILDID"):
        env["SYSTEM_PULLREQUEST_SOURCECOMMITID"] = head_commit
    if env.get("BUILDKITE"):
        env["BUILDKITE_COMMIT"] = head_commit
    if env.get("TRAVIS"):
        env["TRAVIS_COMMIT"] = head_commit
    env["SEMGREP_APP_TOKEN"] = "fake-key-from-tests"

    if env.get("SEMGREP_PROJECT_CONFIG"):
        contents = env.get("SEMGREP_PROJECT_CONFIG")
        make_semgrepconfig_file(repo_copy_base, contents)

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        strict=False,
        assert_exit_code=None,
        env=env,
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )

    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                head_commit,
                head_commit[:7],
                base_commit,
                re.compile(r'GITHUB_EVENT_PATH="(.+?)"'),
                re.compile(
                    r"\(<MagicMock name='post\(\)\.json\(\)\.get\(\)' id='\d+'>\)"
                ),
                re.compile(r'"commit_hash": "(.*)",?'),
                re.compile(r'"commit_timestamp": "(.*)",?'),
            ]
        ),
        "results.txt",
    )

    # Check correct metadata
    scan_create_json = start_scan_mock.last_request.json()
    meta_json = scan_create_json["meta"]

    if "SEMGREP_COMMIT" in env:
        assert meta_json["commit"] == env["SEMGREP_COMMIT"]
        meta_json["commit"] = "sanitized semgrep commit"
    else:
        assert meta_json["commit"] == head_commit
        meta_json["commit"] = "sanitized"

    assert meta_json["semgrep_version"] == __VERSION__
    meta_json["semgrep_version"] = "<sanitized version>"

    assert meta_json["commit_timestamp"] == FROZEN_ISOTIMESTAMP

    if env.get("GITLAB_CI"):
        # If in a merge pipeline, base_sha is defined, otherwise is None
        gitlab_base_sha = (
            base_commit if env.get("CI_MERGE_REQUEST_TARGET_BRANCH_NAME") else None
        )
        if gitlab_base_sha != None:
            assert meta_json["base_sha"] == gitlab_base_sha
            meta_json["base_sha"] = "sanitized"

    # TODO: we should add those in the snapshots at some point
    del scan_create_json["project_metadata"]
    del scan_create_json["project_config"]
    del scan_create_json["scan_metadata"]
    snapshot.assert_match(json.dumps(scan_create_json, indent=2), "meta.json")

    findings_and_ignores_json = upload_results_mock.last_request.json()
    for f in findings_and_ignores_json["findings"]:
        assert f["commit_date"] is not None
        f["commit_date"] = "sanitized"
    for f in findings_and_ignores_json["ignores"]:
        assert f["commit_date"] is not None
        f["commit_date"] = "sanitized"
    for f in findings_and_ignores_json["contributions"]:
        assert f["commit_hash"] is not None
        f["commit_hash"] = "sanitized"
        assert f["commit_timestamp"] is not None
        f["commit_timestamp"] = "sanitized"
    snapshot.assert_match(
        json.dumps(findings_and_ignores_json, indent=2), "findings_and_ignores.json"
    )

    complete_json = complete_scan_mock.last_request.json()
    complete_json["stats"]["total_time"] = 0.5  # Sanitize time for comparison
    # TODO: flaky tests (on Linux at least)
    # see https://linear.app/r2c/issue/PA-2461/restore-flaky-e2e-tests for more info
    complete_json["stats"]["lockfile_scan_info"] = {}
    snapshot.assert_match(json.dumps(complete_json, indent=2), "complete.json")


@pytest.mark.osemfail
def test_lockfile_parse_failure_reporting(
    git_tmp_path_with_commit,
    run_semgrep: RunSemgrep,
    snapshot,
    start_scan_mock,
    upload_results_mock,
    complete_scan_mock,
):
    repo_base, base_commit, _ = git_tmp_path_with_commit
    subprocess.run(
        ["git", "config", "user.email", AUTHOR_EMAIL],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "config", "user.name", AUTHOR_NAME],
        check=True,
        capture_output=True,
    )

    bad_lockfile = repo_base / "Pipfile.lock"
    bad_lockfile.write_text(
        dedent(
            """
            invalid
            {
                "_meta": {
                    "hash": {
                        "sha256": "7f7606f08e0544d8d012ef4d097dabdd6df6843a28793eb6551245d4b2db4242"
                    },
                    "pipfile-spec": 6,
                    "requires": {
                        "python_version": "3.8"
                    },
                    "sources": [
                        {
                            "name": "pypi",
                            "url": "https://pypi.org/simple",
                            "verify_ssl": true
                        }
                    ]
                },
                "default": {},
                "develop": {}
            }
            """
        )
    )

    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", "Bad lockfile"],
        check=True,
        capture_output=True,
    )

    head_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )
    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                head_commit,
                head_commit[:7],
                base_commit,
                re.compile(r'GITHUB_EVENT_PATH="(.+?)"'),
                re.compile(
                    r"\(<MagicMock name='post\(\)\.json\(\)\.get\(\)' id='\d+'>\)"
                ),
                re.compile(r'"commit_hash": "(.*)",?'),
                re.compile(r'"commit_timestamp": "(.*)",?'),
            ]
        ),
        "results.txt",
    )

    # Check correct metadata
    findings_and_ignores_json = upload_results_mock.last_request.json()
    for f in findings_and_ignores_json["findings"]:
        assert f["commit_date"] is not None
        f["commit_date"] = "sanitized"
    for f in findings_and_ignores_json["ignores"]:
        assert f["commit_date"] is not None
        f["commit_date"] = "sanitized"
    for f in findings_and_ignores_json["contributions"]:
        assert f["commit_hash"] is not None
        f["commit_hash"] = "sanitized"
        assert f["commit_timestamp"] is not None
        f["commit_timestamp"] = "sanitized"
    snapshot.assert_match(
        json.dumps(findings_and_ignores_json, indent=2), "findings_and_ignores.json"
    )

    complete_json = complete_scan_mock.last_request.json()
    complete_json["stats"]["total_time"] = 0.5  # Sanitize time for comparison
    complete_json["stats"]["lockfile_scan_info"] = {}
    assert len(complete_json["dependency_parser_errors"]) > 0
    snapshot.assert_match(json.dumps(complete_json, indent=2), "complete.json")


# TODO: flaky test on Linux
# see https://linear.app/r2c/issue/PA-2461/restore-flaky-e2e-tests
# def test_github_ci_bad_base_sha(
#    run_semgrep: RunSemgrep, snapshot, git_tmp_path, tmp_path, monkeypatch, start_scan_mock, upload_results_mock, complete_scan_mock
# ):
#    """
#    Github PullRequest Event Webhook file's reported base sha is not guaranteed
#    to be the shahash of the latest commit on the base branch
#
#    In particular the following situations can cause the base sha to be stale
#    (and if we rely on it being latest cause semgrep to incorrectly calculate merge-base):
#    - If new commits are pushed onto base branch and a githubaction is rerun
#    - If the base branch latest is merged into some third branch and that third branch
#      is merged into the PR branch
#
#    Note that simply merging the base branch into the PR branch does cause the base sha to be updated
#
#    This test verifies that we scan the right things even if base sha in a mocked github
#    env is stale. Note that the test does not mock the exact situations above but simply
#    some state where reported base sha is stale
#    """
#
#    # Setup Git Repo
#    """
#        *   17b3114 (HEAD -> bar) merging foo
#        |\
#        | * f7ee312 (foo) commit #2
#        * | e04f88c commit #1
#        |/
#        * 191a3ac commit #1
#
#    Regenerate this tree by running:
#        git_log = subprocess.run(["git", "--no-pager", "log", "--oneline", "--decorate", "--graph"], check=True, capture_output=True, encoding="utf-8")
#        print(git_log.stdout)
#    """
#    commits = defaultdict(list)
#    foo = git_tmp_path / "foo.py"
#    bar = git_tmp_path / "bar.py"
#
#    subprocess.run(["git", "checkout", "-b", "foo"])
#    foo.open("a").write(f"foo == 5\n\n")
#    commits["foo"].append(_git_commit(1, add=True))
#
#    subprocess.run(["git", "checkout", "-b", "bar"])
#    bar.open("a").write(f"bar == 5\n\n")
#    commits["bar"].append(_git_commit(1, add=True))
#
#    subprocess.run(["git", "checkout", "foo"])
#    foo.open("a").write(f"new == 5\n\n")
#    commits["foo"].append(_git_commit(2, add=True))
#
#    subprocess.run(["git", "checkout", "bar"])
#    commits["bar"].append(_git_merge("foo"))
#
#    # Mock Github Actions Env Vars
#    env = {
#        "CI": "true",
#        "GITHUB_ACTIONS": "true",
#        "GITHUB_EVENT_NAME": "pull_request",
#        "GITHUB_REPOSITORY": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
#        # Sent in metadata but no functionality change
#        "GITHUB_RUN_ID": "35",
#        "GITHUB_ACTOR": "some_test_username",
#        "GITHUB_REF": BRANCH_NAME,
#    }
#    event = {
#        "pull_request": {
#            "user": {
#                "login": "user-login",
#                "avatar_url": "some.user.avatar.com",
#            },
#            "head": {
#                "sha": commits["bar"][-1],
#                "ref": "bar",
#                "number": "7",
#                "title": "placeholder-pr-title",
#                "repo": {"clone_url": str(git_tmp_path)},
#            },
#            "base": {
#                "sha": commits["foo"][0],  # Note how this is not latest commit in foo
#                "ref": "foo",
#                "repo": {"clone_url": str(git_tmp_path)},
#            },
#        },
#        "sender": {
#            "login": "test-username",
#            "avatar_url": "some.test.avatar.url.com",
#        },
#    }
#    event_path = tmp_path / "event_path.json"
#    event_path.write_text(json.dumps(event))
#    env["GITHUB_EVENT_PATH"] = str(event_path)
#    env["SEMGREP_APP_TOKEN"] = "fake-key-from-tests"
#
#    # Mimic having a remote by having a new repo dir and pointing origin to the repo
#    # we setup above
#    repo_copy_base = tmp_path / "copy"
#    repo_copy_base.mkdir()
#    monkeypatch.chdir(repo_copy_base)
#    subprocess.run(["git", "init"], check=True, capture_output=True)
#    subprocess.run(
#        ["git", "remote", "add", "origin", git_tmp_path],
#        check=True,
#        capture_output=True,
#    )
#    subprocess.run(["git", "fetch", "origin", "--depth", "1", "bar:bar"])
#    subprocess.run(["git", "checkout", "bar"], check=True, capture_output=True)
#
#    result = run_semgrep(
#        options=["ci", "--debug", "--no-force-color", "--no-suppress-errors"],
#        strict=False,
#        assert_exit_code=None,
#        env=env,
#    )
#
#    snapshot.assert_match(
#        result.as_snapshot(
#            mask=[
#                re.compile(r'GITHUB_EVENT_PATH="(.+?)"'),
#                re.compile(r"\(<MagicMock name='post\(\)\.json\(\)\.get\(\)' id='\d+'>\)")
#                # Mask variable debug output
#                re.compile(r"/(.*)/semgrep-core"),
#                re.compile(r"loaded 1 configs in(.*)"),
#                re.compile(r".*https://semgrep.dev(.*).*"),
#                re.compile(r"(.*Main\.Dune__exe__Main.*)"),
#                re.compile(r"(.*Main\.Run_semgrep.*)"),
#                re.compile(r"(.*Main\.Common.*)"),
#                re.compile(r"(.*Main\.Parse_target.*)"),
#                re.compile(r"(.*Main\.Core_CLI.*)"),
#                re.compile(r"semgrep ran in (.*) on 1 files"),
#                re.compile(r"\"total_time\":(.*)"),
#                re.compile(r"\"commit_date\":(.*)"),
#                re.compile(r"-targets (.*) -timeout"),
#                re.compile(r"-rules (.*).json"),
#                re.compile(r".*Main.Autofix.*"),
#                str(git_tmp_path),
#                str(tmp_path),
#            ]
#        ),
#        "results.txt",
#    )
#
#    findings_json = upload_results_mock.last_request.json()
#    assert (
#        len(findings_json["findings"]) == 1
#    ), "Potentially scanning wrong files/commits"


@pytest.mark.osemfail
def test_shallow_wrong_merge_base(
    run_semgrep: RunSemgrep,
    snapshot,
    git_tmp_path,
    tmp_path,
    monkeypatch,
    upload_results_mock,
):
    """ """
    commits = defaultdict(list)
    foo = git_tmp_path / "foo.py"
    bar = git_tmp_path / "bar.py"
    baz = git_tmp_path / "baz.py"

    subprocess.run(["git", "checkout", "-b", "foo"])
    foo.open("a").write(f"foo == 5\n")
    commits["foo"].append(_git_commit(1, add=True))
    subprocess.run(
        [
            "git",
            "show",
            "-s",
            "--format=%ct",
            "b903231925961ac9d787ae53ee0bd15ec156e689",
        ]
    )

    subprocess.run(["git", "checkout", "-b", "baz"])
    baz.open("a").write(f"baz == 5\n")
    commits["baz"].append(_git_commit(2, add=True))

    subprocess.run(["git", "checkout", "foo"])
    foo.open("a").write("foo == 5\n")
    commits["foo"].append(_git_commit(3, add=True))

    subprocess.run(["git", "checkout", "-b", "bar"])
    bar.open("a").write(f"bar == 5\n\n")
    commits["bar"].append(_git_commit(4, add=True))

    for _ in range(16):
        subprocess.run(["git", "checkout", "foo"])
        foo.open("a").write(f"new == 5\n\n")
        commits["foo"].append(_git_commit(5, add=True))

    commits["foo"].append(_git_merge("baz"))
    git_log = subprocess.run(
        ["git", "--no-pager", "log", "--oneline", "--decorate", "--graph", "--all"],
        check=True,
        capture_output=True,
        encoding="utf-8",
    )
    print(git_log.stdout)

    subprocess.run(["git", "checkout", "bar"])
    git_log = subprocess.run(
        ["git", "--no-pager", "log", "--oneline", "--decorate", "--graph"],
        check=True,
        capture_output=True,
        encoding="utf-8",
    )
    print(git_log.stdout)

    # Mock Github Actions Env Vars
    env = {
        "CI": "true",
        "GITHUB_ACTIONS": "true",
        "GITHUB_EVENT_NAME": "pull_request",
        "GITHUB_REPOSITORY": f"{REPO_DIR_NAME}/{REPO_DIR_NAME}",
        # Sent in metadata but no functionality change
        "GITHUB_RUN_ID": "35",
        "GITHUB_ACTOR": "some_test_username",
        "GITHUB_REF": BRANCH_NAME,
    }
    event = {
        "pull_request": {
            "user": {
                "login": "user-login",
                "avatar_url": "some.user.avatar.com",
            },
            "head": {
                "sha": commits["bar"][-1],
                "ref": "bar",
                "number": "7",
                "title": "placeholder-pr-title",
                "repo": {"clone_url": str(git_tmp_path)},
            },
            "base": {
                "sha": commits["foo"][-1],  # Note how this is not latest commit in foo
                "ref": "foo",
                "repo": {"clone_url": str(git_tmp_path)},
            },
        },
        "sender": {
            "login": "test-username",
            "avatar_url": "some.test.avatar.url.com",
        },
    }
    event_path = tmp_path / "event_path.json"
    event_path.write_text(json.dumps(event))
    env["GITHUB_EVENT_PATH"] = str(event_path)
    env["SEMGREP_APP_TOKEN"] = "fake-key-from-tests"

    # Mimic having a remote by having a new repo dir and pointing origin to the repo
    # we setup above
    repo_copy_base = tmp_path / "copy"
    repo_copy_base.mkdir()
    monkeypatch.chdir(repo_copy_base)
    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "remote", "add", "origin", git_tmp_path],
        check=True,
        capture_output=True,
    )
    subprocess.run(["git", "fetch", "origin", "--depth", "1", "bar:bar"])
    subprocess.run(["git", "checkout", "bar"], check=True, capture_output=True)

    # Scan the wrong thing first and verify we get more findings than expected (2 > 1)
    result = run_semgrep(
        subcommand="ci",
        options=["--no-force-color", "--no-suppress-errors"],
        strict=False,
        assert_exit_code=None,
        env=env,
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )
    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                re.compile(r'GITHUB_EVENT_PATH="(.+?)"'),
            ]
        ),
        "bad_results.txt",
    )
    findings_json = upload_results_mock.last_request.json()
    assert (
        len(findings_json["findings"]) == 2
    ), "Test might be invalid since we expect this to scan the wrong thing"

    # Run again with greater depth
    result = run_semgrep(
        subcommand="ci",
        options=["--no-force-color", "--no-suppress-errors"],
        strict=False,
        assert_exit_code=None,
        env={**env, "SEMGREP_GHA_MIN_FETCH_DEPTH": "100"},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )

    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                re.compile(r'GITHUB_EVENT_PATH="(.+?)"'),
            ]
        ),
        "results.txt",
    )

    findings_json = upload_results_mock.last_request.json()
    assert (
        len(findings_json["findings"]) == 1
    ), "Potentially scanning wrong files/commits"


@pytest.mark.osemfail
def test_config_run(
    run_semgrep: RunSemgrep,
    git_tmp_path_with_commit,
    snapshot,
    mock_autofix,
    requests_mock,
    scan_config,
):
    # This test seems to provide coverage over running `semgrep ci --config <registry thing>` while logged out
    # Not actually sure who uses this, but its explicitly supported in code.
    requests_mock.get("https://semgrep.dev/p/something", text=scan_config)
    result = run_semgrep(
        "p/something",
        subcommand="ci",
        options=["--no-suppress-errors"],
        strict=False,
        assert_exit_code=1,
        env={"SEMGREP_APP_TOKEN": ""},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )
    snapshot.assert_match(
        result.as_snapshot(),
        "results.txt",
    )


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "format",
    ["--json", "--gitlab-sast", "--gitlab-secrets", "--sarif", "--emacs", "--vim"],
)
@pytest.mark.osemfail
def test_outputs(
    git_tmp_path_with_commit,
    snapshot,
    format,
    mock_autofix,
    run_semgrep: RunSemgrep,
):
    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", format],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        output_format=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )
    snapshot.assert_match(
        result.as_snapshot(),
        "results.txt",
    )


@pytest.mark.parametrize("nosem", ["--enable-nosem", "--disable-nosem"])
@pytest.mark.osemfail
def test_nosem(
    git_tmp_path_with_commit, snapshot, mock_autofix, nosem, run_semgrep: RunSemgrep
):
    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", nosem],
        target_name=None,
        strict=False,
        assert_exit_code=1,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )
    snapshot.assert_match(
        result.as_snapshot(),
        "output.txt",
    )


@pytest.mark.parametrize("mocked_scan_id", [None])
@pytest.mark.osemfail
def test_dryrun(
    tmp_path,
    git_tmp_path_with_commit,
    snapshot,
    run_semgrep: RunSemgrep,
    start_scan_mock,
):
    _, base_commit, head_commit = git_tmp_path_with_commit
    result = run_semgrep(
        subcommand="ci",
        options=["--dry-run", "--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )

    assert start_scan_mock.last_request.json()["scan_metadata"]["dry_run"] == True
    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                head_commit,
                head_commit[:7],
                base_commit,
                re.compile(r'"commit_date": (.*),?'),
                re.compile(r'"commit_timestamp": "(.*)",?'),
                re.compile(r'"total_time": (.*),?'),
                re.compile(r'"event_id": (.*),?'),
            ]
        ),
        "results.txt",
    )


def test_fail_auth_invalid_key(
    run_semgrep: RunSemgrep, mocker, git_tmp_path_with_commit, requests_mock
):
    """
    Test that an invalid api key returns exit code 13, even when errors are supressed
    """
    requests_mock.post("https://semgrep.dev/api/cli/scans", status_code=401)
    fail_open = requests_mock.post("https://fail-open.prod.semgrep.dev/failure")
    run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=13,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )
    assert not fail_open.called


@pytest.mark.osemfail
def test_fail_auth_invalid_key_suppressed_by_default(
    run_semgrep: RunSemgrep, mocker, git_tmp_path_with_commit, requests_mock
):
    """
    Test that an invalid api key returns exit code 13, even when errors are supressed
    """
    scan_create = requests_mock.post(
        "https://semgrep.dev/api/cli/scans", status_code=401
    )
    fail_open = requests_mock.post("https://fail-open.prod.semgrep.dev/failure")
    run_semgrep(
        subcommand="ci",
        target_name=None,
        strict=False,
        assert_exit_code=0,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )

    assert fail_open.called
    assert fail_open.last_request.json() == {
        "url": "https://semgrep.dev/api/cli/scans",
        "method": "POST",
        "status_code": 401,
        "request_id": scan_create.last_request.json()["scan_metadata"]["unique_id"],
        "error": str_containing("INVALID_API_KEY_EXIT_CODE"),
    }


@pytest.mark.osemfail
def test_fail_auth_invalid_response(
    run_semgrep: RunSemgrep, mocker, git_tmp_path_with_commit, requests_mock
):
    """
    Test that and invalid api key returns exit code 13
    """
    requests_mock.post("https://semgrep.dev/api/cli/scans", status_code=500)
    run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=2,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )


@pytest.mark.osemfail
def test_fail_auth_invalid_response_can_be_supressed(
    run_semgrep: RunSemgrep, mocker, git_tmp_path_with_commit, requests_mock
):
    """
    Test that failure to authenticate with --suppres-errors returns exit code 0
    """
    requests_mock.post("https://semgrep.dev/api/cli/scans", status_code=500)
    mock_send = mocker.spy(ErrorHandler, "send")
    run_semgrep(
        subcommand="ci",
        target_name=None,
        strict=False,
        assert_exit_code=0,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )

    mock_send.assert_called_once_with(mocker.ANY, 2)


@pytest.mark.osemfail
def test_fail_start_scan(run_semgrep: RunSemgrep, mocker, git_tmp_path_with_commit):
    """
    Test that failing to start scan does not have exit code 0 or 1
    """
    mocker.patch.object(ScanHandler, "start_scan", side_effect=Exception("Timeout"))
    run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=2,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )


@pytest.mark.osemfail
def test_fail_start_scan_error_handler(
    run_semgrep: RunSemgrep, mocker, git_tmp_path_with_commit
):
    """
    Test that failing to start scan with --suppres-errors returns exit code 0
    """
    mocker.patch.object(ScanHandler, "start_scan", side_effect=Exception("Timeout"))
    mock_send = mocker.spy(ErrorHandler, "send")
    run_semgrep(
        subcommand="ci",
        target_name=None,
        strict=False,
        assert_exit_code=0,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )

    mock_send.assert_called_once_with(mocker.ANY, 2)


@pytest.mark.osemfail
def test_fail_open_works_when_backend_is_down(
    run_semgrep: RunSemgrep, mocker, git_tmp_path_with_commit, requests_mock
):
    """
    Test that an invalid api key returns exit code 13, even when errors are supressed
    """
    scan_create = requests_mock.post(
        "https://semgrep.dev/api/cli/scans", exc=ConnectionError
    )
    fail_open = requests_mock.post("https://fail-open.prod.semgrep.dev/failure")
    run_semgrep(
        subcommand="ci",
        target_name=None,
        strict=False,
        assert_exit_code=0,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )

    assert fail_open.called
    assert fail_open.last_request.json() == {
        "url": "https://semgrep.dev/api/cli/scans",
        "method": "POST",
        "request_id": scan_create.last_request.json()["scan_metadata"]["unique_id"],
        "error": str_containing("requests.exceptions.ConnectionError"),
    }


@pytest.mark.parametrize("scan_config", [BAD_CONFIG], ids=["bad_config"])
@pytest.mark.osemfail
def test_bad_config(run_semgrep: RunSemgrep, git_tmp_path_with_commit):
    """
    Test that bad rules has exit code > 1
    """
    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=7,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )
    assert "Invalid rule schema" in result.stderr


@pytest.mark.parametrize("scan_config", [BAD_CONFIG], ids=["bad_config"])
@pytest.mark.osemfail
def test_bad_config_error_handler(
    run_semgrep: RunSemgrep, mocker, git_tmp_path_with_commit
):
    """
    Test that bad rules with --suppres-errors returns exit code 0
    """
    mock_send = mocker.spy(ErrorHandler, "send")
    result = run_semgrep(
        subcommand="ci",
        target_name=None,
        strict=False,
        assert_exit_code=0,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )
    assert "Invalid rule schema" in result.stderr
    mock_send.assert_called_once_with(mocker.ANY, 7)


@pytest.mark.osemfail
def test_fail_scan_findings(
    run_semgrep: RunSemgrep,
    mocker,
    git_tmp_path_with_commit,
    upload_results_mock,
):
    """
    Test failure with findings has exit code == 1.

    Asserts that error logs are NOT sent to fail-open
    """
    mock_send = mocker.spy(ErrorHandler, "send")

    run_semgrep(
        subcommand="ci",
        options=["--suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=1,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )
    mock_send.assert_called_once_with(mocker.ANY, 1)
    assert upload_results_mock.called


@pytest.mark.osemfail
def test_fail_finish_scan(run_semgrep: RunSemgrep, mocker, git_tmp_path_with_commit):
    """
    Test failure to send findings has exit code > 1
    """
    mocker.patch.object(ScanHandler, "report_findings", side_effect=Exception)
    run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=2,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )


@pytest.mark.osemfail
def test_backend_exit_code(
    run_semgrep: RunSemgrep,
    mocker,
    git_tmp_path_with_commit,
):
    """
    Test backend sending non-zero exit code on complete causes exit 1
    """
    mocker.patch.object(
        ScanHandler,
        "report_findings",
        return_value=ScanCompleteResult(True, True, "some reason to fail"),
    )
    run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=1,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )


@pytest.mark.osemfail
def test_fail_finish_scan_error_handler(
    run_semgrep: RunSemgrep, mocker, git_tmp_path_with_commit
):
    """
    Test failure to send findings with --suppres-errors returns exit code 0
    """
    mocker.patch.object(ScanHandler, "report_findings", side_effect=Exception)
    mock_send = mocker.spy(ErrorHandler, "send")
    run_semgrep(
        subcommand="ci",
        target_name=None,
        strict=False,
        assert_exit_code=0,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )
    mock_send.assert_called_once_with(mocker.ANY, 2)


@pytest.mark.osemfail
def test_git_failure(run_semgrep: RunSemgrep, git_tmp_path_with_commit, mocker):
    """
    Test failure from using git has exit code > 1
    """
    mocker.patch.object(GitMeta, "to_project_metadata", side_effect=Exception)
    run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=2,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )


@pytest.mark.osemfail
def test_git_failure_error_handler(
    run_semgrep: RunSemgrep, git_tmp_path_with_commit, mocker
):
    """
    Test failure from using git --suppres-errors returns exit code 0
    """
    mocker.patch.object(GitMeta, "to_project_metadata", side_effect=Exception)
    mock_send = mocker.spy(ErrorHandler, "send")
    run_semgrep(
        subcommand="ci",
        target_name=None,
        strict=False,
        assert_exit_code=0,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )
    mock_send.assert_called_once_with(mocker.ANY, 2)


@pytest.mark.parametrize(
    "scan_config",
    [
        dedent(
            """
    rules:
      - id: eqeq-bad
        pattern: $X == $X
        message: "useless comparison"
        languages: [python]
        severity: ERROR
      - id: supply-chain1
        message: "found a dependency"
        languages: [python]
        severity: ERROR
        r2c-internal-project-depends-on:
          namespace: pypi
          package: badlib
          version: == 99.99.99
        metadata:
          dev.semgrep.actions: [block]
          sca-kind: upgrade-only
    """
        ).lstrip()
    ],
    ids=["config"],
)
@pytest.mark.parametrize("enable_dependency_query", [True])
@pytest.mark.osemfail
def test_query_dependency(
    git_tmp_path_with_commit,
    snapshot,
    mocker,
    run_semgrep: RunSemgrep,
    start_scan_mock,
    upload_results_mock,
    complete_scan_mock,
):
    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,
    )
    snapshot.assert_match(
        result.as_snapshot(),
        "output.txt",
    )

    results_json = upload_results_mock.last_request.json()
    snapshot.assert_match(
        json.dumps(results_json["dependencies"], indent=2), "dependencies.json"
    )

    complete_json = complete_scan_mock.last_request.json()
    complete_json["stats"]["total_time"] = 0.5  # Sanitize time for comparison
    # TODO: flaky tests (on Linux at least)
    # see https://linear.app/r2c/issue/PA-2461/restore-flaky-e2e-tests for more info
    complete_json["stats"]["lockfile_scan_info"] = {}
    snapshot.assert_match(json.dumps(complete_json, indent=2), "complete.json")


@pytest.mark.osemfail
def test_metrics_enabled(
    run_semgrep: RunSemgrep,
    mocker,
):
    mock_send = mocker.patch.object(Metrics, "_post_metrics")
    run_semgrep(
        subcommand="ci",
        target_name=None,
        strict=False,
        assert_exit_code=1,
        force_metrics_off=False,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )
    mock_send.assert_called_once()


@pytest.mark.parametrize(
    "scan_config",
    [
        dedent(
            """
            rules:
              - id: supply-chain1
                message: "found a dependency"
                languages: [python]
                severity: ERROR
                r2c-internal-project-depends-on:
                    namespace: pypi
                    package: python-dateutil
                    version: == 2.8.2
                metadata:
                    dev.semgrep.actions: [block]
                    sca-kind: upgrade-only
            """
        ).lstrip()
    ],
    ids=["config"],
)
@pytest.mark.osemfail
def test_existing_supply_chain_finding(
    git_tmp_path_with_commit,
    snapshot,
    mocker,
    run_semgrep: RunSemgrep,
    start_scan_mock,
    upload_results_mock,
    complete_scan_mock,
):
    repo_copy_base, base_commit, head_commit = git_tmp_path_with_commit
    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,
    )
    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                head_commit,
                head_commit[:7],
                base_commit,
            ]
        ),
        "base_output.txt",
    )

    findings_json = upload_results_mock.last_request.json()
    assert len(findings_json["findings"]) == 1

    lockfile1 = repo_copy_base / "poetry.lock"
    lockfile1.write_text(
        dedent(
            """\
        [[package]]
        name = "badlib"
        version = "99.99.99"
        description = "it's bad"
        category = "dev"
        optional = false
        python-versions = ">=3.7"

        [[package]]
        name = "some-other-lib"
        version = "1.1.1"
        description = "it's bad"
        category = "dev"
        optional = false
        python-versions = ">=3.7"

        [[package]]
        name = "mypy"
        version = "0.950"
        description = "Optional static typing for Python"
        category = "dev"
        optional = false
        python-versions = ">=3.6"

        [[package]]
        name = "python-dateutil"
        version = "2.8.2"
        description = "Extensions to the standard Python datetime module"
        category = "main"
        optional = false
        python-versions = "!=3.0.*,!=3.1.*,!=3.2.*,>=2.7"
        """
        )
    )
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", "add lockfile"], check=True, capture_output=True
    )
    new_head_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--baseline-commit", head_commit],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )
    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                new_head_commit,
                new_head_commit[:7],
                head_commit,
            ]
        ),
        "new_output.txt",
    )
    findings_json = upload_results_mock.last_request.json()
    assert len(findings_json["findings"]) == 0


@pytest.mark.parametrize(
    "enabled_products",
    [[], ["product"]],
    ids=["empty-products", "non-empty-products"],
)
@pytest.mark.osemfail
def test_enabled_products(
    enabled_products: List[str],
    run_semgrep: RunSemgrep,
    mocker,
    git_tmp_path_with_commit,
):
    """
    Verify that for any given product, there is a valid output
    """
    mocker.patch.object(ScanHandler, "enabled_products", enabled_products)

    result = run_semgrep(
        options=["ci", "--no-suppress-errors"],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,
    )

    if not enabled_products:
        assert "Enabled products: None" in result.stderr
        assert "No products are enabled for this organization" in result.stderr
    else:
        assert f"Enabled products: {enabled_products[0]}" in result.stderr
        assert "No products are enabled for this organization" not in result.stderr


@pytest.mark.parametrize("enable_deepsemgrep", [True, False])
@pytest.mark.osemfail
def test_pro_diff_slow_rollout(
    run_semgrep: RunSemgrep,
    mocker,
    enable_deepsemgrep,
):
    """
    Verify that generic_slow_rollout enables pro diff scan
    """
    mocker.patch.object(ScanHandler, "generic_slow_rollout", True)
    mocker.patch.object(ScanHandler, "deepsemgrep", enable_deepsemgrep)
    mocker.patch.object(EngineType, "check_if_installed", return_value=True)
    mock_send = mocker.patch.object(Metrics, "add_diff_depth")

    result = run_semgrep(
        options=["ci", "--no-suppress-errors"],
        target_name=None,
        strict=False,
        force_metrics_off=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,
    )
    if enable_deepsemgrep:
        mock_send.assert_called_once_with(2)
    else:
        mock_send.assert_not_called()
