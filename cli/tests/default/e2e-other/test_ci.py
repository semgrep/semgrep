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
import tempfile
from collections import defaultdict
from pathlib import Path
from textwrap import dedent
from typing import List
from typing import Mapping

import pytest
from requests.exceptions import ConnectionError
from ruamel.yaml import YAML
from tests.conftest import load_anonymous_user_id
from tests.conftest import make_semgrepconfig_file
from tests.conftest import make_settings_file
from tests.conftest import str_containing
from tests.default.e2e.test_baseline import _git_commit
from tests.default.e2e.test_baseline import _git_merge
from tests.fixtures import RunSemgrep

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.app.scans import ScanHandler
from semgrep.constants import OutputFormat
from semgrep.engine import EngineType
from semgrep.error_handler import ErrorHandler
from semgrep.meta import GithubMeta
from semgrep.meta import GitlabMeta
from semgrep.meta import GitMeta
from semgrep.metrics import Metrics
from semgrep.settings import generate_anonymous_user_id
from semgrep.target_manager import SAST_PRODUCT
from semgrep.target_manager import SECRETS_PRODUCT

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
GENERIC_SECRETS_AND_REAL_RULE = dedent(
    """
    rules:
    - id: generic-secrets-rule-example
      message: "generic secrets rule message"
      languages: [python]
      severity: ERROR
      pattern: $X
      metadata:
        product: secrets
        generic_secrets: true
    - id: real-rule-example
      message: "this rule should actually display findings in CLI"
      languages: [python]
      severity: ERROR
      pattern: $X
"""
).lstrip()
FROZEN_ISOTIMESTAMP = out.Datetime("1970-01-01T00:00:00Z")
DUMMY_APP_TOKEN_ALICE = "peasoup"
DUMMY_APP_TOKEN_BOB = "coolcucumber"

# To ensure our tests are as accurate as possible, lets try to detect what
# GITHUB_ vars the app code uses, so the tests can enforce the env is mocked
# appropriately.
# UPDATE: the code below is now commented because it's using `git grep`
# which prevents to build pysemgrep (cd cli; make build) when not
# inside a repository (as in pro/.../check-semgrep-oss.jsonnet).
# Instead, we run the commented code below and just copy pasted the
# output in the USED_GITHUB_VARS below.
#
##_cli_src = (Path(__file__).parent.parent.parent.parent / "src").resolve()
##USED_GITHUB_VARS = set(
##    subprocess.run(
##        f"git grep --recurse-submodules -hPo 'GITHUB_[\\w_]*' {_cli_src}",
##        shell=True,
##        capture_output=True,
##        check=True,
##    )
##    .stdout.decode()
##    .strip()
##    .split("\n")
##) - {
##    "GITHUB_TOKEN",  # not used in the cli, just passed to the backend
##    "GITHUB_EVENT_PATH",  # TODO: mock this for more than just PR events
##    "GITHUB_xxx",  # not used, just an example in comments
##}

USED_GITHUB_VARS = {
    "GITHUB_HEAD_REF",
    "GITHUB_REPOSITORY",
    "GITHUB_RUN_ID",
    "GITHUB_SERVER_URL",
    "GITHUB_EVENT_NAME",
    "GITHUB_API_URL",
    "GITHUB_SHA",
    "GITHUB_WORKSPACE",
    "GITHUB_REF",
    "GITHUB_REPOSITORY_OWNER_ID",
    "GITHUB_ACTIONS",
    "GITHUB_REPOSITORY_ID",
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
def git_path_empty_repo(monkeypatch, tmp_path):
    """
    Initialize a git repo with no commits
    """
    repo_base = tmp_path / REPO_DIR_NAME
    repo_base.mkdir()

    monkeypatch.chdir(repo_base)
    subprocess.run(["git", "init"], check=True, capture_output=True)
    # Initialize State
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
    yield repo_base


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
def always_suppress_errors(mocker):
    mocker.patch.object(ScanHandler, "always_suppress_errors", False)


# Defining these fixtures to return functions allows the hostname (SEMGREP_URL) to be set for each test,
# permitting tests that use different base URLs to succeed using these mocks.
@pytest.fixture
def start_scan_mock_maker(
    requests_mock,
    scan_config,
    mocked_scan_id,
    enable_dependency_query,
):
    # TODO: fix the noqa above
    def _start_scan_func(
        semgrep_url: str = "https://semgrep.dev",
        product_ignored_files: Mapping[out.Product, List[str]] = {},  # noqa
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
                    "triage_ignored_syntactic_ids": [
                        "f3b21c38bc22a1f1f870d49fc3a40244"
                    ],
                    "triage_ignored_match_based_ids": [
                        "e536489e68267e16e71dd76a61e27815fd86a7e2417d96f8e0c43af48540a41d41e6acad52f7ccda83b5c6168dd5559cd49169617e3aac1b7ea091d8a20ebf12_0"
                    ],
                },
                "engine_params": {
                    "dependency_query": enable_dependency_query,
                    "product_ignored_files": [
                        [product.to_json(), ignores]
                        for product, ignores in product_ignored_files.items()
                    ],
                },
            }
        )
        return requests_mock.post(
            f"{semgrep_url}/api/cli/scans", json=start_scan_response.to_json()
        )

    return _start_scan_func


@pytest.fixture
def upload_results_mock_maker(requests_mock, mocked_scan_id, mocked_task_id):
    def _upload_results_func(semgrep_url: str = "https://semgrep.dev"):
        results_response = out.CiScanResultsResponse(errors=[], task_id=mocked_task_id)
        return requests_mock.post(
            f"{semgrep_url}/api/agent/scans/{mocked_scan_id}/results",
            json=results_response.to_json(),
        )

    return _upload_results_func


@pytest.fixture
def mocked_complete_response():
    return out.CiScanCompleteResponse(
        success=True, app_block_override=True, app_block_reason="Test Reason"
    )


@pytest.fixture
def complete_scan_mock_maker(requests_mock, mocked_scan_id, mocked_complete_response):
    def _complete_scan_func(semgrep_url: str = "https://semgrep.dev"):
        return requests_mock.post(
            f"{semgrep_url}/api/agent/scans/{mocked_scan_id}/complete",
            json=mocked_complete_response.to_json(),
        )

    return _complete_scan_func


@pytest.fixture
def scan_failure_mock_maker(requests_mock, mocked_scan_id):
    def _scan_failure_func(semgrep_url: str = "https://semgrep.dev"):
        return requests_mock.post(
            f"{semgrep_url}/api/agent/scans/{mocked_scan_id}/error",
            json=json.dumps({"exit_code": 0}),
        )

    return _scan_failure_func


@pytest.fixture
def mock_ci_api(
    start_scan_mock_maker, upload_results_mock_maker, complete_scan_mock_maker
):
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
        {  # Github full scan with custom tenant
            "CI": "true",
            **DEFAULT_GITHUB_VARS,
            "GITHUB_EVENT_NAME": "push",
            "GITHUB_REF": f"refs/heads/{BRANCH_NAME}",
            "GITHUB_BASE_REF": "",
            "GITHUB_HEAD_REF": "",
            "SEMGREP_URL": "https://tenantname.semgrep.dev",
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
        "github-push-with-app-url",
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
    start_scan_mock_maker,
    upload_results_mock_maker,
    complete_scan_mock_maker,
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

    start_scan_mock = start_scan_mock_maker(
        env.get("SEMGREP_URL", "https://semgrep.dev")
    )
    upload_results_mock = upload_results_mock_maker(
        env.get("SEMGREP_URL", "https://semgrep.dev")
    )
    complete_scan_mock = complete_scan_mock_maker(
        env.get("SEMGREP_URL", "https://semgrep.dev")
    )

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only"],
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
    prj_meta_json = scan_create_json["project_metadata"]
    scan_meta_json = scan_create_json["scan_metadata"]

    if "SEMGREP_COMMIT" in env:
        assert prj_meta_json["commit"] == env["SEMGREP_COMMIT"]
        prj_meta_json["commit"] = "sanitized semgrep commit"
    else:
        assert prj_meta_json["commit"] == head_commit
        prj_meta_json["commit"] = "sanitized"

    scan_meta_json["cli_version"] = "<sanitized version>"
    scan_meta_json["unique_id"] = "<sanitized id>"

    assert prj_meta_json["commit_timestamp"] == FROZEN_ISOTIMESTAMP.value

    if env.get("GITLAB_CI"):
        # If in a merge pipeline, base_sha is defined, otherwise is None
        gitlab_base_sha = (
            base_commit if env.get("CI_MERGE_REQUEST_TARGET_BRANCH_NAME") else None
        )
        if gitlab_base_sha != None:
            assert prj_meta_json["base_sha"] == gitlab_base_sha
            prj_meta_json["base_sha"] = "sanitized"

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
    start_scan_mock_maker,
    upload_results_mock_maker,
    complete_scan_mock_maker,
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

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only"],
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
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
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

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    # Scan the wrong thing first and verify we get more findings than expected (2 > 1)
    result = run_semgrep(
        subcommand="ci",
        options=["--no-force-color", "--no-suppress-errors", "--oss-only"],
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
        options=["--no-force-color", "--no-suppress-errors", "--oss-only"],
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
        options=["--no-suppress-errors", "--oss-only"],
        strict=False,
        assert_exit_code=1,
        env={"SEMGREP_APP_TOKEN": ""},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )
    snapshot.assert_match(
        result.as_snapshot(),
        "results.txt",
    )


# Testing semgrep ci on an empty repo, where the expected behavior
# is that the run succeeds
@pytest.mark.osemfail
def test_empty_repo_run(
    run_semgrep: RunSemgrep,
    start_scan_mock_maker,
    git_path_empty_repo,
    requests_mock,
    scan_config,
):
    requests_mock.get("https://semgrep.dev/p/something", text=scan_config)
    # Here we only test that the run exits with an exit code of 0
    # i.e the cli succeeding
    run_semgrep(
        "p/something",
        subcommand="ci",
        options=["--no-suppress-errors"],
        strict=False,
        assert_exit_code=0,  # This run must succeed
        env={
            "SEMGREP_APP_TOKEN": "",
            "SEMGREP_REPO_URL": REMOTE_REPO_URL,
        },
        use_click_runner=True,
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
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only", format],
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


# NOTE: in the future, the App should ideally *not* send such commented lines,
# but for now we have to do some filtering.
@pytest.mark.kinda_slow
@pytest.mark.parametrize("ignored_file", ["foo.py", "", "# foo.py"])
@pytest.mark.parametrize("ignored_product", [SAST_PRODUCT, SECRETS_PRODUCT])
@pytest.mark.osemfail
def test_app_ignore(
    git_tmp_path_with_commit,
    snapshot,
    ignored_file,
    ignored_product,
    run_semgrep: RunSemgrep,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    start_scan_mock = start_scan_mock_maker(
        "https://semgrep.dev",
        product_ignored_files={
            ignored_product: [ignored_file],
        },
    )
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only"],
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


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_with_dataflow_traces(
    git_tmp_path_with_commit,
    snapshot,
    run_semgrep: RunSemgrep,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--dataflow-traces", "--oss-only"],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        output_format=OutputFormat.SARIF,
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
    git_tmp_path_with_commit,
    snapshot,
    mock_autofix,
    nosem,
    run_semgrep: RunSemgrep,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only", nosem],
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


@pytest.mark.parametrize(
    "scan_config",
    [GENERIC_SECRETS_AND_REAL_RULE],
    ids=["generic_secrets_and_real_rule"],
)
@pytest.mark.osemfail
def test_generic_secrets_output(
    git_tmp_path_with_commit,
    snapshot,
    run_semgrep: RunSemgrep,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        target_name=None,
        strict=False,
        assert_exit_code=1,
        options=["--oss-only"],
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )
    snapshot.assert_match(
        result.as_snapshot(),
        "output.txt",
    )

    assert "real-rule-example" in result.raw_stdout
    # because it doesn't go in the "Blocking findings" section or otherwise,
    # the rule message doesn't show. these go straight to the App with minimal
    # CLI output
    assert "generic secrets rule message" not in result.raw_stdout


@pytest.mark.osemfail
def test_semgrep_managed_scan_id(run_semgrep: RunSemgrep, requests_mock):
    MANAGED_SCAN_ID = "12321"
    scan_create = requests_mock.post("https://semgrep.dev/api/cli/scans")
    run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only"],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={
            "SEMGREP_APP_TOKEN": "fake-key-from-tests",
            "SEMGREP_MANAGED_SCAN_ID": MANAGED_SCAN_ID,
        },
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )
    assert scan_create.call_count == 1
    request_body = scan_create.request_history[-1].json()
    assert request_body["scan_metadata"]["sms_scan_id"] == MANAGED_SCAN_ID


@pytest.mark.parametrize("mocked_scan_id", [None])
@pytest.mark.osemfail
def test_dryrun(
    tmp_path,
    git_tmp_path_with_commit,
    snapshot,
    run_semgrep: RunSemgrep,
    start_scan_mock_maker,
):
    _, base_commit, head_commit = git_tmp_path_with_commit

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--dry-run", "--no-suppress-errors", "--oss-only"],
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
        options=["--no-suppress-errors", "--oss-only"],
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
        "exit_code": 13,
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
        options=["--no-suppress-errors", "--oss-only"],
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
        options=["--no-suppress-errors", "--oss-only"],
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
        "exit_code": 2,
    }


@pytest.mark.parametrize("scan_config", [BAD_CONFIG], ids=["bad_config"])
@pytest.mark.osemfail
def test_bad_config(
    run_semgrep: RunSemgrep,
    mocker,
    git_tmp_path_with_commit,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
    scan_failure_mock_maker,
):
    """
    Test that bad rules has exit code > 1 and we notify the app.
    """

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")
    scan_failure_mock = scan_failure_mock_maker("https://semgrep.dev")

    # This is the function that notifies the app of the failure.
    report_failure = mocker.patch.object(ScanHandler, "report_failure")

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only"],
        target_name=None,
        strict=False,
        assert_exit_code=7,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )
    assert "Invalid rule schema" in result.stderr
    report_failure.assert_called_once()


@pytest.mark.parametrize("scan_config", [BAD_CONFIG], ids=["bad_config"])
@pytest.mark.osemfail
def test_bad_config_error_handler(
    run_semgrep: RunSemgrep,
    mocker,
    git_tmp_path_with_commit,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
    scan_failure_mock_maker,
):
    """
    Test that bad rules with --suppres-errors returns exit code 0
    and we notify the app.
    """
    # This is the function that traps all exceptions at the top level for
    # all commands.
    top_level_error_handler = mocker.spy(ErrorHandler, "send")

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")
    scan_failure_mock = scan_failure_mock_maker("https://semgrep.dev")

    # This is the function that notifies the app of the failure.
    report_failure = mocker.patch.object(ScanHandler, "report_failure")

    result = run_semgrep(
        subcommand="ci",
        options=["--oss-only"],
        target_name=None,
        strict=False,
        assert_exit_code=0,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        use_click_runner=True,
    )
    assert "Invalid rule schema" in result.stderr
    top_level_error_handler.assert_called_once_with(mocker.ANY, 7)
    report_failure.assert_called_once()


@pytest.mark.osemfail
def test_fail_scan_findings(
    run_semgrep: RunSemgrep,
    mocker,
    git_tmp_path_with_commit,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    """
    Test failure with findings has exit code == 1.

    Asserts that error logs are NOT sent to fail-open
    """
    mock_send = mocker.spy(ErrorHandler, "send")

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    run_semgrep(
        subcommand="ci",
        options=["--suppress-errors", "--oss-only"],
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
        options=["--no-suppress-errors", "--oss-only"],
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
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    """
    Test backend sending non-zero exit code on complete causes exit 1
    """
    mocker.patch.object(
        ScanHandler,
        "report_findings",
        return_value=out.CiScanCompleteResponse(True, True, "some reason to fail"),
    )

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only"],
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
        options=["--no-suppress-errors", "--oss-only"],
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
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only"],
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
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    mock_send = mocker.patch.object(Metrics, "_post_metrics")

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    run_semgrep(
        subcommand="ci",
        options=["--oss-only"],
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
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    repo_copy_base, base_commit, head_commit = git_tmp_path_with_commit

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only"],
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
        options=[
            "--no-suppress-errors",
            "--oss-only",
            "--baseline-commit",
            head_commit,
        ],
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
    ("subdir", "succeeds"),
    [
        ("org/examples", True),
        ("test/../org/examples/", True),
        ("org/", True),
        ("../org", False),
        ("..", False),
        ("/checkout_project_name/org/examples", False),
    ],
)
@pytest.mark.osemfail
def test_subdir(
    subdir,
    succeeds,
    git_tmp_path_with_commit,
    snapshot,
    mocker,
    run_semgrep: RunSemgrep,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
    scan_config,
    requests_mock,
):
    repo_copy_base, base_commit, head_commit = git_tmp_path_with_commit

    requests_mock.get("https://semgrep.dev/p/something", text=scan_config)

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    dir1 = repo_copy_base / "org"
    dir1.mkdir()
    dir2 = repo_copy_base / "test"
    dir2.mkdir()
    codedir = dir1 / "examples"
    codedir.mkdir()
    pyfile1 = codedir / "foo.py"
    pyfile1.write_text(f"x == 5\n")

    result = run_semgrep(
        subcommand="ci",
        options=[
            "--subdir",
            subdir,
            "--oss-only",
        ],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )
    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                head_commit,
            ]
        ),
        "output.txt",
    )
    if succeeds:
        findings_json = upload_results_mock.last_request.json()
        assert len(findings_json["findings"]) == 1


@pytest.mark.parametrize(
    "scan_config",
    [
        dedent(
            """
            rules:
              - id: supply-chain-parity-1
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
              - id: supply-chain-parity-2
                message: "found another dependency without a pattern"
                languages: [python]
                severity: ERROR
                r2c-internal-project-depends-on:
                    namespace: pypi
                    package: mypy
                    version: == 0.950
                metadata:
                    dev.semgrep.actions: [block]
                    sca-kind: upgrade-only
              - id: supply-chain-reachable-1
                message: "found a reachable vulnerability from a dependency"
                pattern: $X = 2
                languages: [python]
                severity: ERROR
                r2c-internal-project-depends-on:
                    namespace: pypi
                    package: mypy
                    version: == 0.950
                metadata:
                    dev.semgrep.actions: [block]
            """
        ).lstrip()
    ],
    ids=["config"],
)
@pytest.mark.osemfail
def test_reachable_and_unreachable_diff_scan_findings(
    git_tmp_path_with_commit,
    snapshot,
    mocker,
    run_semgrep: RunSemgrep,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    repo_copy_base, base_commit, head_commit = git_tmp_path_with_commit

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only"],
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
    assert len(findings_json["findings"]) == 3

    pyfile1 = repo_copy_base / "foo.py"
    pyfile1.write_text(f"x = 2\n")

    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", "add lockfile"], check=True, capture_output=True
    )
    new_head_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    result = run_semgrep(
        subcommand="ci",
        options=[
            "--no-suppress-errors",
            "--oss-only",
            "--baseline-commit",
            head_commit,
        ],
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
    assert len(findings_json["findings"]) == 1


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
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    """
    Verify that for any given product, there is a valid output
    """
    mocker.patch.object(ScanHandler, "enabled_products", enabled_products)

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        options=["ci", "--no-suppress-errors", "--oss-only"],
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


@pytest.mark.parametrize("oss_only", [True])
@pytest.mark.osemfail
def test_pro_diff_slow_rollout(
    run_semgrep: RunSemgrep,
    mocker,
    oss_only,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    """
    Verify that generic_slow_rollout enables pro diff scan
    """
    mocker.patch.object(ScanHandler, "generic_slow_rollout", True)
    mocker.patch.object(EngineType, "check_if_installed", return_value=True)
    mock_send = mocker.patch.object(Metrics, "add_diff_depth")

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    engine_flag_opt = ["--oss-only"] if oss_only else []

    result = run_semgrep(
        options=["ci", "--no-suppress-errors", *engine_flag_opt],
        target_name=None,
        strict=False,
        force_metrics_off=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,
    )
    if oss_only:
        mock_send.assert_not_called()
    else:
        mock_send.assert_called_once_with(2)


@pytest.mark.parametrize(
    "env",
    [
        {  # ci run with app token alice
            "SEMGREP_APP_TOKEN": DUMMY_APP_TOKEN_ALICE,
            "SEMGREP_SETTINGS_FILE": tempfile.NamedTemporaryFile().name,
            "EXISTING_SEMGREP_SETTINGS": False,
        },
        {  # ci run with app token bob
            "SEMGREP_APP_TOKEN": DUMMY_APP_TOKEN_BOB,
            "SEMGREP_SETTINGS_FILE": tempfile.NamedTemporaryFile().name,
            "EXISTING_SEMGREP_SETTINGS": False,
        },
        {  # ci run with app token but existing settings file
            "SEMGREP_APP_TOKEN": DUMMY_APP_TOKEN_BOB,
            "SEMGREP_SETTINGS_FILE": tempfile.NamedTemporaryFile().name,
            "EXISTING_SEMGREP_SETTINGS": True,
        },
    ],
    ids=["token-alice", "token-bob", "existing-settings"],
)
@pytest.mark.osemfail
def test_ci_uuid(
    env,
    run_semgrep: RunSemgrep,
    mocker,
):
    """
    Verify that the expected UUID is generated for a fresh CI run.
    When we don't have an existing settings file, we should generate a fixed UUID
    Otherwise, we should continue using the same UUID.
    """

    settings_file = env.get("SEMGREP_SETTINGS_FILE")
    # Check if we should mimic an existing settings file via simulation
    existing_settings = env.get("EXISTING_SEMGREP_SETTINGS")
    del env["EXISTING_SEMGREP_SETTINGS"]

    generated_uuid = generate_anonymous_user_id(env.get("SEMGREP_APP_TOKEN"))
    # Assume we will generate a new UUID from app_token
    expected_uuid = generated_uuid

    # Simulate the case where we have an existing settings file
    if existing_settings:
        make_settings_file(settings_file)
        # Update the expected UUID to be the one in the settings file
        expected_uuid = load_anonymous_user_id(settings_file)

    # We don't actually need to run a full scan here: just need to mock out the
    # first few steps of the scan process (1 sec vs 30 sec for this test suite)
    mocker.patch.object(ScanHandler, "start_scan", side_effect=Exception("Timeout"))

    result = run_semgrep(
        subcommand="ci",
        options=["--dry-run", "--no-suppress-errors", "--oss-only"],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env=env,
        use_click_runner=True,
    )

    # Check that the UUID in the settings file matches the expected UUID
    found_uuid = load_anonymous_user_id(settings_file)

    assert found_uuid is not None, "Expected UUID to be generated in settings"
    assert (
        found_uuid == expected_uuid
    ), f"Expected {expected_uuid} but found {found_uuid}"


@pytest.mark.osemfail
def test_fail_on_historical_scan_without_secrets(
    run_semgrep: RunSemgrep,
    snapshot,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--historical-secrets", "--no-suppress-errors", "--oss-only"],
        strict=False,
        env={"SEMGREP_APP_TOKEN": "fake-key-from-tests"},
        assert_exit_code=2,
        target_name=None,
        use_click_runner=True,
    )
    snapshot.assert_match(
        result.as_snapshot(),
        "output.txt",
    )


@pytest.mark.parametrize(
    "scan_config",
    [
        dedent(
            """
            rules:
              - id: supply-chain-parity-1
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
              - id: supply-chain-parity-2
                message: "found another dependency without a pattern"
                languages: [python]
                severity: ERROR
                r2c-internal-project-depends-on:
                    namespace: pypi
                    package: mypy
                    version: == 0.950
                metadata:
                    dev.semgrep.actions: [block]
                    sca-kind: upgrade-only
              - id: supply-chain-reachable-1
                message: "found a reachable vulnerability from a dependency"
                pattern: $X = 2
                languages: [python]
                severity: ERROR
                r2c-internal-project-depends-on:
                    namespace: pypi
                    package: mypy
                    version: == 0.950
                metadata:
                    dev.semgrep.actions: [block]
            """
        ).lstrip()
    ],
    ids=["config"],
)
@pytest.mark.osemfail
def test_existing_reachable_finding_deduplication(
    git_tmp_path_with_commit,
    snapshot,
    mocker,
    run_semgrep: RunSemgrep,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    repo_copy_base, _, base_commit = git_tmp_path_with_commit

    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    # Add vulnerability here so that it's already existing when we run a scan later
    pyfile1 = repo_copy_base / "foo.py"
    pyfile1.write_text(f"x = 2\n")

    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", "add reachable vulnerability"],
        check=True,
        capture_output=True,
    )
    vulnerable_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    result = run_semgrep(
        subcommand="ci",
        options=[
            "--no-suppress-errors",
            "--oss-only",
            "--baseline-commit",
            base_commit,
        ],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,
    )

    # This scan should have a reachable finding which we added earlier
    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                vulnerable_commit,
                vulnerable_commit[:7],
                base_commit,
            ]
        ),
        "base_output.txt",
    )

    findings_json = upload_results_mock.last_request.json()
    assert len(findings_json["findings"]) == 1

    # Since we want to ensure that reachability works on git tracked files, we modify the
    # same file with a safe change (but do not fix the vulnerability) so that the reachablity
    # check can be done on the same file by the baseline scanner
    pyfile1 = repo_copy_base / "foo.py"
    pyfile1.write_text(f"x = 2\nprint('hello')\n")

    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", "add another thing"], check=True, capture_output=True
    )
    non_vulnerable_head_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    result = run_semgrep(
        subcommand="ci",
        options=[
            "--no-suppress-errors",
            "--oss-only",
            "--baseline-commit",
            vulnerable_commit,
        ],
        target_name=None,
        strict=False,
        assert_exit_code=None,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,  # TODO: probably because rely on some mocking
    )

    # This scan should have no findings since reachability analysis in the baseline
    # should also raise the same finding (leading to deduplication downstream)
    snapshot.assert_match(
        result.as_snapshot(
            mask=[
                non_vulnerable_head_commit,
                non_vulnerable_head_commit[:7],
                vulnerable_commit,
            ]
        ),
        "new_output.txt",
    )
    findings_json = upload_results_mock.last_request.json()
    assert len(findings_json["findings"]) == 0


@pytest.mark.parametrize("always_suppress_errors", [True, False], indirect=True)
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
          metadata:
            source: https://semgrep.dev/r/eqeq-bad
        - id: pattern-parse-error
          pattern: $X ==
          message: "useless comparison to 5"
          languages: [python]
          severity: ERROR
     """
        )
    ],
)
@pytest.mark.osemfail
def test_always_suppress_errors(
    run_semgrep: RunSemgrep,
    snapshot,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
    always_suppress_errors,
):
    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        options=["ci", "--oss-only", "--no-suppress-errors"],
        strict=False,
        assert_exit_code=0 if always_suppress_errors else 2,
        env={"SEMGREP_APP_TOKEN": "fake_key"},
        use_click_runner=True,
    )


@pytest.mark.parametrize(
    "mocked_complete_response",
    [
        # Should produce output showing all findings as blocking
        out.CiScanCompleteResponse(
            success=True,
            app_block_override=True,
            app_block_reason="",
            app_blocking_match_based_ids=[
                out.MatchBasedId(
                    "186b96f64aca90b7f5a9c75f2e44538885d0e727ed3161ef7b6d46c40b3d078acfc8859b290e118cb8ca42f5b41e61afe73b0f416f47a2f16abce67b1be307d3_0"
                ),
                out.MatchBasedId(
                    "2c4ff12fcdf80ef1c00dd0f566ae102d792c7ba68e560d70f111aae3b3216c0b1b943e74d2ce29c0361f1fbc37bd4e9aafd32c3435a36c61b8bd3963efe0d7a1_0"
                ),
            ],
        ),
        # Should produce output showing all findings as blocking, and also mention the 'Test reason'
        out.CiScanCompleteResponse(
            success=True,
            app_block_override=True,
            app_block_reason="Test reason",
            app_blocking_match_based_ids=[
                out.MatchBasedId(
                    "186b96f64aca90b7f5a9c75f2e44538885d0e727ed3161ef7b6d46c40b3d078acfc8859b290e118cb8ca42f5b41e61afe73b0f416f47a2f16abce67b1be307d3_0"
                ),
                out.MatchBasedId(
                    "2c4ff12fcdf80ef1c00dd0f566ae102d792c7ba68e560d70f111aae3b3216c0b1b943e74d2ce29c0361f1fbc37bd4e9aafd32c3435a36c61b8bd3963efe0d7a1_0"
                ),
            ],
        ),
    ],
)
@pytest.mark.osemfail
def test_app_blocked_findings(
    git_tmp_path_with_commit,
    snapshot,
    mocker,
    run_semgrep: RunSemgrep,
    start_scan_mock_maker,
    complete_scan_mock_maker,
    upload_results_mock_maker,
):
    start_scan_mock = start_scan_mock_maker("https://semgrep.dev")
    complete_scan_mock = complete_scan_mock_maker("https://semgrep.dev")
    upload_results_mock = upload_results_mock_maker("https://semgrep.dev")

    result = run_semgrep(
        subcommand="ci",
        options=["--no-suppress-errors", "--oss-only"],
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
