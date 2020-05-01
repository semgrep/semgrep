#!/usr/bin/env python3
import hashlib
import json
import os
import sys
import tarfile
import tempfile
import urllib.request
from pathlib import Path
from shutil import copyfile
from typing import Any
from typing import Callable
from typing import Dict
from typing import List
from zipfile import ZipFile

print(os.environ)

GITHUB_TOKEN = os.environ["AUTH_TOKEN"]
repo = os.environ.get("GITHUB_REPOSITORY", "returntocorp/semgrep")

API_BASE_URL = f"https://api.github.com/repos/{repo}"


def download_json(url: str) -> Any:
    if not url.startswith("https"):
        url = f"{API_BASE_URL}{url}"

    request = urllib.request.Request(
        url=url, headers={"authorization": f"Bearer {GITHUB_TOKEN}"}
    )
    return json.load(urllib.request.urlopen(request))


def get_latest_artifact_url(branch: str, workflow: str) -> str:
    workflows = download_json("/actions/workflows")["workflows"]
    workflow_objs = [w for w in workflows if w["name"] == workflow]
    if not workflow_objs:
        print(f'No workflow with name: "{workflow}"', file=sys.stderr)
        sys.exit(1)
    workflow_obj = workflow_objs[0]

    workflow_runs = download_json(f"/actions/workflows/{workflow_obj['id']}/runs")[
        "workflow_runs"
    ]
    successful_runs = [
        run
        for run in workflow_runs
        if run["conclusion"] == "success" and run["head_branch"] == branch
    ]
    if not successful_runs:
        print(f'No successful run for "{workflow}" on "{branch}"', file=sys.stderr)
        sys.exit(1)

    last_successful_run = successful_runs[0]
    print(f'Found a release from {last_successful_run["created_at"]}', file=sys.stderr)
    artifacts = download_json(last_successful_run["artifacts_url"])["artifacts"]
    return str(artifacts[0]["archive_download_url"])


def download_extract_install(url: str) -> None:
    with tempfile.TemporaryDirectory() as tempdir:
        request = urllib.request.Request(
            url=url, headers={"authorization": f"Bearer {GITHUB_TOKEN}"}
        )
        response = urllib.request.urlopen(request)
        with open("artifacts.zip", "wb") as tf:
            tf.write(response.read())
        with ZipFile(tf.name) as zf:
            zf.extractall()
            zf.close()
        with tarfile.open("artifacts.tar.gz") as tar:
            tar.extract("semgrep-files/semgrep-core", path=tempdir)

        if not os.path.exists("/usr/local/bin/semgrep-core"):
            copyfile(
                Path(tempdir) / "semgrep-files" / "semgrep-core",
                "/usr/local/bin/semgrep-core",
            )
        else:
            print(
                "Refusing to overwite existing file for semgrep-core!", file=sys.stderr
            )
            sys.exit(1)


if __name__ == "__main__":
    branch = os.environ.get("BRANCH", "develop")
    workflow = os.environ.get("WORKFLOW", "release-ubuntu-16-04")
    url = get_latest_artifact_url(branch, workflow)
    download_extract_install(url)
