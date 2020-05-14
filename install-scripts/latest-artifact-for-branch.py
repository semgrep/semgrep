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
from typing import Optional
from zipfile import ZipFile

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


def get_latest_artifact_url(branch: str, workflow: str) -> Optional[str]:
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
        return None

    last_successful_run = successful_runs[0]
    print(f'Found a release from {last_successful_run["created_at"]}', file=sys.stderr)
    artifacts = download_json(last_successful_run["artifacts_url"])["artifacts"]
    return str(artifacts[0]["archive_download_url"])


def make_executable(path: str) -> None:
    mode = os.stat(path).st_mode
    mode |= (mode & 0o444) >> 2  # copy R bits to X
    os.chmod(path, mode)


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

        binary_path = "/usr/local/bin/semgrep-core"
        if not os.path.exists(binary_path):
            copyfile(
                Path(tempdir) / "semgrep-files" / "semgrep-core", binary_path,
            )
            make_executable(binary_path)

        else:
            print(
                "Refusing to overwite existing file for semgrep-core!", file=sys.stderr
            )
            sys.exit(1)


if __name__ == "__main__":
    # If there is a HEAD_REF (this branch) use that.
    # Otherewise try to use the branch we're being merged into
    # if we're in a weird state, just use master
    branch_options = [
        os.environ.get("GITHUB_HEAD_REF"),
        os.environ.get("GITHUB_BASE_REF"),
        "develop",
    ]
    branches: List[str] = [b for b in branch_options if b]
    workflow = os.environ.get("WORKFLOW", "release-ubuntu")
    for branch in branches:
        print(f"Downloading the semgrep-core binary for {branch}", file=sys.stderr)
        url = get_latest_artifact_url(branch, workflow)
        if url is not None:
            download_extract_install(url)
            print(f"Installed prebuilt asset from {branch}", file=sys.stderr)
            sys.exit(0)
        else:
            print(f"Tried {branch} but no prebuilt asset existed", file=sys.stderr)
