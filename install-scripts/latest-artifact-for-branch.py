#!/usr/bin/env python3
import json
import os
import sys
import tarfile
import tempfile
import urllib.request
from pathlib import Path
from shutil import copyfile
from shutil import move
from typing import Any
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


def get_latest_artifact_url(
    branch: str, workflow: str, artifact_name: Optional[str]
) -> Optional[str]:
    workflows = download_json("/actions/workflows")["workflows"]
    workflow_objs = [w for w in workflows if w["name"] == workflow]
    if not workflow_objs:
        return None
    workflow_obj = workflow_objs[0]

    workflow_runs = download_json(f"/actions/workflows/{workflow_obj['id']}/runs")[
        "workflow_runs"
    ]
    successful_runs = [run for run in workflow_runs if run["head_branch"] == branch]
    if not successful_runs:
        return None

    last_successful_run = successful_runs[0]
    print(f'Found a release from {last_successful_run["created_at"]}', file=sys.stderr)
    artifacts = download_json(last_successful_run["artifacts_url"])["artifacts"]
    if artifact_name:
        artifacts = [a for a in artifacts if a["name"].startswith(artifact_name)]
    if not artifacts:
        return None
    return str(artifacts[0]["archive_download_url"])


def make_executable(path: str) -> None:
    mode = os.stat(path).st_mode
    mode |= (mode & 0o444) >> 2  # copy R bits to X
    os.chmod(path, mode)


def download_artifact(url: str) -> str:
    tempdir = tempfile.mkdtemp()
    request = urllib.request.Request(
        url=url, headers={"authorization": f"Bearer {GITHUB_TOKEN}"}
    )
    response = urllib.request.urlopen(request)
    with tempfile.TemporaryFile() as tf:
        tf.write(response.read())
        tf.flush()
        with ZipFile(tf) as zf:
            zf.extractall(path=tempdir)
            zf.close()
    assert os.path.exists(tempdir)
    return tempdir


def install_semgrep_core(url: str) -> None:
    tempdir = download_artifact(url)
    with tarfile.open(Path(tempdir) / "artifacts.tar.gz") as tar:
        tar.extract("semgrep-files/semgrep-core", path=tempdir)
    binary_path = "/usr/local/bin/semgrep-core"
    if not os.path.exists(binary_path):
        copyfile(
            Path(tempdir) / "semgrep-files" / "semgrep-core", binary_path,
        )
        make_executable(binary_path)

    else:
        print(
            "Refusing to overwite existing file for semgrep-core!", file=sys.stderr,
        )
        sys.exit(1)


def download_benchmarks(url: str, output: str) -> None:
    tempdir = download_artifact(url)
    assert os.path.exists(tempdir)
    _mergedir(Path(tempdir), Path(output))


def _mergedir(src: Path, target: Path) -> None:
    if target.is_dir():
        for path in os.listdir(src):
            subtarget = target / path
            if subtarget.exists():
                _mergedir(src / path, target / path)
            else:
                move(str(src / path), target / path)
    else:
        print(f"Warning: duplicates files {target}", file=sys.stderr)


if __name__ == "__main__":
    # If there is a HEAD_REF (this branch) use that.
    # Otherewise try to use the branch we're being merged into
    # if we're in a weird state, just use master
    if os.environ.get("SEMGREP_CORE"):
        branch_options = [
            os.environ.get("GITHUB_HEAD_REF"),
            os.environ.get("GITHUB_BASE_REF"),
            "develop",
        ]
        branches: List[str] = [b for b in branch_options if b]
        workflow = os.environ.get("WORKFLOW", "Tests")
        for branch in branches:
            print(f"Downloading the semgrep-core binary for {branch}", file=sys.stderr)
            url = get_latest_artifact_url(branch, workflow, "semgrep-ubuntu")
            if url is not None:
                install_semgrep_core(url)
                print(f"Installed prebuilt asset from {branch}", file=sys.stderr)
                sys.exit(0)
            else:
                print(f"Tried {branch} but no prebuilt asset existed", file=sys.stderr)
        print(
            f"Could not find a semgrep-core asset on any of {branches}", file=sys.stderr
        )
        sys.exit(1)
    elif os.environ.get("BENCHMARK"):
        branch = os.environ["BRANCH"]
        workflow = os.environ.get("WORKFLOW", "Incorporate Benchmark Data")
        output = os.environ.get("OUT_DIR", "benchmarks")
        url = get_latest_artifact_url(branch, workflow, None)
        if not os.path.exists(output):
            os.mkdir(output)
        if url is not None:
            download_benchmarks(url, output)
            print(f"saved benchmark data to {output}", file=sys.stderr)
        else:
            print(f"No benchmark data found", file=sys.stderr)
    else:
        print("Nothing to do.", file=sys.stderr)
        sys.exit(1)
