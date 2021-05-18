import subprocess

from util import abort
from util import diffs
from util import git
from util import release_version


BUMP_FILES = {"semgrep/semgrep/__init__.py", "setup.py"}


def bump_and_push(version: str):
    subprocess.run(["make", "bump"], env={"SEMGREP_VERSION": version})
    changed_files = [d[1] for d in diffs()]
    if set(changed_files) != BUMP_FILES:
        abort(2, f"Expected diffs only in {BUMP_FILES}")
    git("add", *BUMP_FILES)
    git("commit", "-m", f"bump version to ${version}")
    git("push", "origin", "HEAD")


if __name__ == "__main__":
    bump_and_push(release_version())
