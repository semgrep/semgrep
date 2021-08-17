from util import abort
from util import diffs
from util import release_version

# from util import git


BUMP_FILES = {"semgrep/semgrep/__init__.py", "setup.py", "CHANGELOG.md"}


def add_commit_push(version: str):
    """
    Add all changes to files in BUMP_FILES, creates a release commit, and pushes said commit

    Assumes all files in BUMP_FILES were modified
    """
    changed_files = [d[1] for d in diffs()]
    if set(changed_files) != BUMP_FILES:
        abort(2, f"Expected diffs only in {BUMP_FILES}")
    # git("add", *BUMP_FILES)
    # git("commit", "-m", f"Release {version}")
    # git("push", "origin", "HEAD")


if __name__ == "__main__":
    add_commit_push(release_version())
