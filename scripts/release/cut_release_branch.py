from util import abort
from util import diffs
from util import git
from util import release_branch_name


def cut(release_branch: str):
    if diffs():
        abort(
            2,
            f"Git checkout is not clean. Please stash all changes and retry. Changes:\n{diffs()}",
        )
    git("checkout", "develop")
    git("pull", "--ff-only")
    git("submodule", "update", "--recursive")
    git("checkout", "-b", release_branch)
    git("push", "origin", "HEAD")


if __name__ == "__main__":
    cut(release_branch_name())
