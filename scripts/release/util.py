import os
import sys
from typing import Any
from typing import Callable
from typing import Optional
from typing import Sequence

import click
import sh


_RELEASE_VERSION: Optional[str] = None


def abort(code: int, message: str) -> None:
    click.secho(message, err=True, fg="red")
    sys.exit(code)


def debug_bake(f: Callable[..., str]) -> Callable[..., str]:
    def runner(*args: Any, **kwargs: Any) -> str:
        res = f(*args, **kwargs)
        click.echo(res, err=True)
        return res

    return runner


def release_version() -> str:
    global _RELEASE_VERSION

    if _RELEASE_VERSION:
        return _RELEASE_VERSION

    _RELEASE_VERSION = os.getenv("RELEASE")
    if not _RELEASE_VERSION:
        abort(2, "Must be run with RELEASE=X.X.X")
    click.echo(f"Creating branch for release {_RELEASE_VERSION}", err=True)
    return _RELEASE_VERSION


def release_branch_name() -> str:
    return f"release-{release_version()}"


git = debug_bake(sh.git)


def diffs() -> Sequence[Sequence[str]]:
    return [
        d.strip().split(" ")
        for d in git("status", "--porcelain").strip().split("\n")
        if d
    ]
