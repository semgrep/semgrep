"""
Contributors.py

This modules handles the collection, cleansing, and submission of contributor
information for logged-in users using semgrep CLI.
"""


from datetime import datetime
from enum import Enum
from typing import Iterator, Sequence

from attrs import define, field, frozen
from pydriller import Commit, Repository
from rich import box
from rich.columns import Columns
from rich.padding import Padding
from rich.table import Table

from semgrep.console import console
from semgrep.target_manager import TargetManager

IGNORED_COMMITTERS = ["semgrep.dev"]
IGNORED_AUTHORS = ["semgrep-ci[bot]"]


class ContributorSource(Enum):
    author = "author"
    user = "user"


@frozen(eq=True)
class Contributor:
    """
    Represents a single actor who has contributed in some way to a project scanned with
    Semgrep.

    Contributors can be any of:
    1. A commit author (per related commits via `git log`)
    2. A committer (per related commits via `git log`)
    3. The logged in user
    """

    name: str | None
    email: str | None
    source: ContributorSource


@frozen(eq=True)
class Contribution:
    contributor: Contributor
    commit_hash: str
    commit_date: datetime


@define(eq=False)
class ContributorManager:
    """

    Contributors are analyzed at the time of a scan and will be collected
    from either the entire git history (for a full scan) or the output of `git diff`
    when running a diff scan.
    """

    target_manager: TargetManager
    repositories: Sequence[Repository] = field(init=False)

    def __attrs_post_init__(self) -> None:
        self.repositories = [
            Repository(str(target.path.resolve()))
            for target in self.target_manager.targets
        ]

    def collect_contributors(self) -> Sequence[Contributor]:
        contributors: list[Contributor] = []

        for commit in self._traverse_commits():
            contributor = Contributor(
                name=commit.author.name,
                email=commit.author.email,
                source=ContributorSource.author,
            )
            contribution = Contribution(
                contributor=contributor,
                commit_hash=commit.hash,
                commit_date=commit.author_date,
            )

            contributors.append(contributor)

        return list(set(contributors))

    @classmethod
    def print(cls, contributors: Sequence[Contributor]) -> None:
        table = Table(box=box.SIMPLE, show_edge=False)
        table.add_column("Name")
        table.add_column("Email")
        table.add_column("Source")

        sorted_contributors = sorted(
            contributors, key=lambda c: (c.name, c.email, c.source)
        )
        for contributor in sorted_contributors:
            table.add_row(contributor.name, contributor.email, contributor.source.value)

        columns = Columns([table], padding=(1, 8))
        console.print(Padding(columns, (1, 0)), deindent=1)

    def _traverse_commits(self) -> Iterator[Commit]:
        for repository in self.repositories:
            for commit in repository.traverse_commits():
                if (
                    commit.committer.name not in IGNORED_COMMITTERS
                    and commit.author.name not in IGNORED_AUTHORS
                ):
                    yield commit
