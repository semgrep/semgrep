"""
Contributors.py

This modules handles the collection, cleansing, and submission of contributor
information for logged-in users using semgrep CLI.
"""


from dataclasses import Field
from datetime import UTC, datetime, timedelta
from enum import Enum
from typing import Iterator, Sequence

from attrs import define, field, frozen
from pydriller import Commit, Repository
from rich import box
from rich.columns import Columns
from rich.padding import Padding
from rich.table import Table

from semgrep.console import console, Title
from semgrep.target_manager import TargetManager, PATHS_ALWAYS_SKIPPED
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


IGNORED_COMMITTERS = ["semgrep.dev"]
IGNORED_AUTHORS = ["semgrep-ci[bot]", "dependabot[bot]", "r2c-argo[bot]"]
DEFAULT_LOOKBACK = timedelta(days=45)


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


@frozen(eq=True)
class Contribution:
    project: str
    contributor: Contributor


@define(eq=False)
class ContributionsResponse:
    contributions: list[Contribution]
    contributors_count: int = field(init=False)
    # from_timestamp: datetime
    # to_timestamp: datetime
    # from_commit: str
    # to_commit: str

    def __attrs_post_init__(self) -> None:
        self.contributors_count = len(self.contributors)

    @property
    def contributors(self) -> list[Contributor]:
        return list(
            set(contribution.contributor for contribution in self.contributions)
        )

    @property
    def projects(self) -> list[str]:
        return list(set(contribution.project for contribution in self.contributions))

    def print_contributors(self) -> None:
        console.print(Title(f"{len(self.contributions)} Contributions"))
        console.print(
            Padding(
                f"Found the following {len(self.contributions)} contributions across the {len(self.projects)} scanned projects:",
                (1, 0),
            ),
            deindent=1,
        )

        table = Table(box=box.SIMPLE, show_edge=False)
        table.add_column("Project")
        table.add_column("Name")
        table.add_column("Email")

        sorted_contributions = sorted(
            self.contributions,
            key=lambda c: (c.project, c.contributor.name, c.contributor.email),
        )
        for contribution in sorted_contributions:
            table.add_row(
                contribution.project,
                contribution.contributor.name,
                contribution.contributor.email,
            )

        columns = Columns([table], padding=(1, 8))
        console.print(Padding(columns, (1, 0)), deindent=1)


@define(eq=False)
class ContributionManager:
    """

    Contributors are analyzed at the time of a scan and will be collected
    from either the entire git history (for a full scan) or the output of `git diff`
    when running a diff scan.
    """

    target_manager: TargetManager

    since: datetime | None = None
    to: datetime | None = None
    from_commit: str | None = None
    to_commit: str | None = None

    repository: Repository = field(init=False)

    def __attrs_post_init__(self) -> None:
        if not self.since:
            self.since = datetime.utcnow() - DEFAULT_LOOKBACK

        self.repository = Repository(
            path_to_repo=[
                str(target.path.resolve()) for target in self.target_manager.targets
            ],
            since=self.since,
        )

    def collect_contributions(self) -> ContributionsResponse:
        contributions: list[Contribution] = []

        for commit in self._iter_commits():
            contributor = Contributor(
                name=commit.author.name,
                email=commit.author.email,
            )
            contribution = Contribution(
                contributor=contributor,
                project=commit.project_name,
            )
            contributions.append(contribution)

        return ContributionsResponse(contributions=list(set(contributions)))

    def _iter_commits(self) -> Iterator[Commit]:
        yield from filter(
            self._is_valid_commit,
            self.repository.traverse_commits(),
        )

    def _is_valid_commit(self, commit: Commit) -> bool:
        if commit.committer.name in IGNORED_COMMITTERS:
            return False

        if commit.author.name in IGNORED_AUTHORS:
            return False

        return True
