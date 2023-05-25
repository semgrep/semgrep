"""
Contributors.py

This modules handles the collection, cleansing, and submission of contributor
information for logged-in users using semgrep CLI.
"""


from datetime import UTC, datetime, timedelta

from attrs import define, field, frozen
from pydriller import Repository
import requests
from rich import box
from rich.columns import Columns
from rich.padding import Padding
from rich.table import Table

from semgrep.console import console, Title
from semgrep.state import get_state
from semgrep.target_manager import TargetManager
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

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
    commit: str
    timestamp: datetime
    contributor: Contributor


@define(eq=False)
class ContributionsResponse:
    contributions: list[Contribution]
    contributions_count: int
    contributors: list[Contributor]
    contributors_count: int
    projects: list[str]
    projects_count: int

    def print(self) -> None:
        console.print(Title(f"{self.contributions_count} Contributions"))
        console.print(
            Padding(
                f"Found the following {self.contributions_count} contributions across the {self.projects_count} scanned projects:",
                (1, 0),
            ),
            deindent=1,
        )

        table = Table(box=box.SIMPLE, show_edge=False)
        table.add_column("Project")
        table.add_column("Name")
        table.add_column("Email")
        table.add_column("Timestamp")
        table.add_column("Commit")

        sorted_contributions = sorted(
            self.contributions,
            key=lambda c: (
                c.project,
                (c.contributor.name or "").lower(),
                (c.contributor.email or "").lower(),
            ),
        )
        for contribution in sorted_contributions:
            table.add_row(
                contribution.project,
                contribution.contributor.name,
                contribution.contributor.email,
                str(contribution.timestamp),
                contribution.commit,
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
        contributors: list[Contributor] = []
        projects: list[str] = []

        for commit in self.repository.traverse_commits():
            contributor = Contributor(
                name=commit.author.name,
                email=commit.author.email,
            )
            if contributor not in contributors:
                contributors.append(contributor)

            if (project_name := commit.project_name) not in projects:
                projects.append(project_name)

            contribution = Contribution(
                contributor=contributor,
                project=project_name,
                commit=commit.hash,
                timestamp=commit.author_date.astimezone(UTC),
            )
            if contribution not in contributions:
                contributions.append(contribution)

        return ContributionsResponse(
            contributions=contributions,
            contributions_count=len(contributions),
            projects=projects,
            projects_count=len(projects),
            contributors=contributors,
            contributors_count=len(contributors),
        )

    def collect_latest_contributions(self) -> ContributionsResponse:
        contributions_cache: dict[Contributor, datetime] = {}
        latest_contributions_cache: dict[Contributor, Contribution] = {}

        contributions = self.collect_contributions()
        for contribution in contributions.contributions:
            if (contributor := contribution.contributor) in contributions_cache:
                if (
                    contribution.timestamp
                    > latest_contributions_cache[contributor].timestamp
                ):
                    contributions_cache[contributor] = contribution.timestamp
                    latest_contributions_cache[contributor] = contribution
            else:
                contributions_cache[contributor] = contribution.timestamp
                latest_contributions_cache[contributor] = contribution

        latest_contributions = list(latest_contributions_cache.values())
        return ContributionsResponse(
            contributions=latest_contributions,
            contributions_count=len(latest_contributions),
            projects=contributions.projects,
            projects_count=contributions.projects_count,
            contributors=contributions.contributors,
            contributors_count=contributions.contributors_count,
        )

    @classmethod
    def report_contributions(cls, contributions: list[Contribution]) -> bool:
        request_body = {
            "contributions": [contribution.asdict() for contribution in contributions]
        }

        state = get_state()
        response = state.app_session.post(
            f"{state.env.semgrep_url}/api/billing/contributors",
            json=request_body,
        )

        try:
            response.raise_for_status()
        except requests.RequestException:
            logger.error(f"API server returned this error: {response.text}")
            return False

        return True
