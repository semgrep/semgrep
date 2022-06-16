from typing import Dict
from typing import List

import click
import requests

from semgrep.constants import RETURNTOCORP_LEVER_URL


def print_job_postings() -> None:
    # http get postings
    response = requests.get(RETURNTOCORP_LEVER_URL, timeout=30)
    postings = response.json()

    # organise per team
    postings_dict: Dict[str, List[Dict[str, str]]] = {}
    for posting in postings:
        team = posting["categories"]["team"]
        if team not in postings_dict:
            postings_dict[team] = []

        title = posting["text"]
        url = posting["hostedUrl"]
        postings_dict[team].append({"title": title, "url": url})

    click.echo(
        "We love that you're using Semgrep. Would you like to... work on it full time?! 😄 ",
        err=True,
    )
    click.echo("", err=True)
    # iterate over teams alphabetically
    for team in sorted(postings_dict):
        team_character_count = len(team)

        click.echo((team_character_count + 4) * "#", err=True)
        click.echo("# " + team + " #", err=True)
        click.echo((team_character_count + 4) * "#", err=True)
        click.echo("", err=True)

        for posting in postings_dict[team]:
            click.echo("* " + posting["title"] + ": " + posting["url"], err=True)

        click.echo("", err=True)

    click.echo("", err=True)
    click.echo(
        "PS: Just wanted to apply autofixes? Use --autofix instead of --apply", err=True
    )
