from typing import Dict
from typing import List

import requests

from semgrep.constants import RETURNTOCORP_LEVER_URL


def print_job_postings() -> None:
    # http get postings
    response = requests.get(RETURNTOCORP_LEVER_URL)
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

    print(
        "We love that you're using Semgrep. Would you like to... work on it full time?! 😄 "
    )
    print("")
    # iterate over teams alphabetically
    for team in sorted(postings_dict):
        team_character_count = len(team)

        print_n_hashtags(team_character_count + 4)
        print("#", team, "#")
        print_n_hashtags(team_character_count + 4)
        print("")

        for posting in postings_dict[team]:
            print("*", posting["title"] + ":", posting["url"])

        print("")


def print_n_hashtags(n: int) -> None:
    s = ""
    for _i in range(0, n):
        s += "#"
    print(s)
