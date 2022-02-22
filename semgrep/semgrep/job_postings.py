
import requests

from semgrep.constants import RETURNTOCORP_LEVER_URL
from semgrep.types import JsonObject


def print_job_postings() -> None:
    # http get postings
    response = requests.get(RETURNTOCORP_LEVER_URL)
    postings = response.json()

    # organise per team
    postings_dict = {}
    for posting in postings:
        team = posting["categories"]["team"]
        if team not in postings_dict:
            postings_dict[team] = []

        title = posting["text"]
        url = posting["hostedUrl"]
        postings_dict[team].append({"title": title, "url": url})

    # iterate over teams alphabetically
    for team in sorted(postings_dict):
        team_character_count = len(team)

        print_n_hashtags(team_character_count+4)
        print("#",team,"#")
        print_n_hashtags(team_character_count+4)
        print("")
        
        for posting in postings_dict[team]:
            print("*", posting["title"] + ":",posting["url"])

        print("")

def print_n_hashtags(n: int) -> None:
    s = ""
    for i in range(0,n):
        s += "#"
    print(s)

