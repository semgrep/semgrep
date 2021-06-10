import json
import os
import sys
from pathlib import Path
from typing import List

import requests


def send_comment(message: str, github_token: str, pull_request_number: str) -> None:
    """

    Assumes commithash is in GITHUB_EVENT_PATH file at obj["pull_request"]["head"]["sha"]

    If pull_request_number is "" then does nothing
    """
    if pull_request_number == "":
        return

    session = requests.Session()
    session.headers["Authorization"] = f"Token {github_token}"
    url = f"https://api.github.com/repos/returntocorp/semgrep/issues/{pull_request_number}/comments"
    session.headers["Accept"] = "application/vnd.github.comfort-fade-preview+json"

    commit_id = json.loads(Path(os.getenv("GITHUB_EVENT_PATH")).read_text())[
        "pull_request"
    ]["head"]["sha"]
    pr_comment_payload = {
        "body": message,
        "commit_id": commit_id,
    }
    r = session.post(url, json=pr_comment_payload)

    # Raise if unable to comment
    print(message)
    r.raise_for_status()


def read_timing(filename: Path) -> List[float]:
    print(f"Reading {filename}")
    times = filename.read_text()
    return [float(t) for t in times.split()]


def main() -> None:
    # There are two versions of each timing to improve stability
    baseline_timing_file_1 = Path(sys.argv[1])
    baseline_timing_file_2 = Path(sys.argv[2])
    latest_timing_file_1 = Path(sys.argv[3])
    latest_timing_file_2 = Path(sys.argv[4])
    github_token = sys.argv[5]

    # Note this only defined in pull requests
    pull_request_number = sys.argv[6] if len(sys.argv) >= 7 else ""

    baseline_times = zip(
        read_timing(baseline_timing_file_1), read_timing(baseline_timing_file_2)
    )
    baseline_times = [max(t1, t2) for t1, t2 in baseline_times]
    latest_times = zip(
        read_timing(latest_timing_file_1), read_timing(latest_timing_file_2)
    )
    latest_times = [min(t1, t2) for t1, t2 in latest_times]

    total_baseline = 0.0
    total_latest = 0.0
    for baseline_time, latest_time in zip(baseline_times, latest_times):
        print(f"Baseline: {baseline_time}, Latest: {latest_time}")

        # Assert latest time is not more than 20% slower than baseline
        # or is within a fixed "probably environmental" range
        assert latest_time < baseline_time * 1.2 or latest_time - baseline_time < 5.0
        total_baseline += baseline_time
        total_latest += latest_time

        # Send PR comment if performance is slower but not past blocking threshold
        if latest_time > baseline_time * 1.1:
            send_comment(
                f"Potential non-blocking slowdown latest time {latest_time} is over 12 percent slower than baseline {baseline_time}. See run output for more details",
                github_token,
                pull_request_number,
            )

    # Assert the rules in aggregate are not more than 6% slower than baseline
    assert total_latest < total_baseline * 1.06


if __name__ == "__main__":
    main()
