#!/usr/bin/env python3
import os
import sys

import requests
import yaml


def upload_rule(rule_file):
    token = os.getenv("SEMGREP_TOKEN")

    if token is None:
        print("semgrep token not found")
        sys.exit(1)
    session = requests.Session()
    session.headers["Authorization"] = f"Bearer {token}"

    with open(rule_file) as file:
        rule = yaml.safe_load(file)

    req_json = {
        "definition": rule,
        "language": rule["rules"][0]["languages"][0],
        "deployment_id": 1,
    }

    print(req_json)
    res = session.post("http://localhost:5000/api/registry/rule", json=req_json)
    print(res)

    created_rule = res.json()

    print(
        f"You can find your private rule athttp://localhost:3000/r/{created_rule['path']}"
    )


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Uploads file as")
        print("Skips directories and files prefixed with a .")
        print(f"Usage {sys.argv[0]} [input rule]")
    else:
        rule_file = sys.argv[1]
        upload_rule(rule_file)
