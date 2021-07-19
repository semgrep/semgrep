#!/usr/bin/env python3
import argparse
import os
import sys
from pathlib import Path
from typing import Any
from typing import Dict

import jsonschema
import requests
import yaml

SEMGREP_UPLOAD_DEPLOYMENT_ID_ENVVAR = "SEMGREP_UPLOAD_DEPLOYMENT"
SEMGREP_UPLOAD_TOKEN_ENVVAR = "SEMGREP_TOKEN"
SCHEMA_PATH = "rule_schema.yaml"
SEMGREP_REGISTRY_BASE_URL = os.environ.get(
    "SEMGREP_REGISTRY_BASE_URL", "https://semgrep.dev"
)
SEMGREP_REGISTRY_UPLOAD_URL = f"{SEMGREP_REGISTRY_BASE_URL}/api/registry/rule"
SEMGREP_REGISTRY_VIEW_URL = f"{SEMGREP_REGISTRY_BASE_URL}/r/"


def is_valid(rule: Dict[str, Any]) -> bool:
    """
    Returns true if the rule is valid for uploading

    A valid rule definition should:
    - only have one rule definition
    - passes schema validation
    """
    if len(rule.get("rules", [])) != 1:
        print("Rule contains more than one rule")
        return False

    with Path(SCHEMA_PATH).open() as schema_file:
        schema = yaml.safe_load(schema_file)

    try:
        jsonschema.validate(rule, schema, cls=jsonschema.validators.Draft7Validator)
    except jsonschema.ValidationError as ve:
        print(ve)
        return False

    return True


def upload_rule(rule_file: Path, deployment_id: int, token: str) -> None:
    """
    Uploads rule in rule_file to private registry of deployment_id

    Args:
        rule_file: path to valid rule yaml file that contains single
        rule to be uploaded
        deployment_id: which deployment to upload a rule to
        token: token with permissions to upload a rule
    """
    with rule_file.open("r") as file:
        rule = yaml.safe_load(file)

    if not is_valid(rule):
        print(
            "Rule is not valid. rule file must conform to semgrep rule schema and only contain a single rule"
        )
        sys.exit(1)

    session = requests.Session()
    session.headers["Authorization"] = f"Bearer {token}"

    request_json = {
        "definition": rule,
        "visibility": "org_private",
        # below should always be defined if passed validation
        "languages": rule["rules"][0]["languages"],
        "deployment_id": deployment_id,
    }

    response = session.post(SEMGREP_REGISTRY_UPLOAD_URL, json=request_json, timeout=30)

    if not response.ok:
        print(f"Failed to upload rule with status_code {response.status_code}")
        print(response.text)
        sys.exit(1)
    else:
        created_rule = response.json()
        print(
            f"You can find your private rule at {SEMGREP_REGISTRY_VIEW_URL}{created_rule['path']}"
        )


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Simple script to upload rule yaml to registry"
    )
    parser.add_argument("rule_file", help="File that contains single rule definition")
    parser.add_argument(
        "--deployment_id",
        type=int,
        default=os.environ.get(SEMGREP_UPLOAD_DEPLOYMENT_ID_ENVVAR),
        help=f"Deployment Id to upload private rule to. Defaults to {SEMGREP_UPLOAD_DEPLOYMENT_ID_ENVVAR} env variable",
    )
    parser.add_argument(
        "--token",
        default=os.environ.get(SEMGREP_UPLOAD_TOKEN_ENVVAR),
        help=f"Token to use when uploading. Needs to have permissions to upload to given deployment id. Defaults to {SEMGREP_UPLOAD_TOKEN_ENVVAR} env variable",
    )

    args = parser.parse_args()
    if args.deployment_id is None:
        print("No deployment id specified. See `--deployment_id` option")
        sys.exit(1)
    if args.token is None:
        print("No semgrep token specified. See `--token` option")
        sys.exit(1)
    if not Path(args.rule_file).is_file():
        print(f"Rule file {args.rule_file} must be a valid file")
        sys.exit(1)

    upload_rule(Path(args.rule_file), args.deployment_id, args.token)


if __name__ == "__main__":
    main()
