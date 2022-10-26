#!/usr/bin/env python3
import json
import sys
from pathlib import Path

from ruamel.yaml import YAML

RULE_SCHEMA_PATH = "cli/src/semgrep/semgrep_interfaces/rule_schema.yaml"
RULE_SCHEMA_OUT = "rule_schema.json"
RULE_SCHEMA_YAML_OUT = "rule_schema.yaml"


def remove_comment(content):
    yaml = YAML()

    yaml_content = yaml.load(content)
    # Remove for release
    if "$comment" in yaml_content:
        del yaml_content["$comment"]
    with open(RULE_SCHEMA_YAML_OUT, "w") as f:
        yaml.dump(yaml_content, f)
    return json.dumps(yaml_content, indent=4)


if __name__ == "__main__":
    if len(sys.argv) != 1 or not Path(RULE_SCHEMA_PATH).exists():
        print("Converts rule schema yaml file to json and removes comment key")
        print(f"Please check {RULE_SCHEMA_PATH} exists")
        print("Usage: prepare-schema.py")
        sys.exit(1)
    else:
        with open(RULE_SCHEMA_PATH) as f:
            with open(RULE_SCHEMA_OUT, "w") as g:
                g.write(remove_comment(f.read()))
