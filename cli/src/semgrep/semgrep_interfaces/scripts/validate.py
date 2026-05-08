# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "jsonschema>=4.0.0",
#     "pyyaml>=6.0",
# ]
# ///
"""
Use a JSON Schema validator to check input files that should pass or fail.

A file with the .ok.yaml extension is expected to pass validation.
A file with the .fail.yaml extension is expected to fail.
"""

import glob
import json
import sys
from pathlib import Path

import yaml
from jsonschema import ValidationError, validate


def load_schema(schema_path: str) -> dict:
    text = Path(schema_path).read_text()
    if schema_path.endswith(".yaml") or schema_path.endswith(".yml"):
        return yaml.safe_load(text)
    return json.loads(text)


def load_instance(instance_path: str) -> object:
    text = Path(instance_path).read_text()
    if instance_path.endswith(".yaml") or instance_path.endswith(".yml"):
        return yaml.safe_load(text)
    return json.loads(text)


def validate_file(schema: dict, instance_path: str) -> bool:
    """Returns True if validation passes, False if it fails."""
    instance = load_instance(instance_path)
    try:
        validate(instance=instance, schema=schema)
        return True
    except ValidationError:
        return False


def main() -> None:
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <schema_file> <input_dir>", file=sys.stderr)
        sys.exit(2)

    schema_file = sys.argv[1]
    input_dir = sys.argv[2]

    schema = load_schema(schema_file)

    exit_code = 0

    # Check well-formed files
    ok_files = sorted(glob.glob(f"{input_dir}/*.ok.yaml"))
    for input_file in ok_files:
        if validate_file(schema, input_file):
            print(f"OK: {input_file}")
        else:
            print(f"*** {input_file}: should have passed validation", file=sys.stderr)
            exit_code = 1

    # Check that malformed files are detected
    fail_files = sorted(glob.glob(f"{input_dir}/*.fail.yaml"))
    for input_file in fail_files:
        if validate_file(schema, input_file):
            print(f"*** {input_file}: should have failed validation", file=sys.stderr)
            exit_code = 1
        else:
            print(f"XFAIL (failed as expected): {input_file}")

    sys.exit(exit_code)


if __name__ == "__main__":
    main()
