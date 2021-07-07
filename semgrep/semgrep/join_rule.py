import json
import logging
import sys
import tempfile
from collections import defaultdict
from enum import Enum
from functools import reduce
from itertools import chain
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Tuple

import attr
import peewee as pw
from ruamel.yaml import YAML

import semgrep.semgrep_main
from semgrep.config_resolver import Config
from semgrep.config_resolver import resolve_config
from semgrep.rule import Rule

yaml = YAML()

logger = logging.getLogger(__file__)
logger.setLevel(logging.INFO)
handler = logging.StreamHandler(stream=sys.stderr)
handler.setFormatter(
    logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
)
logger.addHandler(handler)

# TODO: placate mypy
# TODO: refactor into nice code files instead of this giant file
# TODO: update messages, severity on the results
# TODO: probably, add error handling
# TODO: decide how to represent these kinds of rules in the output.
# # report the last finding? report multiple findings?

"""
rules:
- id: blah
  join:
    refs:
    - id: join_rule/unescaped-template-extension.yaml
      as: unescaped-extensions
    - id: join_rule/any-template-var.yaml
      renames:
      - from: '$...EXPR'
        to: '$VAR'
      as: template-vars
    on:
    - 'unescaped-extensions.$VAR == template-vars.$VAR'
    - 'unescaped-extensions.$PATH ~ template-vars.path'
"""


### Utility functions
def group(items: List[Any], key) -> Dict[Any, Any]:
    dd = defaultdict(list)
    for item in items:
        k = key(item)
        dd[k].append(item)
    return dd


def camel_case(s: str) -> str:
    return "".join(c for c in s.title() if c.isalnum())


class Operator(Enum):
    EQUALS = "=="
    NOT_EQUALS = "!="
    SIMILAR = "~"


@attr.s(auto_attribs=True)
class Ref:
    id: str
    renames: Dict[str, str]
    alias: str


@attr.s(auto_attribs=True)
class Condition:
    collection_a: str
    property_a: str
    collection_b: str
    property_b: str
    operator: Operator

    @classmethod
    def parse(cls, condition_string: str) -> "Condition":
        lhs, operator, rhs = condition_string.split()
        # TODO: consider enforcing aliases to avoid extra dots
        # Also consider using a different property accessor instead of a dot
        a, prop_a = (".".join(rhs.split(".")[:-1]), rhs.split(".")[-1])
        b, prop_b = (".".join(lhs.split(".")[:-1]), lhs.split(".")[-1])
        return cls(a, prop_a, b, prop_b, Operator(operator))


db = pw.SqliteDatabase(":memory:")


class BaseModel(pw.Model):
    class Meta:
        database = db


def model_factory(model_name: str, columns: List[str]) -> BaseModel.__class__:
    return type(
        model_name,
        (BaseModel,),
        dict(
            [("raw", pw.BlobField())]
            + [(column, pw.TextField(null=True)) for column in columns]
        ),
    )


def evaluate_condition(
    A: BaseModel, property_a: str, B: BaseModel, property_b: str, operator: Operator
) -> Any:
    if operator == Operator.EQUALS:
        return getattr(A, property_a) == getattr(B, property_b)
    elif operator == Operator.NOT_EQUALS:
        return getattr(A, property_a) != getattr(B, property_b)
    elif operator == Operator.SIMILAR:
        return getattr(A, property_a).contains(getattr(B, property_b))

    raise NotImplementedError(f"The operator {operator} is not supported.")


def match_on_conditions(
    model_map,
    aliases,
    conditions: List[Condition],
) -> List[pw.ModelSelect]:
    # get all collections
    collections = set(
        chain(
            *[
                (condition.collection_a, condition.collection_b)
                for condition in conditions
            ]
        )
    )
    collection_models = list(
        map(lambda collection: model_map[aliases.get(collection)], collections)
    )

    # join them together
    joined = reduce(
        lambda A, B: A.select().join(B, join_type=pw.JOIN.CROSS), collection_models
    )

    # evaluate conjoined conditions
    condition_terms = []
    for condition in conditions:
        collection_a_real = aliases.get(condition.collection_a, condition.collection_a)
        collection_b_real = aliases.get(condition.collection_b, condition.collection_b)
        A = model_map[collection_a_real]
        B = model_map[collection_b_real]

        condition_terms.append(
            (A, condition.property_a, B, condition.property_b, condition.operator)
        )

    return joined.select().where(
        *list(map(lambda terms: evaluate_condition(*terms), condition_terms))
    )


def create_config_map(semgrep_config_strings: List[str]) -> Dict[str, Rule]:
    config = {}
    for config_string in semgrep_config_strings:
        resolved = resolve_config(config_string)
        # Some code-fu to get single rules
        config.update(
            {config_string: list(Config._validate(resolved)[0].values())[0][0]}
        )
    return config


def rename_metavars_in_place(
    semgrep_results: List[Dict[str, Any]],
    refs_lookup: Dict[str, Ref],
):
    for result in semgrep_results:
        metavars = result.get("extra", {}).get("metavars", {})
        renamed_metavars = {
            refs_lookup[result.get("check_id")].renames.get(metavar, metavar): contents
            for metavar, contents in metavars.items()
        }
        result["extra"]["metavars"] = renamed_metavars


def create_model_map(semgrep_results: List[Dict[str, Any]]) -> Dict[str, BaseModel]:
    collections = group(semgrep_results, key=lambda item: item.get("check_id"))
    model_map: Dict[str, BaseModel] = {}
    for name, findings in collections.items():
        metavars = set()
        for finding in findings:
            metavars.update(finding.get("extra", {}).get("metavars", {}).keys())
        model_fields = ["path"] + list(metavars)
        model_class = model_factory(camel_case(name), model_fields)
        model_map[name] = model_class
    return model_map


def populate_data(
    semgrep_results: List[Dict[str, Any]], model_map: Dict[str, BaseModel]
):
    collections = group(semgrep_results, key=lambda item: item.get("check_id"))
    for name, findings in collections.items():
        for finding in findings:
            model_map[name].create(
                path=finding.get("path"),
                raw=json.dumps(finding),
                **{
                    metavar: content.get("abstract_content").strip()
                    for metavar, content in finding.get("extra", {})
                    .get("metavars", {})
                    .items()
                },
            )


def main(
    join_rule: Dict[str, Any],
    targets: List[Path],
) -> Tuple[List[Dict[str, Any]], List[Dict[str, Any]]]:
    semgrep_config_strings = [ref.get("rule") for ref in join_rule.get("refs", [])]
    config_map = create_config_map(semgrep_config_strings)

    join_rule_refs: List[Ref] = [
        Ref(
            id=config_map[ref.get("rule")].id,
            renames={
                rename.get("from"): rename.get("to")
                for rename in ref.get("renames", [])
            },
            alias=ref.get("as"),
        )
        for ref in join_rule.get("refs", [])
    ]
    refs_lookup = {ref.id: ref for ref in join_rule_refs}
    alias_lookup = {ref.alias: ref.id for ref in join_rule_refs}

    # Run Semgrep
    with tempfile.NamedTemporaryFile() as rule_path:
        yaml.dump({"rules": [rule.raw for rule in config_map.values()]}, rule_path)
        output = semgrep.semgrep_main.invoke_semgrep(
            config=rule_path.name,
            targets=targets,
            no_rewrite_rule_ids=True,
        )

    results = output.get("results", [])
    errors = output.get("errors", [])

    # Rename metavariables with user-defined renames.
    rename_metavars_in_place(results, refs_lookup)

    # Create a model map. This allows dynamically creating DB tables based
    # on Semgrep's results. There is one table for each rule ID.
    model_map = create_model_map(results)
    db.connect()
    db.create_tables(model_map.values())

    # Populate the model tables with real data from the Semgrep results.
    populate_data(results, model_map)

    # Apply the conditions and only keep which combinations
    # of findings satisfy the conditions.

    print("matching...")
    matches = []
    for match in match_on_conditions(
        model_map,
        alias_lookup,
        [
            Condition.parse(condition_string)
            for condition_string in join_rule.get("on", [])
        ],
    ):
        matches.append(json.loads(match.raw.decode("utf-8")))

    print("matching done.")

    # TODO: override result IDs, message, severity, and metadata.
    # Better yet, consider just making new ones
    return matches, errors


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    # Add arguments here
    parser.add_argument("-f", "--config")
    parser.add_argument("target", default=".")
    args = parser.parse_args()

    with Path(args.config).open("r") as fin:
        contents = yaml.load(fin)

    for rule in contents.get("rules", []):
        join_rule = rule.get("join", {})
        matches, errors = main(join_rule, Path(args.target).rglob("*"))
        print(json.dumps({"results": matches, "errors": errors}))
