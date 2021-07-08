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
from typing import Callable
from typing import Dict
from typing import List
from typing import Set
from typing import Tuple
from typing import Type
from typing import Union

import attr
import peewee as pw
from peewee import ModelSelect
from ruamel.yaml import YAML

import semgrep.semgrep_main
from semgrep.config_resolver import Config
from semgrep.config_resolver import resolve_config
from semgrep.error import SemgrepError
from semgrep.pattern_match import PatternMatch
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch

yaml = YAML()

logger = logging.getLogger(__file__)
logger.setLevel(logging.DEBUG)
handler = logging.StreamHandler(stream=sys.stderr)
handler.setFormatter(
    logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
)
logger.addHandler(handler)

# TODO: refactor into nice code files instead of this giant file
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
def group(items: List[Any], key: Callable[[Any], Any]) -> Dict[Any, Any]:
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


class BaseModel(pw.Model):  # type: ignore
    class Meta:
        database = db


def model_factory(model_name: str, columns: List[str]) -> Type[BaseModel]:
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


def create_collection_set_from_conditions(conditions: List[Condition]) -> Set[str]:
    return set(
        chain(
            *[
                (condition.collection_a, condition.collection_b)
                for condition in conditions
            ]
        )
    )


def match_on_conditions(  # type: ignore
    model_map: Dict[str, Type[BaseModel]],
    aliases: Dict[str, str],
    conditions: List[Condition],
) -> Union[pw.ModelSelect, List[None]]:
    # get all collections
    collections = create_collection_set_from_conditions(conditions)
    try:
        collection_models = [
            model_map[aliases.get(collection, "")] for collection in collections
        ]
    except KeyError as ke:
        logger.warning(
            f"No model exists for this rule '{ke}' but a condition for '{ke}' is required. Cannot proceed with this join rule."
        )
        return []

    # join them together
    joined: ModelSelect = reduce(  # type: ignore
        lambda A, B: A.select().join(B, join_type=pw.JOIN.CROSS), collection_models  # type: ignore
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

    # Use the rhs of the final condition as the finding to return.
    # Without this, the return value is non-deterministic.
    last_condition_model: Type[BaseModel] = condition_terms[-1][0]
    query = joined.select(last_condition_model.raw).where(
        *list(map(lambda terms: evaluate_condition(*terms), condition_terms))  # type: ignore
    )
    logger.debug(query)
    return query


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
) -> None:
    for result in semgrep_results:
        metavars = result.get("extra", {}).get("metavars", {})
        renamed_metavars = {
            refs_lookup[result.get("check_id", "")].renames.get(
                metavar, metavar
            ): contents
            for metavar, contents in metavars.items()
        }
        result["extra"]["metavars"] = renamed_metavars


def create_model_map(
    semgrep_results: List[Dict[str, Any]]
) -> Dict[str, Type[BaseModel]]:
    collections: Dict[str, List[Dict]] = group(
        semgrep_results, key=lambda item: item.get("check_id")
    )
    model_map: Dict[str, Type[BaseModel]] = {}
    for name, findings in collections.items():
        metavars = set()
        for finding in findings:
            metavars.update(finding.get("extra", {}).get("metavars", {}).keys())
        model_fields = ["path"] + list(metavars)
        model_class = model_factory(camel_case(name), model_fields)
        model_map[name] = model_class
    return model_map


def populate_data(
    semgrep_results: List[Dict[str, Any]], model_map: Dict[str, Type[BaseModel]]
) -> None:
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
) -> Tuple[List[RuleMatch], List[SemgrepError]]:
    join_contents = join_rule.get("join", {})
    semgrep_config_strings = [ref.get("rule") for ref in join_contents.get("refs", [])]
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
        for ref in join_contents.get("refs", [])
    ]
    refs_lookup = {ref.id: ref for ref in join_rule_refs}
    alias_lookup = {ref.alias: ref.id for ref in join_rule_refs}

    conditions = [
        Condition.parse(condition_string)
        for condition_string in join_contents.get("on", [])
    ]

    # Run Semgrep
    with tempfile.NamedTemporaryFile() as rule_path:
        yaml.dump({"rules": [rule.raw for rule in config_map.values()]}, rule_path)
        output = semgrep.semgrep_main.invoke_semgrep(
            config=Path(rule_path.name),
            targets=targets,
            no_rewrite_rule_ids=True,
        )

    assert isinstance(output, dict)  # placate mypy

    results = output.get("results", [])
    errors = output.get("errors", [])

    # Small optimization: if there are no results for rules that
    # are used in a condition, there's no sense in continuing.
    collection_set_unaliased = {
        alias_lookup[collection]
        for collection in create_collection_set_from_conditions(conditions)
    }
    rule_ids = set(result.get("check_id") for result in results)
    if collection_set_unaliased - rule_ids:
        logger.debug(
            f"No results for {collection_set_unaliased - rule_ids} in join rule '{join_rule.get('id')}'."
        )
        return [], errors

    # Rename metavariables with user-defined renames.
    rename_metavars_in_place(results, refs_lookup)

    # Create a model map. This allows dynamically creating DB tables based
    # on Semgrep's results. There is one table for each rule ID.
    model_map = create_model_map(results)
    db.connect()
    db.create_tables(model_map.values())

    # Populate the model tables with real data from the Semgrep results.
    populate_data(results, model_map)

    # Apply the conditions and only keep combinations
    # of findings that satisfy the conditions.
    matches = []
    for match in match_on_conditions(
        model_map,
        alias_lookup,
        [
            Condition.parse(condition_string)
            for condition_string in join_contents.get("on", [])
        ],
    ):
        matches.append(json.loads(match.raw.decode("utf-8")))

    rule_matches = [
        RuleMatch(
            id=join_rule.get("id", match.get("check_id", "[empty]")),
            pattern_match=PatternMatch({}),
            message=join_rule.get(
                "message", match.get("extra", {}).get("message", "[empty]")
            ),
            metadata=join_rule.get(
                "metadata", match.get("extra", {}).get("metadata", {})
            ),
            severity=join_rule.get("severity", match.get("severity", "INFO")),
            path=Path(match.get("path", "[empty]")),
            start=match.get("start", {}),
            end=match.get("end", {}),
            extra=match.get("extra", {}),
            fix=None,
            fix_regex=None,
            lines_cache={},
        )
        for match in matches
    ]
    return rule_matches, errors


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
        matches, errors = main(rule, list(Path(args.target).rglob("*")))
        print(matches)
        print(errors)
