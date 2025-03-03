import json
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
from typing import Optional
from typing import Set
from typing import Tuple
from typing import Type

import peewee as pw
from attrs import define
from boltons.iterutils import partition
from peewee import CTE
from peewee import ModelSelect
from ruamel.yaml import YAML

import semgrep.run_scan
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.config_resolver import Config
from semgrep.config_resolver import resolve_config
from semgrep.error import ERROR_MAP
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.git import get_project_url
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.verbose_logging import getLogger

logger = getLogger(__file__)

yaml = YAML()


# TODO: probably, add error handling
# TODO: decide how to represent these kinds of rules in the output.
# TODO(bug): join rules don't propagate metavariables forward into messages
# # report the last finding? report multiple findings?


class InvalidConditionError(SemgrepError):
    level = out.ErrorSeverity(out.Error_())
    code = FATAL_EXIT_CODE


### Utility functions
def group(items: List[Any], key: Callable[[Any], Any]) -> Dict[Any, Any]:
    dd = defaultdict(list)
    for item in items:
        k = key(item)
        dd[k].append(item)
    return dd


def camel_case(s: str) -> str:
    return "".join(c for c in s.title() if c.isalnum())


class JoinOperator(Enum):
    EQUALS = "=="
    NOT_EQUALS = "!="
    SIMILAR = "~"
    SIMILAR_LEFT = "<"
    SIMILAR_RIGHT = ">"
    RECURSIVE = "-->"


@define
class Ref:
    id: str
    renames: Dict[str, str]
    alias: str


@define
class Condition:
    collection_a: str
    property_a: str
    collection_b: str
    property_b: str
    operator: JoinOperator

    @classmethod
    def parse(cls, condition_string: str) -> "Condition":
        try:
            lhs, operator, rhs = condition_string.split()
            # TODO: consider enforcing aliases to avoid extra dots
            # Also consider using a different property accessor instead of a dot
            a, prop_a = (".".join(lhs.split(".")[:-1]), lhs.split(".")[-1])
            b, prop_b = (".".join(rhs.split(".")[:-1]), rhs.split(".")[-1])
            return cls(a, prop_a, b, prop_b, JoinOperator(operator))
        except ValueError as ve:
            raise InvalidConditionError(
                f"The condition '{condition_string}' was invalid. Must be of the form '<rule>.<metavar> <operator> <rule>.<metavar>'. {str(ve).capitalize()}"
            )


db = pw.SqliteDatabase(":memory:")


class BaseModel(pw.Model):  # type: ignore
    class Meta:
        database = db


def model_factory(model_name: str, columns: List[str]) -> Type[BaseModel]:
    """
    Dynamically create a database model with the specified column names.
    By default, all columns will be TextFields.
    Returns a model _class_, not a model _object_.
    """
    logger.debug(f"Creating model '{model_name}' with columns {columns}")
    return type(
        model_name,
        (BaseModel,),
        dict(
            [("raw", pw.BlobField())]
            + [(column, pw.TextField(null=True)) for column in columns]
        ),
    )


def evaluate_condition(
    A: BaseModel, property_a: str, B: BaseModel, property_b: str, operator: JoinOperator
) -> Any:
    """
    Apply the specified JoinOperator 'operator' on two models and
    the specified properties.

    The return value is the same as a 'peewee' expression, such as
    BlogPost.author == User.name.

    This is where you can add new JoinOperator functionality.
    """
    if operator == JoinOperator.EQUALS:
        return getattr(A, property_a) == getattr(B, property_b)
    elif operator == JoinOperator.NOT_EQUALS:
        return getattr(A, property_a) != getattr(B, property_b)
    elif operator == JoinOperator.SIMILAR_RIGHT:
        return getattr(A, property_a).contains(getattr(B, property_b))
    elif operator == JoinOperator.SIMILAR or operator == JoinOperator.SIMILAR_LEFT:
        return getattr(B, property_b).contains(getattr(A, property_a))

    raise NotImplementedError(f"The operator '{operator}' is not supported.")


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
) -> Optional[pw.ModelSelect]:
    """
    Retrieve all the findings that satisfy the conditions.

    The return value is the same as a 'peewee' .select() expression, such as
    BlogPost.select().where(author="Author").
    """
    recursive_conditions, normal_conditions = partition(
        conditions, lambda condition: condition.operator == JoinOperator.RECURSIVE
    )

    handle_recursive_conditions(recursive_conditions, model_map, aliases)

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
        lambda A, B: A.select().join(B, join_type=pw.JOIN.CROSS), collection_models
    )

    # evaluate conjoined conditions
    condition_terms = []
    for condition in normal_conditions:
        collection_a_real = aliases.get(condition.collection_a, condition.collection_a)
        collection_b_real = aliases.get(condition.collection_b, condition.collection_b)
        A = model_map[collection_a_real]
        B = model_map[collection_b_real]

        condition_terms.append(
            (A, condition.property_a, B, condition.property_b, condition.operator)
        )

    # Use the rhs of the final condition as the finding to return.
    # Without this, the return value is non-deterministic.
    last_condition_model: Type[BaseModel] = condition_terms[-1][2]
    query = (
        joined.select(last_condition_model.raw)
        .distinct()
        .where(
            *list(map(lambda terms: evaluate_condition(*terms), condition_terms))  # type: ignore
        )
    )
    return query


def create_config_map(semgrep_config_strings: List[str]) -> Dict[str, Rule]:
    """
    Create a mapping of Semgrep config strings to Rule objects.
    This will resolve the config strings into their Rule objects, as well.

    NOTE: this will only use the _first rule_ in the resolved config.
    TODO: support more than the first rule.
    """
    config = {}
    for config_string in semgrep_config_strings:
        resolved, config_errors = resolve_config(config_string, get_project_url())
        # Some code-fu to get single rules
        config.update(
            {config_string: list(Config._validate(resolved)[0].values())[0][0]}
        )
    return config


def rename_metavars_in_place(
    semgrep_results: List[Dict[str, Any]],
    refs_lookup: Dict[str, Ref],
) -> None:
    """
    Rename metavariables in-place for all results in 'semgrep_results'.

    Why?
    Since 'join' rules only work on resolved configs at the moment,
    'renames' make it easier to work with metavariables.
    """
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
    semgrep_results: List[Dict[str, Any]],
) -> Dict[str, Type[BaseModel]]:
    """
    Dynamically create 'peewee' model classes directly from Semgrep results.
    The models are stored in a mapping where the key is the rule ID.
    The models themselves use the result metavariables as fields.

    The return value is a mapping from rule ID to its model class.
    """
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


def load_results_into_db(
    semgrep_results: List[Dict[str, Any]], model_map: Dict[str, Type[BaseModel]]
) -> None:
    """
    Populate the models in the database directly from Semgrep results.

    Returns nothing; this will load all data directly into the in-memory database.
    """
    collections = group(semgrep_results, key=lambda item: item.get("check_id"))
    for name, findings in collections.items():
        for finding in findings:
            model_map[name].create(
                path=finding.get("path"),
                raw=json.dumps(finding),
                **{
                    metavar: content.get("abstract_content").strip().strip("\"'")
                    for metavar, content in finding.get("extra", {})
                    .get("metavars", {})
                    .items()
                },
            )


def handle_recursive_conditions(
    conditions: List[Condition],
    model_map: Dict[str, Type[BaseModel]],
    aliases: Dict[str, str],
) -> None:
    for condition in conditions:
        if condition.collection_a != condition.collection_b:
            raise InvalidConditionError(
                f"Recursive conditions must use the same collection name. This condition uses two names: {condition.collection_a}, {condition.collection_b}"
            )
        collection = condition.collection_a
        # Generate a recursive CTE. See https://docs.peewee-orm.com/en/latest/peewee/querying.html#recursive-ctes
        model = model_map[aliases.get(collection, "")]
        cte = generate_recursive_cte(model, condition.property_a, condition.property_b)
        query = (
            model.select(
                model.raw,
                getattr(cte.c, condition.property_a),
                getattr(cte.c, condition.property_b),
            )
            .join(
                cte,
                join_type=pw.JOIN.LEFT_OUTER,
                on=(
                    getattr(cte.c, condition.property_a)
                    == getattr(model, condition.property_a)
                    and getattr(cte.c, condition.property_b)
                    == getattr(model, condition.property_b)
                ),
            )
            .with_cte(cte)
        )
        new_model = model_factory(
            aliases.get(collection, "") + "-rec", query.dicts()[0].keys()
        )
        new_model.create_table()
        for row in query.dicts():
            new_model.create(**row)
        model_map[aliases.get(collection, "")] = new_model


def generate_recursive_cte(model: Type[BaseModel], column1: str, column2: str) -> CTE:  # type: ignore
    first_clause = model.select(
        getattr(model, column1),
        getattr(model, column2),
    ).cte("base", recursive=True)
    union_clause = first_clause.select(
        getattr(first_clause.c, column1),
        getattr(model, column2),
    ).join(model, on=(getattr(first_clause.c, column2) == getattr(model, column1)))
    cte = first_clause.union(union_clause)
    return cte


# For join mode, we get the final output of Semgrep and then need to construct
# RuleMatches from that. Ideally something would be refactored so that we don't
# need to move backwards in the pipeline like this.
def json_to_rule_match(join_rule: Dict[str, Any], match: Dict[str, Any]) -> RuleMatch:
    cli_match_extra = out.CliMatchExtra.from_json(match.get("extra", {}))
    extra = out.CoreMatchExtra(
        message=cli_match_extra.message,
        # cli_match_extra.metavars is optional, but core_match_extra.metavars is
        # not. This is unsafe, but before it was just implicitly unsafe.
        metavars=cli_match_extra.metavars,  # type: ignore[arg-type]
        dataflow_trace=cli_match_extra.dataflow_trace,
        engine_kind=cli_match_extra.engine_kind
        if cli_match_extra.engine_kind
        else out.EngineOfFinding(out.OSS()),
        is_ignored=False,
    )
    return RuleMatch(
        match=out.CoreMatch(
            check_id=out.RuleId(join_rule.get("id", match.get("check_id", "[empty]"))),
            path=out.Fpath(match.get("path", "[empty]")),
            start=out.Position.from_json(match["start"]),
            end=out.Position.from_json(match["end"]),
            extra=extra,
        ),
        message=join_rule.get(
            "message", match.get("extra", {}).get("message", "[empty]")
        ),
        severity=out.MatchSeverity.from_json(
            join_rule.get("severity", match.get("severity", "INFO"))
        ),
        metadata=join_rule.get("metadata", match.get("extra", {}).get("metadata", {})),
    )


def run_join_rule(
    join_rule: Dict[str, Any],
    scanning_roots: List[Path],
    allow_local_builds: bool = False,
    ptt_enabled: bool = False,
) -> Tuple[List[RuleMatch], List[SemgrepError]]:
    """
    Run a 'join' mode rule.

    Join rules are comprised of multiple Semgrep rules and a set
    of conditions which must be satisfied in order to return a result.
    These conditions are typically some comparison of metavariable contents
    from different rules.

    'join_rule' is a join rule definition in dictionary form. The required keys are
    {'id', 'mode', 'severity', 'message', 'join'}.

    'join' is dictionary with the required keys {'refs', 'on'}.

    'refs' is dictionary with the required key {'rule'}. 'rule' is identical to
    a Semgrep config string -- the same thing used on the command line. e.g.,
    `semgrep -f p/javascript.lang.security.rule` or `semgrep -f path/to/rule.yaml`.

    'refs' has optional keys {'renames', 'as'}. 'renames' is a list of objects
    with properties {'from', 'to'}. 'renames' are used to rename metavariables
    of the associated 'rule'. 'as' lets you alias the collection of rule results
    for use in the conditions, similar to a SQL alias. By default, collection names
    will be the rule ID.

    'on' is a list of strings of the form <collection>.<property> <operator> <collection>.<property>.
    These are the conditions which must be satisfied for this rule to report results.
    All conditions must be satisfied.

    See cli/tests/default/e2e/rules/join_rules/user-input-with-unescaped-extension.yaml
    for an example.
    """
    join_contents = join_rule.get("join", {})

    refs = join_contents.get("refs", [])
    semgrep_config_strings = [ref.get("rule") for ref in refs]
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

    inline_rules = join_contents.get("rules", [])
    for rule in inline_rules:
        # Add severity and message fields so that it can be a full rule
        rule.update({"severity": "INFO", "message": "join rule"})
    inline_rules = [Rule.from_json(rule) for rule in inline_rules]

    # Hack: Use the rule ID for inline rules as keys for refs_lookup and alias_lookup.
    # This behavior should probably split out into a separate code
    # path that only deals with refs in the future.
    refs_lookup.update(
        {rule.id: Ref(id=rule.id, renames={}, alias=rule.id) for rule in inline_rules}
    )
    alias_lookup.update({rule.id: rule.id for rule in inline_rules})

    try:
        conditions = [
            Condition.parse(condition_string)
            for condition_string in join_contents.get("on", [])
        ]
    except InvalidConditionError as e:
        return [], [e]

    # Run Semgrep
    with tempfile.NamedTemporaryFile() as rule_path:
        # Combine inline rules and refs
        raw_rules = [rule.raw for rule in inline_rules]
        raw_rules.extend([rule.raw for rule in config_map.values()])
        yaml.dump({"rules": raw_rules}, rule_path)
        rule_path.flush()
        rule_path.seek(0)

        logger.debug(
            f"Running join mode rule {join_rule.get('id')} on {len(scanning_roots)} files."
        )
        output = semgrep.run_scan.run_scan_and_return_json(
            config=Path(rule_path.name),
            scanning_roots=scanning_roots,
            no_rewrite_rule_ids=True,
            optimizations="all",
            allow_local_builds=allow_local_builds,
            ptt_enabled=ptt_enabled,
        )

    assert isinstance(output, dict)  # placate mypy

    results = output.get("results", [])
    errors = output.get("errors", [])

    parsed_errors = []
    for error_dict in errors:
        try:
            """
            This is a hack to reconstitute errors after they've been
            JSONified as output. Subclasses of SemgrepError define the 'level'
            and 'code' as class properties, which means they aren't accepted
            as arguments when instantiated. 'type' is also added when errors are
            JSONified, and is just a string of the error class name. It's not used
            as an argument.
            All of these properties will be properly populated because it's using the
            class properties of the SemgrepError inferred by 'type'.
            """
            del error_dict["code"]
            del error_dict["level"]
            errortype = error_dict.get("type")
            del error_dict["type"]
            parsed_errors.append(
                ERROR_MAP[error_dict.get(errortype)].from_dict(error_dict)
            )
        except (KeyError, TypeError):
            logger.warning(
                f"Could not reconstitute Semgrep error: {error_dict}.\nSkipping processing of error"
            )
            continue

    # Small optimization: if there are no results for rules that
    # are used in a condition, there's no sense in continuing.
    collection_set_unaliased = {
        alias_lookup[collection]
        for collection in create_collection_set_from_conditions(conditions)
    }
    rule_ids = {result.get("check_id") for result in results}
    if collection_set_unaliased - rule_ids:
        logger.debug(
            f"No results for {collection_set_unaliased - rule_ids} in join rule '{join_rule.get('id')}'."
        )
        return [], parsed_errors

    # Rename metavariables with user-defined renames.
    rename_metavars_in_place(results, refs_lookup)

    # Create a model map. This allows dynamically creating DB tables based
    # on Semgrep's results. There is one table for each rule ID.
    model_map = create_model_map(results)
    db.connect()
    db.create_tables(model_map.values())

    # Populate the model tables with real data from the Semgrep results.
    load_results_into_db(results, model_map)

    # Apply the conditions and only keep combinations
    # of findings that satisfy the conditions.
    matches = []
    matched_on_conditions = match_on_conditions(
        model_map,
        alias_lookup,
        [
            Condition.parse(condition_string)
            for condition_string in join_contents.get("on", [])
        ],
    )
    if matched_on_conditions:  # This is ugly, but makes mypy happy
        for match in matched_on_conditions:
            try:
                matches.append(json.loads(match.raw.decode("utf-8", errors="replace")))
            except AttributeError:
                matches.append(json.loads(match.raw))

    rule_matches = [json_to_rule_match(join_rule, match) for match in matches]

    db.close()
    return rule_matches, parsed_errors
