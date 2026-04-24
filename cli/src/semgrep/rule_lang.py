#
# Copyright (c) 2020-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import json
import re
from io import StringIO
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import Generic
from typing import ItemsView
from typing import KeysView
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple
from typing import TypeVar
from typing import Union

import jsonschema.exceptions
from jsonschema.validators import Draft7Validator
from packaging.version import Version
from ruamel.yaml import MappingNode
from ruamel.yaml import Node
from ruamel.yaml import RoundTripConstructor
from ruamel.yaml import YAML

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import __VERSION__
from semgrep import telemetry
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.error import default_level
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import OK_EXIT_CODE
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.error import warning_level
from semgrep.error_location import Position
from semgrep.error_location import SourceFileHash
from semgrep.error_location import SourceTracker
from semgrep.error_location import Span
from semgrep.rpc_call import validate as rpc_validate
from semgrep.verbose_logging import getLogger

MISSING_RULE_ID = "no-rule-id"
INTERNAL_DEPENDS_ON_KEY = "r2c-internal-project-depends-on"


logger = getLogger(__name__)


class EmptyYamlException(Exception):
    pass


class RuleSchema:
    _schema: Dict[str, Any] = {}

    @classmethod
    def get(cls) -> Dict[str, Any]:
        """
        Returns the rule schema

        Not thread safe.
        """
        if not cls._schema:
            yaml = YAML()
            schema_path = (
                Path(__file__).parent / "semgrep_interfaces" / "rule_schema_v1.yaml"
            )
            with schema_path.open() as fd:
                cls._schema = yaml.load(fd)
        return cls._schema


EmptySpan = Span.from_string("a: b")

# Actually recursive but mypy is unhelpful
YamlValue = Union[str, int, List[Any], Dict[str, Any]]
LocatedYamlValue = Union[str, int, List["YamlTree"], "YamlMap"]

T = TypeVar("T", bound=LocatedYamlValue)


class YamlTree(Generic[T]):
    # __slots__ pre-allocates a fixed size array for the attributes of instances of this class.
    # This speeds up object creation at the cost of losing the ability to add attributes dynamically.
    __slots__ = ("value", "span")

    def __init__(self, value: T, span: Span):
        self.value = value
        self.span = span

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} span={self.span} value={self.value}>"

    def unroll_dict(self) -> Dict[str, Any]:
        """
        Helper wrapper mostly for mypy when you know it contains a dictionary
        """
        ret = self.unroll()
        if not isinstance(ret, dict):
            raise ValueError(
                f"unroll_dict called but object was actually {type(ret).__name__}"
            )
        return ret

    def unroll(self) -> YamlValue:
        """
        Recursively expand the `self.value`, converting back to a normal datastructure
        """
        if isinstance(self.value, list):
            return [x.unroll() for x in self.value]
        elif isinstance(self.value, YamlMap):
            return {str(k.unroll()): v.unroll() for k, v in self.value.items()}
        elif isinstance(self.value, YamlTree):
            return self.value.unroll()
        elif isinstance(self.value, (str, int)) or self.value is None:
            return self.value
        else:
            raise ValueError(
                f"Invalid YAML tree structure (expected a list, dict, tree, int or str, found: {type(self.value).__name__}: {self.value}"
            )

    @classmethod
    def wrap(cls, value: YamlValue, span: Span) -> "YamlTree":
        """
        Wraps a value in a YamlTree and attaches the span everywhere.
        This exists so you can take generate a datastructure from user input, but track all the errors within that
        datastructure back to the user input
        """
        if isinstance(value, list):
            return YamlTree(value=[YamlTree.wrap(x, span) for x in value], span=span)
        elif isinstance(value, dict):
            return YamlTree(
                value=YamlMap(
                    {
                        YamlTree.wrap(k, span): YamlTree.wrap(v, span)
                        for k, v in value.items()
                    }
                ),
                span=span,
            )
        elif isinstance(value, YamlTree):
            return value
        else:
            return YamlTree(value, span)


class YamlMap:
    """
    To preserve span information for keys, which we commonly use in error messages,
    make a custom map type that is indexable by str, but provides views into all
    necessary spans
    """

    __slots__ = ("_internal",)

    def __init__(self, internal: Dict[YamlTree[str], YamlTree]):
        self._internal = internal

    def __getitem__(self, key: str) -> YamlTree:
        try:
            return next(v for k, v in self._internal.items() if k.value == key)
        except StopIteration:
            raise KeyError(key)

    def __setitem__(self, key: YamlTree[str], value: YamlTree) -> None:
        self._internal[key] = value

    def items(self) -> ItemsView[YamlTree[str], YamlTree]:
        return self._internal.items()

    def key_tree(self, key: str) -> YamlTree[str]:
        return next(k for k, v in self._internal.items() if k.value == key)

    def __contains__(self, item: str) -> bool:
        try:
            _ = self[item]
            return True
        except KeyError:
            return False

    def get(self, key: str) -> Optional[YamlTree]:
        match = [v for k, v in self._internal.items() if k.value == key]
        if match:
            return match[0]
        return None

    def keys(self) -> KeysView[YamlTree[str]]:
        return self._internal.keys()


def parse_yaml_preserve_spans(
    contents: str, filename: Optional[str], allow_null: bool = False
) -> Optional[YamlTree]:
    """
    parse yaml into a YamlTree object. The resulting spans are tracked in SourceTracker
    so they can be used later when constructing error messages or displaying context.

    :raise jsonschema.exceptions.SchemaError: if config is invalid
    """

    source_hash = SourceTracker.add_source(contents)

    # this uses the `RoundTripConstructor` which inherits from `SafeConstructor`
    class SpanPreservingRuamelConstructor(RoundTripConstructor):
        def construct_object(self, node: Node, deep: bool = False) -> YamlTree:
            r = super().construct_object(node, deep)

            # Check for duplicate mapping keys.
            # This -should- be caught and raised by ruamel.yaml.
            # However, resetting the constructor below, where the line
            # reads yaml.Constructor = SpanPreservingRuamelConstructor,
            # causes ruamel's DuplicateKeyError not to be raised.
            # This is a quick implementation that will check MappingNodes
            #
            if isinstance(node, MappingNode):
                kv_pairs: List[Tuple[Node, Node]] = [t for t in node.value]
                uniq_key_names: Set[str] = {t[0].value for t in kv_pairs}
                # If the number of unique key names is less than the number
                # of key-value nodes, then there's a duplicate key
                if len(uniq_key_names) < len(kv_pairs):
                    raise InvalidRuleSchemaError(
                        short_msg="Detected duplicate key",
                        long_msg=f"Detected duplicate key name, one of {list(sorted(uniq_key_names))}.",
                        spans=[
                            Span.from_node(
                                node, source_hash=source_hash, filename=filename
                            ).with_context(before=1, after=1)
                        ],
                    )

            if r is None and not allow_null:
                # This was originally intended only for parsing semgrep rules
                # but we use it for yaml based lockfiles now too, and those can have null in them
                Span.from_node(node, source_hash=source_hash, filename=filename)
                raise InvalidRuleSchemaError(
                    short_msg="null values prohibited",
                    long_msg="In semgrep YAML configuration, null values are prohibited",
                    spans=[
                        Span.from_node(
                            node, source_hash=source_hash, filename=filename
                        ).with_context(before=1, after=1)
                    ],
                )

            if isinstance(r, dict):
                r = YamlMap(r)
            return YamlTree(
                r, Span.from_node(node, source_hash=source_hash, filename=filename)
            )

        def construct_yaml_timestamp(
            self, node: Node, values: Optional[List[Any]] = None
        ) -> Any:
            """Load YAML timestamps as strings"""
            return self.construct_yaml_str(node)  # type: ignore ## missing from ruamel stub

    SpanPreservingRuamelConstructor.add_constructor(  # type: ignore ## missing from ruamel stub
        "tag:yaml.org,2002:timestamp",
        SpanPreservingRuamelConstructor.construct_yaml_timestamp,
    )

    yaml = YAML()
    yaml.Constructor = SpanPreservingRuamelConstructor
    data = yaml.load(StringIO(contents))
    if data is None:
        return None

    if not isinstance(data, YamlTree):
        raise Exception(
            f"Something went wrong parsing Yaml (expected a YamlTree as output, but got {type(data).__name__}): {PLEASE_FILE_ISSUE_TEXT}"
        )
    return data


def has_patterns_key(raw_rule: Dict[str, Any]) -> bool:
    """Helper function to check if a rule dict has any of the pattern keys, which is a heuristic for whether it contains patterns at all."""
    return any(key in RuleValidation.PATTERN_KEYS for key in raw_rule)


def project_depends_on(raw_rule: Dict[str, Any]) -> List[Dict[str, str]]:
    if INTERNAL_DEPENDS_ON_KEY in raw_rule:
        depends_on = raw_rule[INTERNAL_DEPENDS_ON_KEY]
        if "depends-on-either" in depends_on:
            dependencies: List[Dict[str, str]] = depends_on["depends-on-either"]
            return dependencies
        else:
            return [depends_on]
    else:
        return []


@telemetry.trace()
def parse_yaml_and_filter_versions(
    contents: str,
    filename: Optional[str],
) -> Tuple[YamlTree, List[SemgrepError]]:
    """Parse YAML with span preservation and filter out version-incompatible rules.
    Does NOT do schema validation."""
    data = parse_yaml_preserve_spans(contents, filename)
    if not data:
        raise EmptyYamlException()
    errors = remove_incompatible_version_yamltree(
        data, filename, no_rewrite_rule_ids=False
    )
    return data, errors


@telemetry.trace()
def parse_json_and_filter_versions(
    contents: str,
    filename: Optional[str],
) -> Tuple[Dict[str, Any], List[SemgrepError]]:
    """Parse JSON and filter out version-incompatible rules.
    Does NOT do schema validation. Mirrors parse_yaml_and_filter_versions for
    JSON config sources."""
    data: Dict[str, Any] = json.loads(contents)
    raw_rules = data.get("rules", [])
    surviving, errors = remove_incompatible_version_dicts(raw_rules, filename)
    data["rules"] = [raw_rules[i] for i in surviving]
    return data, errors


class RuleValidation:
    REQUIRE_REGEX = re.compile(r"'(.*)' is a required property")
    PATTERN_KEYS = {
        "match",
        "taint",  # for new-syntax taint mode rules
        "pattern",
        "pattern-either",
        "pattern-regex",
        "patterns",
        "pattern-sinks",
        "pattern-sources",
        "join",
        "postprocessor-patterns",
        "request",
        "response",
    }
    INVALID_SENTINEL = " is not allowed for "
    INVALID_FOR_MODE_SENTINEL = "False schema does not allow"
    BAD_TYPE_SENTINEL = "is not of type"
    BANNED_SENTINEL = "Additional properties are not allowed"
    REDUNDANT_SENTINEL = "is valid under each of"


def _validation_error_message(error: jsonschema.exceptions.ValidationError) -> str:
    """
    Heuristic that returns meaningful error messages in all examples from
    tests/default/e2e/rules/syntax/badXXX.yaml
    """

    contexts = (error.parent.context or []) if error.parent else [error]
    invalid_for_mode_keys = set()
    redundant_keys = set()
    bad_type = set()
    invalid_keys = set()
    any_of_invalid_keys = set()
    required = set()
    banned = set()
    for context in contexts:
        if RuleValidation.REDUNDANT_SENTINEL in context.message:
            mutex_properties = [
                k["required"][0]
                # type ignore here due to leaking of unset type: https://github.com/python-jsonschema/jsonschema/issues/1298#issuecomment-2573045038
                for k in context.validator_value  # type: ignore
                if "required" in k and k["required"]
            ]
            l = []
            for property in mutex_properties:
                # type ignore here due to leaking of unset type: https://github.com/python-jsonschema/jsonschema/issues/1298#issuecomment-2573045038
                if property and property in context.instance.keys():  # type: ignore
                    l.append(property)
            redundant_keys.add(tuple(l))
        if context.message.startswith(RuleValidation.INVALID_FOR_MODE_SENTINEL):
            invalid_for_mode_keys.add(context.path.pop())
        if RuleValidation.BAD_TYPE_SENTINEL in context.message:
            bad_type.add(context.message)
        if RuleValidation.INVALID_SENTINEL in context.message:
            try:
                required_keys = [
                    k["required"][0]
                    # type ignore here due to leaking of unset type: https://github.com/python-jsonschema/jsonschema/issues/1298#issuecomment-2573045038
                    for k in context.validator_value.get("anyOf", [])  # type: ignore
                    if "required" in k and k["required"]
                ]
                for r in required_keys:
                    # type ignore here due to leaking of unset type: https://github.com/python-jsonschema/jsonschema/issues/1298#issuecomment-2573045038
                    if r and r in context.instance.keys():  # type: ignore
                        any_of_invalid_keys.add(r)
            except (json.JSONDecodeError, AttributeError):
                invalid_keys.add(context.message)
        if context.message.startswith(RuleValidation.BANNED_SENTINEL):
            banned.add(context.message)
        require_matches = RuleValidation.REQUIRE_REGEX.match(context.message)
        if require_matches:
            required.add(require_matches[1])

    if invalid_keys:
        return "\n".join(sorted(invalid_keys))
    if bad_type:
        return "\n".join(sorted(bad_type))
    if banned:
        return "\n".join(sorted(banned))

    outs = []
    if invalid_for_mode_keys:
        keys = ", ".join(f"'{k}'" for k in sorted(invalid_for_mode_keys))
        outs.append(f"These properties are invalid in the current mode: {keys}")
    if any_of_invalid_keys:
        keys = ", ".join(f"'{k}'" for k in sorted(any_of_invalid_keys))
        outs.append(f"One of these properties may be invalid: {keys}")
        required = required - RuleValidation.PATTERN_KEYS
    if required:
        keys = ", ".join(f"'{k}'" for k in sorted(required))
        outs.append(f"One of these properties is missing: {keys}")
    if redundant_keys:
        for mutex_set in sorted(redundant_keys):
            keys = ", ".join(f"'{k}'" for k in sorted(mutex_set))
            outs.append(
                f"These options were {'both' if len(mutex_set) == 2 else 'all'} specified, but they are mutually exclusive: {keys}"
            )
    if outs:
        return "\n".join(outs)

    return contexts[0].message


DUMMY_POSITION = out.Position(line=1, col=0, offset=0)


def safe_relative_to(a: Path, b: Path) -> Path:
    try:
        return a.relative_to(b)
    except ValueError:
        # paths had no common prefix; not possible to relativize
        return a


def sanitize_rule_id_fragment(s: str) -> str:
    """Make a valid fragment for a rule ID.

    This removes characters that aren't allowed in Semgrep rule IDs.
    The transformation is irreversible. The result may be an empty
    string.

    Rule ID format: [a-zA-Z0-9._-]*
    """
    return re.sub("[^a-zA-Z0-9._-]", "", s)


def convert_config_id_to_prefix(config_id: str) -> str:
    at_path = Path(config_id)
    try:
        at_path = safe_relative_to(at_path, Path.cwd())
    except FileNotFoundError:
        pass

    prefix = ".".join(at_path.parts[:-1]).lstrip("./").lstrip(".")
    if len(prefix):
        prefix += "."
    # Remove any remaining special characters that were in the file path.
    prefix = sanitize_rule_id_fragment(prefix)
    return prefix


# Turn a rule ID 'foo' found in file 'a/b/c.yml' into 'a.b.foo'
def prepend_rule_path(filename: Optional[str], rule_id: str) -> str:
    rule_id = rule_id or MISSING_RULE_ID
    if filename:
        # Some test manages to pass a rule_id of type int, hence the
        # str() conversion. Yay.
        return convert_config_id_to_prefix(filename) + str(rule_id)
    else:
        return rule_id


def version_error(
    rule_id: str,
    filename: str,
    msg: str,
    min_ver: Optional[str] = None,
    max_ver: Optional[str] = None,
) -> SemgrepCoreError:
    """Helper function for generating Version errors while parsing min/max version strings from rules"""
    return SemgrepCoreError(
        code=OK_EXIT_CODE,
        level=out.ErrorSeverity(out.Info_()),
        spans=None,
        core=out.CoreError(
            error_type=out.ErrorType(
                out.IncompatibleRule_(
                    out.IncompatibleRule(
                        rule_id=out.RuleId(rule_id),
                        this_version=out.Version(__VERSION__),
                        min_version=out.Version(min_ver) if min_ver else None,
                        max_version=out.Version(max_ver) if max_ver else None,
                    )
                )
            ),
            severity=out.ErrorSeverity(out.Info_()),
            location=out.Location(
                path=out.Fpath(filename or ""),
                start=DUMMY_POSITION,
                end=DUMMY_POSITION,
            ),
            message=msg,
            rule_id=out.RuleId(rule_id),
        ),
    )


class RpcValidationError(Exception):
    def __init__(self, core_error: out.CoreError):
        self.core_error = core_error
        super().__init__(core_error.message)


def remove_incompatible_version_dicts(
    rule_dicts: List[Dict[str, Any]],
    filename: Optional[str] = None,
    no_rewrite_rule_ids: bool = False,
) -> Tuple[List[int], List[SemgrepError]]:
    """Check min-version/max-version constraints on a list of rule dicts.

    Returns a tuple of (surviving_indices, errors) where surviving_indices
    are the indices of rules that passed version checks.
    """
    errors: List[SemgrepError] = []
    surviving: List[int] = []
    for i, raw_rule in enumerate(rule_dicts):
        rule_id = raw_rule.get("id", MISSING_RULE_ID)
        if not no_rewrite_rule_ids:
            rule_id = prepend_rule_path(filename or "", rule_id)

        min_version = raw_rule.get("min-version")
        if min_version and Version(__VERSION__) < Version(min_version):
            msg = (
                f"This rule requires upgrading Semgrep from version "
                f"{__VERSION__} to at least {min_version}"
            )
            errors.append(
                version_error(rule_id, filename or "", msg, min_ver=min_version)
            )
            continue

        max_version = raw_rule.get("max-version")
        if max_version and Version(__VERSION__) > Version(max_version):
            msg = (
                f"This rule is no longer supported by Semgrep. "
                f"The last compatible version was {max_version}. "
                f"This version of Semgrep is {__VERSION__}"
            )
            errors.append(
                version_error(rule_id, filename or "", msg, max_ver=max_version)
            )
            continue

        surviving.append(i)
    return surviving, errors


@telemetry.trace()
def remove_incompatible_version_yamltree(
    root: YamlTree, filename: Optional[str], no_rewrite_rule_ids: bool = False
) -> List[SemgrepError]:
    """
    Modifies a YamlTree of the form {"rules": [{<rule_1>}, {<rule_2}, ...]} by removing any rules with invalid versions.
    Returns an error for each rule that failed to validate.
    """
    root_value = root.value
    if "rules" not in root_value:
        return []
    rules = root_value["rules"]
    rules_value = rules.value
    rule_dicts = [rule.unroll_dict() for rule in rules_value]
    surviving, errors = remove_incompatible_version_dicts(
        rule_dicts, filename, no_rewrite_rule_ids
    )
    rules.value = [rules_value[i] for i in surviving]
    return errors


def validate_rules(
    data: Dict[str, Any],
    rpc_source_hash: SourceFileHash,
    display_source_hash: SourceFileHash,
    filename: Optional[str],
    force_jsonschema: bool = False,
    no_python_schema_validation: bool = False,
    rules_tmp_path: Optional[str] = None,
    rule_spans: Optional[Dict[str, Span]] = None,
) -> None:
    """
    Validates rule data (a dict of the form {"rules": [...]}) via RPC or
    jsonschema fallback. Raises an Exception if the validation fails.

    rpc_source_hash tracks the JSON dump content (line numbers match
    semgrep-core output). display_source_hash tracks the original file
    content (shown in jsonschema fallback errors when rule_spans misses).
    """
    if no_python_schema_validation:
        validate_file_rpc(
            rpc_source_hash,
            filename,
            rules_tmp_path=rules_tmp_path,
            rule_spans=rule_spans,
            display_source_hash=display_source_hash,
        )
    elif force_jsonschema or not rules_tmp_path:
        validate_string_json_schema(
            data,
            source_hash=display_source_hash,
            filename=filename,
            rule_spans=rule_spans,
        )
    else:
        try:
            if not Path.exists(Path(rules_tmp_path)):
                raise NotImplementedError(
                    "Cannot execute RPC validation without a rules_tmp_path"
                )
            run_rpc_validate_exn(rules_tmp_path=rules_tmp_path)
            logger.debug("RPC validation succeeded")
        except (RpcValidationError, NotImplementedError) as e:
            error_type = (
                e.core_error.error_type.kind
                if isinstance(e, RpcValidationError)
                else type(e).__name__
            )
            logger.warning(f"semgrep-core rule validation failed ({error_type})")
            logger.debug(f"semgrep-core validation error detail: {e}")
            validate_string_json_schema(
                data,
                source_hash=display_source_hash,
                filename=filename,
                rule_spans=rule_spans,
            )


@telemetry.trace()
def validate_file_rpc(
    source_hash: SourceFileHash,
    filename: Optional[str] = None,
    rules_tmp_path: Optional[str] = None,
    rule_spans: Optional[Dict[str, Span]] = None,
    display_source_hash: Optional[SourceFileHash] = None,
) -> None:
    """
    Applies validation to a file at filename or rules_tmp_path (preferring filename) via an RPC call to semgrep-core.
    Raises an Exception if validation fails. Uses filename and source_hash to enhance error logging. Ignores some types of validation errors.

    When rule_spans is provided, validation errors are mapped back to the
    originating rule's YAML location using core_error.rule_id. display_source_hash
    should be the hash of the original YAML (not the JSON dump sent to the RPC)
    so the whole-file fallback span renders the YAML.
    """
    if filename and Path(filename).exists():
        path = filename
    elif rules_tmp_path and Path(rules_tmp_path).exists():
        path = rules_tmp_path
    else:
        raise SemgrepError(
            "Cannot execute RPC validation without a rules_tmp_path or filename",
            code=MISSING_CONFIG_EXIT_CODE,
        )

    core_error = rpc_validate(out.Fpath(path))
    logger.debug(f"semgrep-core validation response: {core_error=}")
    if core_error is None:
        logger.debug("semgrep-core validation succeeded")
        return

    if core_error.severity in {default_level, warning_level}:
        message = core_error.message
        if message.startswith("invalid regex"):
            # TODO: Previously, with the Python JSON Schema based validation
            # invalid regex in the rules files would cause an exit with
            # FATAL_EXIT_CODE. We are trying to keep that behavior with the RPC
            # validation, currently. But, once RPC validation is merged, we
            # maybe better off removing this "workaround".
            return
        if message.startswith("invalid language"):
            # Ignore invalid language errors. They are handled by
            # _LanguageData.resolve with the correct exit code.
            return
        rule_id = core_error.rule_id.value if core_error.rule_id else None
        span = _rule_span_or_file_fallback(
            rule_id,
            rule_spans,
            display_source_hash if display_source_hash is not None else source_hash,
            filename,
        )
        logger.debug(f"semgrep-core validation error span: {span}")
        raise InvalidRuleSchemaError(
            short_msg="Invalid rule schema",
            long_msg=message,
            spans=[span],
        )


def run_rpc_validate_exn(rules_tmp_path: str) -> None:
    """
    Applies validation to a file at rules_tmp_path via semgrep-core.
    Raises an Exception if validation fails.
    """
    try:
        error = rpc_validate(out.Fpath(rules_tmp_path))
        logger.debug(f"semgrep-core validation response: {error=}")
        if error is None:
            logger.debug("semgrep-core validation succeeded")
            return
        raise RpcValidationError(error)
    except Exception as e:
        raise e


def validate_yaml_json_schema(
    data: YamlTree,
) -> None:
    """
    Applies validation to a YamlTree of the form {"rules": [{<rule_1>}, {<rule_2}, ...]} via jsonschema validation.
    Raises an Exception if validation fails.
    """
    try:
        # Now enter the jsonschema validation for the custom error messages
        with telemetry.TRACER.start_as_current_span("jsonschema.validate"):
            jsonschema.validate(data.unroll(), RuleSchema.get(), cls=Draft7Validator)
    except jsonschema.ValidationError as ve:
        message = _validation_error_message(ve)
        item = data

        root_error = ve
        while root_error.parent is not None:
            root_error = cast(jsonschema.ValidationError, root_error.parent)

        for el in root_error.absolute_path:
            item = item.value[el]

        raise InvalidRuleSchemaError(
            short_msg="Invalid rule schema",
            long_msg=message,
            spans=[item.span],
        )


def validate_string_json_schema(
    data: Dict[str, Any],
    source_hash: Optional[SourceFileHash] = None,
    filename: Optional[str] = None,
    rule_spans: Optional[Dict[str, Span]] = None,
) -> None:
    """
    Applies validation to a Dictionary of the form {"rules": [{<rule_1>}, {<rule_2}, ...]} via jsonschema validation.
    Raises an Exception if validation fails.

    This function is very similar to `validate_yaml_json_schema` but acts on the data we get from the backend. Converting the
    input to a YamlTree to re-use the validation paths has negative performance impacts for large numbers of rules, leading to
    30-40 seconds of delay just in config parsing.

    When rule_spans is provided (a mapping of rule id to YAML Span), validation
    errors are mapped back to the originating rule's location in the YAML file.
    """
    try:
        with telemetry.TRACER.start_as_current_span("jsonschema.validate"):
            jsonschema.validate(data, RuleSchema.get(), cls=Draft7Validator)
    except jsonschema.ValidationError as ve:
        message = _validation_error_message(ve)
        span = _resolve_validation_error_span(
            ve, data, rule_spans, source_hash, filename
        )
        raise InvalidRuleSchemaError(
            short_msg="Invalid rule schema",
            long_msg=message,
            spans=[span],
        )


def _rule_span_or_file_fallback(
    rule_id: Optional[str],
    rule_spans: Optional[Dict[str, Span]],
    source_hash: Optional[SourceFileHash],
    filename: Optional[str],
) -> Span:
    """Resolve a rule_id to its YAML Span, with a whole-file fallback.

    Shared between the jsonschema and RPC validation paths. Used to point
    schema-validation errors back at the originating rule's location in the
    user's YAML when possible.
    """
    if rule_id and rule_spans and rule_id in rule_spans:
        return rule_spans[rule_id]
    if source_hash is not None:
        return Span(
            start=Position(line=1, col=1, offset=-1),
            end=Position(line=1, col=1, offset=-1),
            file=filename,
            source_hash=source_hash,
        )
    return EmptySpan


def _resolve_validation_error_span(
    error: jsonschema.exceptions.ValidationError,
    data: Dict[str, Any],
    rule_spans: Optional[Dict[str, Span]],
    source_hash: Optional[SourceFileHash],
    filename: Optional[str],
) -> Span:
    """Map a jsonschema ValidationError back to a YAML source span when possible.

    Walks the error's absolute_path to identify the failing rule index and its
    id, then defers to `_rule_span_or_file_fallback` for the lookup + fallback.
    """
    rule_id: Optional[str] = None
    root_error = error
    while root_error.parent is not None:
        root_error = cast(jsonschema.exceptions.ValidationError, root_error.parent)

    path = list(root_error.absolute_path)
    if len(path) >= 2 and path[0] == "rules" and isinstance(path[1], int):
        rule_idx = path[1]
        rules = data.get("rules", [])
        if rule_idx < len(rules):
            rule_id = rules[rule_idx].get("id", "")

    return _rule_span_or_file_fallback(rule_id, rule_spans, source_hash, filename)
