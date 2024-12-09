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
from typing import Literal
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
from semgrep import tracing
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.error import OK_EXIT_CODE
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.error_location import SourceTracker
from semgrep.error_location import Span
from semgrep.rpc_call import validate as rpc_validate
from semgrep.verbose_logging import getLogger

MISSING_RULE_ID = "no-rule-id"


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
                from semgrep.error import InvalidRuleSchemaError

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
                from semgrep.error import InvalidRuleSchemaError

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


@tracing.trace()
def parse_config_preserve_spans(
    contents: str,
    filename: Optional[str],
    force_jsonschema: bool = False,
    rules_tmp_path: Optional[str] = None,
) -> Tuple[YamlTree, List[SemgrepError]]:
    data = parse_yaml_preserve_spans(contents, filename)
    if not data:
        raise EmptyYamlException()
    errors = validate_yaml(
        data,
        filename,
        no_rewrite_rule_ids=False,
        force_jsonschema=force_jsonschema,
        rules_tmp_path=rules_tmp_path,
    )
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
                for k in context.validator_value
                if "required" in k and k["required"]
            ]
            l = []
            for property in mutex_properties:
                if property and property in context.instance.keys():
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
                    for k in context.validator_value.get("anyOf", [])
                    if "required" in k and k["required"]
                ]
                for r in required_keys:
                    if r and r in context.instance.keys():
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


# In-place removal of rules that don't satisfy min-version or max-version
# constraints. It's not great but easier than having to reconstruct a valid
# YamlTree from the root.
@tracing.trace()
def remove_incompatible_rules_based_on_version(
    root: YamlTree, filename: Optional[str], no_rewrite_rule_ids: bool = False
) -> List[SemgrepError]:
    errors: List[SemgrepError] = []
    root_value = root.value
    if "rules" in root_value:
        rules = root_value["rules"]
        rules_value = rules.value
        ok_rules_value = []
        for rule in rules_value:
            rule_value = rule.value
            rule_id = MISSING_RULE_ID
            if "id" in rule_value:
                rule_id = rule_value["id"].value
                # Turn the rule ID into path.to.name to produce better error
                # messages. This is normally done later in a call to 'get_rules'.
                if not no_rewrite_rule_ids:
                    rule_id = prepend_rule_path(filename or "", rule_id)
            if "min-version" in rule_value:
                min_version = rule_value["min-version"]
                min_version_value = min_version.value
                if Version(__VERSION__) < Version(min_version_value):
                    # coupling: we try to print all the same details as
                    # semgrep-core/osemgrep.
                    msg = (
                        f"This rule requires upgrading Semgrep from version "
                        f"{__VERSION__} to at least {min_version.value}"
                    )
                    errors.append(
                        SemgrepCoreError(
                            code=OK_EXIT_CODE,
                            level=out.ErrorSeverity(out.Info_()),
                            spans=None,
                            core=out.CoreError(
                                error_type=out.ErrorType(
                                    out.IncompatibleRule_(
                                        out.IncompatibleRule(
                                            rule_id=out.RuleId(rule_id),
                                            this_version=out.Version(__VERSION__),
                                            min_version=out.Version(min_version_value),
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
                    )
                    continue
            if "max-version" in rule_value:
                max_version = rule_value["max-version"]
                max_version_value = max_version.value
                if Version(__VERSION__) > Version(max_version_value):
                    msg = (
                        f"This rule is no longer supported by Semgrep. "
                        f"The last compatible version was {max_version_value}. "
                        f"This version of Semgrep is {__VERSION__}"
                    )
                    # coupling: almost the same code as above for min-version
                    errors.append(
                        SemgrepCoreError(
                            code=OK_EXIT_CODE,
                            level=out.ErrorSeverity(out.Info_()),
                            spans=None,
                            core=out.CoreError(
                                error_type=out.ErrorType(
                                    out.IncompatibleRule_(
                                        out.IncompatibleRule(
                                            rule_id=out.RuleId(rule_id),
                                            this_version=out.Version(__VERSION__),
                                            max_version=out.Version(max_version_value),
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
                    )
                    continue
            ok_rules_value.append(rule)
        rules.value = ok_rules_value
    return errors


class RpcValidationError(Exception):
    pass


def run_rpc_validate(rules_tmp_path: str) -> Literal[True]:
    try:
        valid = rpc_validate(out.Fpath(rules_tmp_path))
        logger.debug(f"semgrep-core validation response: {valid=}")
        if valid:
            logger.debug("semgrep-core validation succeeded")
            return True
        raise RpcValidationError("semgrep-core validation failed")
    except Exception as e:
        raise e


@tracing.trace()
def validate_yaml(
    data: YamlTree,
    filename: Optional[str] = None,
    no_rewrite_rule_ids: bool = False,
    force_jsonschema: bool = False,
    rules_tmp_path: Optional[str] = None,
) -> List[SemgrepError]:
    from semgrep.error import InvalidRuleSchemaError

    errors = remove_incompatible_rules_based_on_version(
        data, filename, no_rewrite_rule_ids=no_rewrite_rule_ids
    )
    # If we specifically request jsonschema validation (or tmp_path of the rules was not successfully
    # set), we skip the RPC validation and go straight to jsonschema validation
    skip_rpc_validation = force_jsonschema or not rules_tmp_path
    try:
        if skip_rpc_validation:
            logger.debug(
                "Skipping semgrep-core validation to proceed directly to jsonschema validation"
            )
        else:
            # NOTE: Some rule schema errors are marked as "Other syntax error" by the
            # native RPC-based validation and are included in the errors JSON response
            # without outright rejecting the config. This behavior presents an immediate
            # challenge for certain validation test cases and is called out in SAF-1556.
            try:
                if not rules_tmp_path or not Path.exists(Path(rules_tmp_path)):
                    raise NotImplementedError(
                        "Cannot execute RPC validation without a rules_tmp_path"
                    )
                with tracing.TRACER.start_as_current_span("rpc.validate"):
                    run_rpc_validate(rules_tmp_path=rules_tmp_path)
                    logger.debug("RPC validation succeeded")
                    # If we reach this line, the RPC-based validation passed and we can early return
                    return errors
            except (RpcValidationError, NotImplementedError) as e:
                logger.debug(f"run_rpc_validate failed: {e}")

        # Now enter the jsonschema validation for the custom error messages
        with tracing.TRACER.start_as_current_span("jsonschema.validate"):
            jsonschema.validate(data.unroll(), RuleSchema.get(), cls=Draft7Validator)
        # At this point we have successfully validated the rules
        # and can return any errors
        return errors
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
