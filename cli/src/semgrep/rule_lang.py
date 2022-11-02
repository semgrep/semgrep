import hashlib
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
from typing import NewType
from typing import Optional
from typing import Set
from typing import Tuple
from typing import TypeVar
from typing import Union

import jsonschema.exceptions
from attrs import evolve
from attrs import frozen
from jsonschema.validators import Draft7Validator
from ruamel.yaml import MappingNode
from ruamel.yaml import Node
from ruamel.yaml import RoundTripConstructor
from ruamel.yaml import YAML

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT

# Do not construct SourceFileHash directly, use `SpanBuilder().add_source`
SourceFileHash = NewType("SourceFileHash", str)


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


class SourceTracker:
    """
    Singleton class tracking mapping from filehashes -> file contents to support
    building error messages from Spans
    """

    # sources are a class variable to share state
    sources: Dict[SourceFileHash, List[str]] = {}

    @classmethod
    def add_source(cls, source: str) -> SourceFileHash:
        file_hash = cls._src_to_hash(source)
        cls.sources[file_hash] = source.splitlines()
        return file_hash

    @classmethod
    def source(cls, source_hash: SourceFileHash) -> List[str]:
        return cls.sources[source_hash]

    @staticmethod
    def _src_to_hash(contents: Union[str, bytes]) -> SourceFileHash:
        if isinstance(contents, str):
            contents = contents.encode("utf-8")
        return SourceFileHash(hashlib.sha256(contents).hexdigest())


# TODO: use out.PositionBis directly
@frozen(repr=False)
class Position:
    """
    Position within a file.
    :param line: 1-indexed line number
    :param col: 1-indexed column number

    line & column are 0 indexed for compatibility with semgrep-core which also produces 1-indexed results
    """

    line: int
    col: int

    def to_PositionBis(self) -> out.PositionBis:
        return out.PositionBis(line=self.line, col=self.col)

    def next_line(self) -> "Position":
        return evolve(self, line=self.line + 1)

    def previous_line(self) -> "Position":
        return evolve(self, line=self.line - 1)

    def to_dict(self) -> dict:
        return {"line": self.line, "col": self.col}

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} line={self.line} col={self.col}>"


# TODO: use out.ErrorSpan directly
@frozen(repr=False)
class Span:
    """
    Spans are immutable objects, representing segments of code. They have a central focus area, and
    optionally can contain surrounding context.
    """

    start: Position
    end: Position
    source_hash: SourceFileHash
    file: Optional[str]

    # ???
    context_start: Optional[Position] = None
    context_end: Optional[Position] = None

    # The path to the pattern in the yaml rule
    # and an adjusted start/end within just the pattern
    # Used to report playground parse errors in the simpler editor
    config_path: Optional[List[str]] = None
    config_start: Optional[Position] = None
    config_end: Optional[Position] = None

    def to_ErrorSpan(self) -> out.ErrorSpan:
        context_start = None
        if self.context_start:
            context_start = self.context_start.to_PositionBis()
        context_end = None
        if self.context_end:
            context_end = self.context_end.to_PositionBis()

        return out.ErrorSpan(
            config_path=self.config_path,
            context_start=context_start,
            context_end=context_end,
            file=self.file if self.file else "<No file>",
            start=self.start.to_PositionBis(),
            end=self.end.to_PositionBis(),
            source_hash=self.source_hash,
        )

    @classmethod
    def from_node(
        cls, node: Node, source_hash: SourceFileHash, filename: Optional[str]
    ) -> "Span":
        start = Position(line=node.start_mark.line + 1, col=node.start_mark.column + 1)
        end = Position(line=node.end_mark.line + 1, col=node.end_mark.column + 1)
        return Span(start=start, end=end, file=filename, source_hash=source_hash).fix()

    @classmethod
    def from_string(cls, s: str, filename: Optional[str] = None) -> "Span":
        src_hash = SourceTracker.add_source(s)
        start = Position(1, 1)
        lines = s.splitlines()
        end = Position(line=len(lines), col=len(lines[-1]))
        return Span(start=start, end=end, file=filename, source_hash=src_hash)

    def fix(self) -> "Span":
        # some issues in ruamel lead to bad spans
        # correct empty spans by rewinding to the last non-whitespace character:
        if self.start == self.end:
            src = SourceTracker.source(self.source_hash)
            cur_line = self.start.line - 1
            cur_col = self.start.col - 2
            while cur_line > 0:
                while cur_col > 0:
                    cur_char = src[cur_line][cur_col]
                    if cur_char.isspace():
                        cur_col -= 1
                    else:
                        # assign the span to the whitespace
                        start = Position(cur_line + 1, cur_col + 2)
                        end = evolve(start, col=cur_col + 3)
                        return evolve(self, start=start, end=end)
                cur_line -= 1
                cur_col = len(src[cur_line]) - 1
        return self

    def truncate(self, lines: int) -> "Span":
        """
        Produce a new span truncated to at most `lines` starting from the start line.
        - start_context is not considered.
        - end_context is removed
        """
        if self.end.line - self.start.line > lines:
            return evolve(
                self,
                end=Position(line=self.start.line + lines, col=0),
                context_end=None,
            )
        return self

    def with_context(
        self, before: Optional[int] = None, after: Optional[int] = None
    ) -> "Span":
        """
        Expand
        """
        new = self
        if before is not None:
            new = evolve(
                new,
                context_start=Position(col=0, line=max(0, self.start.line - before)),
            )

        if after is not None:
            new = evolve(
                new,
                context_end=Position(
                    col=0,
                    line=min(
                        len(SourceTracker.source(self.source_hash)),
                        self.end.line + after,
                    ),
                ),
            )
        return new

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} start={self.start} end={self.end}>"


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


def parse_yaml_preserve_spans(contents: str, filename: Optional[str]) -> YamlTree:
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

            if r is None:
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

    if not data:
        raise EmptyYamlException()

    validate_yaml(data)

    if not isinstance(data, YamlTree):
        raise Exception(
            f"Something went wrong parsing Yaml (expected a YamlTree as output, but got {type(data).__name__}): {PLEASE_FILE_ISSUE_TEXT}"
        )
    return data


class RuleValidation:
    REQUIRE_REGEX = re.compile(r"'(.*)' is a required property")
    PATTERN_KEYS = {
        "match",
        "pattern",
        "pattern-either",
        "pattern-regex",
        "patterns",
        "pattern-sinks",
        "pattern-sources",
        "join",
    }
    INVALID_SENTINEL = " is not allowed for "
    INVALID_FOR_MODE_SENTINEL = "False schema does not allow"
    BAD_TYPE_SENTINEL = "is not of type"
    BANNED_SENTINEL = "Additional properties are not allowed"
    REDUNDANT_SENTINEL = "is valid under each of"


def _validation_error_message(error: jsonschema.exceptions.ValidationError) -> str:
    """
    Heuristic that returns meaningful error messages in all examples from
    tests/e2e/rules/syntax/badXXX.yaml
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


def validate_yaml(data: YamlTree) -> None:
    from semgrep.error import InvalidRuleSchemaError

    try:
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
