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
from typing import TypeVar
from typing import Union

import attr
import jsonschema.exceptions
import ruamel.yaml
from jsonschema.validators import Draft7Validator
from ruamel.yaml import Node
from ruamel.yaml import RoundTripConstructor
from ruamel.yaml import YAML

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT

# Do not construct SourceFileHash directly, use `SpanBuilder().add_source`
SourceFileHash = NewType("SourceFileHash", str)


class RuleSchema:
    _schema: Dict[str, Any] = {}

    @classmethod
    def get(cls) -> Dict[str, Any]:  # type: ignore
        """
        Returns the rule schema

        Not thread safe.
        """
        if not cls._schema:
            schema_path = Path(__file__).parent / "rule_schema.yaml"
            with schema_path.open() as fd:
                cls._schema = ruamel.yaml.safe_load(fd)
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


@attr.s(auto_attribs=True, frozen=True, repr=False)
class Position:
    """
    Position within a file.
    :param line: 1-indexed line number
    :param col: 1-indexed column number

    line & column are 0 indexed for compatibility with semgrep-core which also produces 1-indexed results
    """

    line: int
    col: int

    def next_line(self) -> "Position":
        return attr.evolve(self, line=self.line + 1)

    def previous_line(self) -> "Position":
        return attr.evolve(self, line=self.line - 1)

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} line={self.line} col={self.col}>"


@attr.s(auto_attribs=True, frozen=True, repr=False)
class Span:
    """
    Spans are immutable objects, representing segments of code. They have a central focus area, and
    optionally can contain surrounding context.
    """

    start: Position
    end: Position
    source_hash: SourceFileHash
    file: Optional[str]
    context_start: Optional[Position] = None
    context_end: Optional[Position] = None

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
                        end = attr.evolve(start, col=cur_col + 3)
                        return attr.evolve(self, start=start, end=end)
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
            return attr.evolve(
                self,
                end=Position(line=self.start.line + lines, col=0),
                context_end=None,
            )
        return self

    def extend_to(self, span: "Span", context_only: bool = True) -> "Span":
        """
        Extend this span to go as far as `span`.
        :param span: The span to extend to
        :param context_only: If true, the additional lines will only be marked as context. These will be displayed,
        but unlike to core span area, they won't be highlighted in error messages.
        """
        if context_only:
            return attr.evolve(self, context_end=span.context_end or span.end)
        else:
            return attr.evolve(self, end=span.end, context_end=span.context_end)

    def with_context(
        self, before: Optional[int] = None, after: Optional[int] = None
    ) -> "Span":
        """
        Expand
        """
        new = self
        if before is not None:
            new = attr.evolve(
                new,
                context_start=Position(col=0, line=max(0, self.start.line - before)),
            )

        if after is not None:
            new = attr.evolve(
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
        elif isinstance(self.value, str) or isinstance(self.value, int):
            return self.value
        else:
            raise ValueError(
                f"Invalid YAML tree structure (expected a list, dict, tree, int or str, found: {type(self.value).__name__}: {self.value}"
            )

    @classmethod
    def wrap(cls, value: YamlValue, span: Span) -> "YamlTree":  # type: ignore
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


def parse_yaml(contents: str) -> Dict[str, Any]:
    # this uses the `RoundTripConstructor` which inherits from `SafeConstructor`
    yaml = YAML(typ="rt")
    return yaml.load(StringIO(contents))  # type: ignore


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

    yaml = YAML()
    yaml.Constructor = SpanPreservingRuamelConstructor
    data = yaml.load(StringIO(contents))

    validate_yaml(data)

    if not isinstance(data, YamlTree):
        raise Exception(
            f"Something went wrong parsing Yaml (expected a YamlTree as output, but got {type(data).__name__}): {PLEASE_FILE_ISSUE_TEXT}"
        )
    return data


class RuleValidation:
    REQUIRE_REGEX = re.compile(r"'(.*)' is a required property")
    PATTERN_KEYS = {
        "pattern",
        "pattern-either",
        "pattern-regex",
        "patterns",
        "pattern-sinks",
        "pattern-sources",
    }
    INVALID_SENTINEL = " is not allowed for "
    BAD_TYPE_SENTINEL = "is not of type"
    BANNED_SENTINEL = "Additional properties are not allowed"


def _validation_error_message(error: jsonschema.exceptions.ValidationError) -> str:  # type: ignore
    """
    Heuristic that returns meaningful error messages in all examples from
    tests/e2e/rules/syntax/badXXX.yaml
    """

    contexts = list(error.parent.context) if error.parent else [error]
    bad_type = set()
    invalid_keys = set()
    any_of_invalid_keys = set()
    required = set()
    banned = set()
    for m in (c.message for c in contexts):
        if RuleValidation.BAD_TYPE_SENTINEL in m:
            bad_type.add(m)
        if RuleValidation.INVALID_SENTINEL in m:
            ix = m.find(RuleValidation.INVALID_SENTINEL)
            try:
                preamble = ruamel.yaml.safe_load(m[:ix]).get("anyOf")
                postscript = ruamel.yaml.safe_load(
                    m[ix + len(RuleValidation.INVALID_SENTINEL) :]
                )
                for r in (p.get("required", [None])[0] for p in preamble):
                    if r and r in postscript.keys():
                        any_of_invalid_keys.add(r)
            except (json.JSONDecodeError, AttributeError) as ex:
                invalid_keys.add(m)
        if m.startswith(RuleValidation.BANNED_SENTINEL):
            banned.add(m)
        require_matches = RuleValidation.REQUIRE_REGEX.match(m)
        if require_matches:
            required.add(require_matches[1])

    if invalid_keys:
        return "\n".join(sorted(invalid_keys))
    if bad_type:
        return "\n".join(sorted(bad_type))
    if banned:
        return "\n".join(sorted(banned))

    outs = []
    if any_of_invalid_keys:
        keys = ", ".join(f"'{k}'" for k in sorted(any_of_invalid_keys))
        outs.append(f"One of these properties may be invalid: {keys}")
        required = required - RuleValidation.PATTERN_KEYS
    if required:
        keys = ", ".join(f"'{k}'" for k in sorted(required))
        outs.append(f"One of these properties is missing: {keys}")
    if outs:
        return "\n".join(outs)

    return cast(str, contexts[0].message)


def validate_yaml(data: YamlTree) -> None:
    from semgrep.error import InvalidRuleSchemaError

    try:
        jsonschema.validate(data.unroll(), RuleSchema.get(), cls=Draft7Validator)
    except jsonschema.ValidationError as ve:
        message = _validation_error_message(ve)
        item = data

        root_error = ve
        while root_error.parent is not None:
            root_error = root_error.parent

        for el in root_error.absolute_path:
            item = item.value[el]

        raise InvalidRuleSchemaError(
            short_msg="Invalid rule schema", long_msg=message, spans=[item.span],
        )
