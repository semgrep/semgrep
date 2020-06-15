import hashlib
from io import StringIO
from typing import Any
from typing import Dict
from typing import List
from typing import NamedTuple
from typing import NewType
from typing import Optional
from typing import Union

from ruamel.yaml import Node
from ruamel.yaml import RoundTripConstructor
from ruamel.yaml import YAML

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT

# Do not construct directly, use `SpanBuilder().add_source`
SourceFileHash = NewType("SourceFileHash", str)


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


class Position(NamedTuple):
    """
    Position within a file.
    :param line: 1-indexed line number
    :param column: 1-indexed column number

    line & column are 0 indexed for compatibility with semgrep-core which also produces 1-indexed results
    """

    line: int
    col: int

    def next_line(self) -> "Position":
        return self._replace(line=self.line + 1)

    def previous_line(self) -> "Position":
        return self._replace(line=self.line - 1)

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} line={self.line} col={self.col}>"


class Span(NamedTuple):
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
        return Span(start=start, end=end, file=filename, source_hash=source_hash)

    def truncate(self, lines: int) -> "Span":
        """
        Produce a new span truncated to at most `lines` starting from the start line.
        - start_context is not considered.
        - end_context is removed
        """
        if self.end.line - self.start.line > lines:
            return self._replace(
                end=Position(line=self.start.line + lines, col=0), context_end=None
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
            new = new._replace(
                context_start=Position(col=0, line=max(0, self.start.line - before))
            )

        if after is not None:
            new = new._replace(
                context_end=Position(
                    col=0,
                    line=min(
                        len(SourceTracker.source(self.source_hash)),
                        self.end.line + after,
                    ),
                )
            )
        return new

    def as_dict(self) -> Dict[str, Any]:
        return dict(start=self.start._asdict(), end=self.end._asdict(), file=self.file)

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} start={self.start} end={self.end}>"


# Actually recursive but mypy is unhelpful
YamlValue = Union[str, int, List[Any], Dict[str, Any]]
LocatedYamlValue = Union[str, int, List["YamlTree"], Dict["YamlTree", "YamlTree"]]


class YamlTree:
    def __init__(self, value: LocatedYamlValue, span: Span):
        self.value = value
        self.span = span

    # __eq__ and _hash__ delegate to value to support `value['a']` working properly.
    # otherwise, since the key is _actually_ a `Located` object you'd need to give the
    # span to pull it out of the dictionary.
    def __eq__(self, other: Any) -> bool:
        return self.value.__eq__(other)

    def __hash__(self) -> int:
        return hash(self.value)

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
        elif isinstance(self.value, dict):
            return {str(k.unroll()): v.unroll() for k, v in self.value.items()}
        elif isinstance(self.value, YamlTree):
            return self.value.unroll()
        else:
            return self.value

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
                value={
                    YamlTree.wrap(k, span): YamlTree.wrap(v, span)
                    for k, v in value.items()
                },
                span=span,
            )
        elif isinstance(value, YamlTree):
            return value
        else:
            return YamlTree(value, span)


def parse_yaml(contents: str) -> Dict[str, Any]:
    # this uses the `RoundTripConstructor` which inherits from `SafeConstructor`
    yaml = YAML(typ="rt")
    return yaml.load(StringIO(contents))  # type: ignore


def parse_yaml_preserve_spans(contents: str, filename: Optional[str]) -> YamlTree:
    """
    parse yaml into a YamlTree object. The resulting spans are tracked in SourceTracker
    so they can be used later when constructing error messages or displaying context.
    """
    # this uses the `RoundTripConstructor` which inherits from `SafeConstructor`
    source_hash = SourceTracker.add_source(contents)

    class SpanPreservingRuamelConstructor(RoundTripConstructor):
        def construct_object(self, node: Node, deep: bool = False) -> YamlTree:
            r = super().construct_object(node, deep)
            return YamlTree(
                r, Span.from_node(node, source_hash=source_hash, filename=filename)
            )

    yaml = YAML()
    yaml.Constructor = SpanPreservingRuamelConstructor
    data = yaml.load(StringIO(contents))
    if not isinstance(data, YamlTree):
        raise Exception(
            f"Something went wrong parsing Yaml (expected a YamlTree as output): {PLEASE_FILE_ISSUE_TEXT}"
        )
    return data


EmptySpan = parse_yaml_preserve_spans("a: b", None).span
