from typing import Any, Dict, List, Sequence, Set, Tuple, Optional
from typing import Generic, TypeVar, NewType, Union

# Do not construct SourceFileHash directly, use `SpanBuilder().add_source`
SourceFileHash = NewType("SourceFileHash", str)

class EmptyYamlException(Exception): ...

class SourceTracker:
    """
    Singleton class tracking mapping from filehashes -> file contents to support
    building error messages from Spans
    """

    @classmethod
    def source(cls, source_hash: SourceFileHash) -> List[str]: ...

class Position:
    """
    Position within a file.
    :param line: 1-indexed line number
    :param col: 1-indexed column number

    line & column are 0 indexed for compatibility with semgrep-core which also produces 1-indexed results
    """

    line: int
    col: int

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
    # The path to the pattern in the yaml rule
    # and an adjusted start/end within just the pattern
    # Used to report playground parse errors in the simpler editor
    config_path: Optional[Tuple[Any, ...]] = None
    config_start: Optional[Position] = None
    config_end: Optional[Position] = None

    @classmethod
    def from_string(cls, s: str, filename: Optional[str] = None) -> "Span": ...

EmptySpan = ...

# Actually recursive but mypy is unhelpful
YamlValue = Union[str, int, List[Any], Dict[str, Any]]
LocatedYamlValue = Union[str, int, List["YamlTree"], "YamlMap"]
T = TypeVar("T", bound=LocatedYamlValue)

class YamlTree(Generic[T]):
    def __init__(self, value: T, span: Span): ...
    @classmethod
    def wrap(cls, value: YamlValue, span: Span) -> "YamlTree":
        """
        Wraps a value in a YamlTree and attaches the span everywhere.
        This exists so you can take generate a datastructure from user input, but track all the errors within that
        datastructure back to the user input
        """
        ...

class YamlMap:
    """
    To preserve span information for keys, which we commonly use in error messages,
    make a custom map type that is indexable by str, but provides views into all
    necessary spans
    """

    def __init__(self, internal: Dict[YamlTree[str], YamlTree]): ...

def parse_yaml_preserve_spans(contents: str, filename: Optional[str]) -> YamlTree:
    """
    parse yaml into a YamlTree object. The resulting spans are tracked in SourceTracker
    so they can be used later when constructing error messages or displaying context.

    :raise jsonschema.exceptions.SchemaError: if config is invalid
    """
    ...

class RuleValidation:
    # TODO: type?
    PATTERN_KEYS = ...
