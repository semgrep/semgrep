from typing import NamedTuple, Any

class LineCol(NamedTuple):
    line: int
    column: int

class Node(NamedTuple):
    start_mark: LineCol
    end_mark: LineCol

class MappingNode:
    value: Any

class RoundTripConstructor:
    def construct_object(self, node: Node, deep: bool = False) -> Any: ...

def safe_load(stream: Any) -> Any: ...

class YAML:
    Constructor: Any
    representer: Any

    def __init__(self, typ: str = "rt"):
        self.default_flow_style: bool = ...
        ...
    def load(self, stream: Any) -> Any: ...
    def safe_load(self, stream: Any) -> Any: ...
    def dump(self, obj: Any, stream: Any) -> None: ...

class YAMLError(Exception): ...
