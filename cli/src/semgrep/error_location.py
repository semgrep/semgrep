#
# Definitions for locations used in parsing errors.
#
# (used to be in rule_lang.py but was taken out to avoid circular dependencies
# with error.py)
#
import hashlib
from typing import Dict
from typing import List
from typing import NewType
from typing import Optional
from typing import Union

from attrs import evolve
from attrs import frozen
from ruamel.yaml import Node

import semgrep.semgrep_interfaces.semgrep_output_v1 as out

# Do not construct SourceFileHash directly, use `SpanBuilder().add_source`
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


# TODO: use out.Position directly
# If we switch to using out.Position directly, we get the following error:
# $ pipenv run pytest -k 'test_osv_parsing[targets/dependency_aware/osv_parsing/pnpm/no-packages/pnpm-lock.yaml]'
# ...
# E           attr.exceptions.NotAnAttrsClassError: <class 'semgrep.semgrep_interfaces.semgrep_output_v1.Position'> is not an attrs-decorated class.
#
# This looks like it's because the generated Position class uses dataclasses
# rather than attrs.
#
@frozen(repr=False)
class Position:
    """
    Position within a file.
    :param line: 1-indexed line number
    :param col: 1-indexed column number

    line & column are 1-indexed for compatibility with semgrep-core which also produces 1-indexed results
    """

    line: int
    col: int
    offset: int  # set to -1 if unknown

    def to_Position(self) -> out.Position:
        return out.Position(line=self.line, col=self.col, offset=self.offset)

    def to_dict(self) -> dict:
        return {"line": self.line, "col": self.col, "offset": self.offset}

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
            context_start = self.context_start.to_Position()
        context_end = None
        if self.context_end:
            context_end = self.context_end.to_Position()

        return out.ErrorSpan(
            config_path=self.config_path,
            context_start=context_start,
            context_end=context_end,
            file=out.Fpath(self.file if self.file else "<No file>"),
            start=self.start.to_Position(),
            end=self.end.to_Position(),
            source_hash=self.source_hash,
        )

    @classmethod
    def from_node(
        cls, node: Node, source_hash: SourceFileHash, filename: Optional[str]
    ) -> "Span":
        start = Position(
            line=node.start_mark.line + 1, col=node.start_mark.column + 1, offset=-1
        )
        end = Position(
            line=node.end_mark.line + 1, col=node.end_mark.column + 1, offset=-1
        )
        return Span(start=start, end=end, file=filename, source_hash=source_hash).fix()

    @classmethod
    def from_string(cls, s: str, filename: Optional[str] = None) -> "Span":
        src_hash = SourceTracker.add_source(s)
        start = Position(line=1, col=1, offset=-1)
        lines = s.splitlines()
        end = Position(line=len(lines), col=len(lines[-1]), offset=-1)
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
                        start = Position(cur_line + 1, cur_col + 2, offset=-1)
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
                end=Position(line=self.start.line + lines, col=0, offset=-1),
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
                context_start=Position(
                    col=0, line=max(0, self.start.line - before), offset=-1
                ),
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
                    offset=-1,
                ),
            )
        return new

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} start={self.start} end={self.end}>"
