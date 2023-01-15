(*
   Translated from rule_lang.py
*)

type span = Semgrep_output_v1_t.error_span

(* TODO: use out.ErrorSpan directly *)
(*
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
*)
