from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Union

from colorama import Fore

from semgrep.constants import OutputFormat
from semgrep.rule_lang import Span
from semgrep.rule_lang import SpanBuilder

OK_EXIT_CODE = 0
FINDINGS_EXIT_CODE = 1
FATAL_EXIT_CODE = 2
INVALID_CODE_EXIT_CODE = 3
INVALID_PATTERN_EXIT_CODE = 4
UNPARSEABLE_YAML_EXIT_CODE = 5
NEED_ARBITRARY_CODE_EXEC_EXIT_CODE = 6
MISSING_CONFIG_EXIT_CODE = 7


class SemgrepError(Exception):
    """
    Parent class of all exceptions we anticipate in Semgrep commands

    All Semgrep Exceptions are caught and their error messages
    are displayed to the user.
    """

    def __init__(self, *args: object, code: int = FATAL_EXIT_CODE) -> None:
        self.code = code

        super().__init__(*args)


class InvalidRuleSchemaError(SemgrepError):
    pass


class InvalidPatternNameError(SemgrepError):
    pass


class UnknownOperatorError(SemgrepError):
    pass


class ErrorWithContext(SemgrepError):
    """
    Error which will print context from the Span. You should provide the most specific span possible,
    eg. if the error is an invalid key, provide exactly the span for that key. You can then expand what's printed
    with span.with_context(...)
    """

    def __init__(
        self,
        short_msg: str,
        long_msg: Optional[str],
        level: str,
        spans: List[Span],
        help: Optional[str] = None,
        cause: Optional[Exception] = None,
    ):
        self.short_msg = short_msg
        self.long_msg = long_msg
        self.level = level
        self.spans = spans
        self.help = help
        self.__cause__ = cause

    @staticmethod
    def _line_number_width(span: Span) -> int:
        return len(str((span.context_end or span.end).line + 1)) + 1

    @staticmethod
    def _format_line_number(span: Span, line_number: Optional[int]) -> str:
        # line numbers are 0 indexed
        width = ErrorWithContext._line_number_width(span)
        if line_number is not None:
            base_str = str(line_number + 1)
            assert len(base_str) < width
            return with_color(Fore.LIGHTBLUE_EX, base_str.ljust(width) + "| ")
        else:
            return with_color(Fore.LIGHTBLUE_EX, "".ljust(width) + "| ")

    def _code_segment(
        self, start_line: int, end_line: int, source: List[str], span: Span
    ) -> List[str]:
        code_segment = source[start_line : end_line + 1]
        snippet = []
        for line_num, line in zip(range(start_line, end_line + 1), code_segment):
            snippet.append(f"{self._format_line_number(span, line_num)}{line}")
        return snippet

    def emit_str(self) -> str:
        header = f"{with_color(Fore.RED, self.level)}: {self.short_msg}"
        snippets = []
        for span in self.spans:
            location_hint = f"  --> {span.file}:{span.start.line + 1}"
            snippet = [location_hint]
            source = SpanBuilder().source(span.source_hash)
            if span.context_start:
                snippet += self._code_segment(
                    span.context_start.line, span.start.line - 1, source, span
                )
            snippet += self._code_segment(span.start.line, span.end.line, source, span)
            # Currently, only span highlighting if it's a one line span
            if span.start.line == span.end.line:
                error = with_color(
                    Fore.RED, (span.end.column - span.start.column) * "^"
                )
                snippet.append(
                    self._format_line_number(span, None)
                    + " " * span.start.column
                    + error
                )
            if span.context_end:
                snippet += self._code_segment(
                    span.end.line + 1, span.context_end.line, source, span
                )

            snippets.append("\n".join(snippet))
        snippet_str = "\n".join(snippets)
        if self.help:
            help_str = f"= {with_color(Fore.CYAN, 'help', bold=True)}: {self.help}"
        else:
            help_str = ""
        return f"{header}\n{snippet_str}\n{help_str}\n{with_color(Fore.RED, self.long_msg or '')}\n"

    def emit(self, output_format: OutputFormat) -> Union[Dict[str, Any], str]:
        if output_format in {OutputFormat.JSON, OutputFormat.JSON_DEBUG}:
            return dict(
                short_msg=self.short_msg,
                long_msg=self.long_msg,
                level=self.level,
                spans=[span.as_dict() for span in self.spans],
            )
        else:
            return self.emit_str()


def with_color(color: str, text: str, bold: bool = False) -> str:
    """
    Wrap text in color & reset
    """
    reset = Fore.RESET
    if bold:
        color = color + "\033[1m"
        reset += "\033[0m"
    return f"{color}{text}{reset}"
