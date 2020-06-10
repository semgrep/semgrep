from typing import List
from typing import Optional

from colorama import Fore

from semgrep.rule_lang import Span
from semgrep.rule_lang import SpanBuilder
from semgrep.util import with_color

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


class ErrorWithSpan(SemgrepError):
    """
    Error which will print context from the Span. You should provide the most specific span possible,
    eg. if the error is an invalid key, provide exactly the span for that key. You can then expand what's printed
    with span.with_context(...). Conversely, if you don't want to display the entire span, you can use `span.truncate`

    Here is what the generated error will look like:

        <level>: <short_msg>
          --> <span.filename>:<span.start.line>
        1 | rules:
        2 |   - id: eqeq-is-bad
        3 |     pattern-inside: foo(...)
          |     ^^^^^^^^^^^^^^
        4 |     patterns:
        5 |       - pattern-not: 1 == 1
        = help: <help>
        <long_msg>

    :param short_msg: 1 or 2 word description of the problem (eg. missing key)
    :param level: How bad is the problem? error,warn, etc.
    :param spans: A list of spans to display for context.
    :help help: An optional hint about how to fix the problem
    :cause cause: The underlying exception
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
        width = ErrorWithSpan._line_number_width(span)
        if line_number is not None:
            base_str = str(line_number + 1)
            assert len(base_str) < width
            return with_color(Fore.LIGHTBLUE_EX, base_str.ljust(width) + "| ")
        else:
            return with_color(Fore.LIGHTBLUE_EX, "".ljust(width) + "| ")

    def _format_code_segment(
        self, start_line: int, end_line: int, source: List[str], span: Span
    ) -> List[str]:
        """
        Line by line output for a snippet of code from `start_line` to `end_line`
        Each line will be annotated with a line number, properly spaced according to
        the highest line number required to render `span`

        :param start_line: First LOC, 0 indexed into the file to format
        :param end_line: Last LOC, inclusive, 0 indexed into the file to format

        :returns A list of strings, suitable to be combined with `'\n'.join(...)`

        >>> code_segment(5, 6, [...], span)
        List[
            "5  | def my_func():",
            "6  |   return True"
        ]
        """
        code_segment = source[start_line : end_line + 1]
        snippet = []
        for line_num, line in zip(range(start_line, end_line + 1), code_segment):
            snippet.append(f"{self._format_line_number(span, line_num)}{line}")
        return snippet

    def __repr__(self) -> str:
        """
        Format this exception into a pretty string with context and color
        """
        header = f"{with_color(Fore.RED, self.level)}: {self.short_msg}"
        snippets = []
        for span in self.spans:
            location_hint = f"  --> {span.file}:{span.start.line + 1}"
            snippet = [location_hint]
            source = SpanBuilder().source(span.source_hash)
            # First, print the span from `context_start` to `start`
            # Next, sprint the focus of the span from `start` to `end`
            # If the actual span is only 1 line long, use `column` information to highlight the exact problem
            # Finally, print end context from `end` to `context_end`
            if span.context_start:
                snippet += self._format_code_segment(
                    span.context_start.line, span.start.line - 1, source, span
                )
            snippet += self._format_code_segment(
                span.start.line, span.end.line, source, span
            )
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
                snippet += self._format_code_segment(
                    span.end.line + 1, span.context_end.line, source, span
                )

            snippets.append("\n".join(snippet))
        snippet_str = "\n".join(snippets)
        if self.help:
            help_str = f"= {with_color(Fore.CYAN, 'help', bold=True)}: {self.help}"
        else:
            help_str = ""
        return f"{header}\n{snippet_str}\n{help_str}\n{with_color(Fore.RED, self.long_msg or '')}\n"
