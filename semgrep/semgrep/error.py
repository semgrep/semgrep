from typing import Any
from typing import Dict
from typing import List
from typing import Optional

import attr
from colorama import Fore

from semgrep.rule_lang import Position
from semgrep.rule_lang import SourceTracker
from semgrep.rule_lang import Span
from semgrep.util import with_color

OK_EXIT_CODE = 0
FINDINGS_EXIT_CODE = 1
FATAL_EXIT_CODE = 2
INVALID_CODE_EXIT_CODE = 3
INVALID_PATTERN_EXIT_CODE = 4
UNPARSEABLE_YAML_EXIT_CODE = 5
NEED_ARBITRARY_CODE_EXEC_EXIT_CODE = 6
MISSING_CONFIG_EXIT_CODE = 7

INVALID_LANGUAGE_EXIT_CODE = 8


class SemgrepError(Exception):
    """
    Parent class of all exceptions we anticipate in Semgrep commands

    All Semgrep Exceptions are caught and their error messages
    are displayed to the user.

    For pretty-printing, exceptions should override `__str__`.
    """

    def __init__(self, *args: object, code: int = FATAL_EXIT_CODE) -> None:
        self.code = code

        super().__init__(*args)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "type": self.__class__.__name__,
            "code": self.code,
            **self.to_dict_base(),
        }

    def to_dict_base(self) -> Dict[str, Any]:
        """
        Default implementation. Subclasses should override to provide custom information.
        All values returned must be JSON serializable.
        """
        return {"message": str(self)}


class SemgrepInternalError(Exception):
    """
    Parent class of internal semgrep exceptions that should be handled internally and converted into `SemgrepError`s

    Classes that inherit from SemgrepInternalError should begin with `_`
    """

    pass


class UnknownOperatorError(SemgrepError):
    pass


class ErrorWithSpan(SemgrepError):
    """
    In general, you should not be constructing ErrorWithSpan directly, and instead be constructing a subclass
    that sets the code.

    Error which will print context from the Span. You should provide the most specific span possible,
    eg. if the error is an invalid key, provide exactly the span for that key. You can then expand what's printed
    with span.with_context(...). Conversely, if you don't want to display the entire span, you can use `span.truncate`

    The __str__ method produces the pretty-printed error.
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
        spans: List[Span],
        level: str = "error",
        help: Optional[str] = None,
    ):

        self.short_msg = short_msg
        self.long_msg = long_msg
        self.level = level
        self.spans = spans
        self.help = help
        assert hasattr(
            self, "code"
        ), "Inheritors of SemgrepError must define an exit code"

    def to_dict_base(self) -> Dict[str, Any]:
        base = dict(
            short_msg=self.short_msg,
            long_msg=self.long_msg,
            level=self.level,
            spans=[attr.asdict(s) for s in self.spans],
        )
        # otherwise, we end up with `help: null` in JSON
        if self.help:
            base["help"] = self.help
        return base

    @staticmethod
    def _line_number_width(span: Span) -> int:
        return len(str((span.context_end or span.end).line)) + 1

    @staticmethod
    def _format_line_number(span: Span, line_number: Optional[int]) -> str:
        """
        Produce a string like:
        ` 10 |`

        The amount of padding is set for printing within `span` (so it handles up to `context_end.line`)
        """
        # line numbers are 0 indexed
        width = ErrorWithSpan._line_number_width(span)
        if line_number is not None:
            base_str = str(line_number)
            assert len(base_str) < width
            return with_color(Fore.LIGHTBLUE_EX, base_str.ljust(width) + "| ")
        else:
            return with_color(Fore.LIGHTBLUE_EX, "".ljust(width) + "| ")

    def _format_code_segment(
        self, start: Position, end: Position, source: List[str], part_of_span: Span
    ) -> List[str]:
        """
        Line by line output for a snippet of code from `start_line` to `end_line`
        Each line will be annotated with a line number, properly spaced according to
        the highest line number required to render `span`

        :param start: start position
        :param end: end position

        :returns A list of strings, suitable to be combined with `'\n'.join(...)`
        eg:
        List[
            "5  | def my_func():",
            "6  |   return True"
        ]
        """
        # -1 because positions are 1-indexed
        code_segment = source[start.line - 1 : end.line]
        snippet = []
        for line_num, line in zip(range(start.line, end.line + 1), code_segment):
            snippet.append(f"{self._format_line_number(part_of_span, line_num)}{line}")
        return snippet

    def __str__(self) -> str:
        """
        Format this exception into a pretty string with context and color
        """
        header = f"{with_color(Fore.RED, self.level)}: {self.short_msg}"
        snippets = []
        for span in self.spans:
            location_hint = f"  --> {span.file}:{span.start.line}"
            snippet = [location_hint]

            # all the lines of code in the file this comes from
            source: List[str] = SourceTracker.source(span.source_hash)

            # First, print the span from `context_start` to `start`
            # Next, sprint the focus of the span from `start` to `end`
            # If the actual span is only 1 line long, use `column` information to highlight the exact problem
            # Finally, print end context from `end` to `context_end`
            if span.context_start:
                snippet += self._format_code_segment(
                    span.context_start, span.start.previous_line(), source, span
                )
            snippet += self._format_code_segment(span.start, span.end, source, span)
            # Currently, only span highlighting if it's a one line span
            if span.start.line == span.end.line:
                error = with_color(Fore.RED, (span.end.col - span.start.col) * "^")
                snippet.append(
                    self._format_line_number(span, None)
                    + " " * (span.start.col - 1)
                    + error
                )
            if span.context_end:
                snippet += self._format_code_segment(
                    span.end.next_line(), span.context_end, source, span
                )

            snippets.append("\n".join(snippet))
        snippet_str = "\n".join(snippets)
        if self.help:
            help_str = f"= {with_color(Fore.CYAN, 'help', bold=True)}: {self.help}"
        else:
            help_str = ""
        return f"{header}\n{snippet_str}\n{help_str}\n{with_color(Fore.RED, self.long_msg or '')}\n"


class InvalidPatternError(ErrorWithSpan):
    code = INVALID_PATTERN_EXIT_CODE


class InvalidRuleSchemaError(ErrorWithSpan):
    code = INVALID_PATTERN_EXIT_CODE


class UnknownLanguageError(ErrorWithSpan):
    code = INVALID_LANGUAGE_EXIT_CODE


class InvalidPatternNameError(InvalidRuleSchemaError):
    pass


class _UnknownLanguageError(SemgrepInternalError):
    pass
