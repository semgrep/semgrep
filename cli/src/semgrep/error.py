import dataclasses
import inspect
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import cast
from typing import List
from typing import Optional
from typing import Sequence
from typing import Tuple

import attr

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.constants import Colors
from semgrep.error_location import Position
from semgrep.error_location import SourceTracker
from semgrep.error_location import Span
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

OK_EXIT_CODE = 0
FINDINGS_EXIT_CODE = 1
FATAL_EXIT_CODE = 2
TARGET_PARSE_FAILURE_EXIT_CODE = 3
RULE_PARSE_FAILURE_EXIT_CODE = 4
UNPARSEABLE_YAML_EXIT_CODE = 5
# the commented one below are not used anymore
# NEED_ARBITRARY_CODE_EXEC_EXIT_CODE = 6
MISSING_CONFIG_EXIT_CODE = 7
INVALID_LANGUAGE_EXIT_CODE = 8
# MATCH_TIMEOUT_EXIT_CODE = 9
# MATCH_MAX_MEMORY_EXIT_CODE = 10
# LEXICAL_ERROR_EXIT_CODE = 11
# TOO_MANY_MATCHES_EXIT_CODE = 12
INVALID_API_KEY_EXIT_CODE = 13
SCAN_FAIL_EXIT_CODE = 14

default_level = out.ErrorSeverity(out.Error_())


class SemgrepError(Exception):
    """
    Parent class of all exceptions we anticipate in Semgrep commands

    All Semgrep Exceptions are caught and their error messages
    are displayed to the user.

    For pretty-printing, exceptions should override `__str__`.
    """

    # In theory we should define those fields here:
    # code: int
    # level: out.ErrorSeverity
    # type_: out.CoreErrorKind

    def __init__(
        self,
        *args: object,
        code: int = FATAL_EXIT_CODE,
        level: out.ErrorSeverity = default_level,
    ) -> None:
        self.code = code
        self.level = level
        super().__init__(*args)

    def to_CliError(self) -> out.CliError:
        err = out.CliError(code=self.code, type_=self.type_(), level=self.level)
        return self.adjust_CliError(err)

    # to be overridden in children
    def type_(self) -> out.ErrorType:
        return out.ErrorType(out.SemgrepError())

    # to be overridden in children
    def adjust_CliError(self, base: out.CliError) -> out.CliError:
        """
        Default implementation. Subclasses should override to provide custom information.
        """
        return dataclasses.replace(base, message=str(self))

    def format_for_terminal(self) -> str:
        level_tag = (
            with_color(Colors.red, "[", bgcolor=Colors.red)
            + with_color(
                Colors.forced_white,
                cast(str, self.level.to_json()).upper(),
                bgcolor=Colors.red,
                bold=True,
            )
            + with_color(Colors.red, "]", bgcolor=Colors.red)
        )

        return f"{level_tag} {self}"


# used in text and sarif output, and currently also stored in our metrics
# payload.errors.errors
def error_type_string(type_: out.ErrorType) -> str:
    # convert to the same string of out.ParseError for now
    if isinstance(type_.value, out.PartialParsing):
        return error_type_string(out.ErrorType(out.ParseError()))
    # constructors with arguments
    if isinstance(type_.value, out.PatternParseError):
        return error_type_string(out.ErrorType(out.PatternParseError0()))
    if isinstance(type_.value, out.IncompatibleRule_):
        return error_type_string(out.ErrorType(out.IncompatibleRule0()))
    # All the other cases don't have arguments in Semgrep_output_v1.atd
    # and have some <json name="..."> annotations to generate the right string
    else:
        return str(type_.to_json())


def is_real_error(severity: out.ErrorSeverity) -> bool:
    return severity.kind == "Error_"


def select_real_errors(errors: List[SemgrepError]) -> List[SemgrepError]:
    return [x for x in errors if is_real_error(x.level)]


@dataclass()
class SemgrepCoreError(SemgrepError):
    code: int
    level: out.ErrorSeverity
    # TODO: spans are used only for PatternParseError
    spans: Optional[List[out.ErrorSpan]]
    core: out.CoreError

    def type_(self) -> out.ErrorType:
        return self.core.error_type

    def adjust_CliError(self, base: out.CliError) -> out.CliError:
        base = dataclasses.replace(base, message=str(self))
        if self.core.rule_id:
            base = dataclasses.replace(base, rule_id=self.core.rule_id)

        # For rule errors path is a temp file so for now will just be confusing to add
        if not isinstance(
            self.core.error_type.value, out.RuleParseError
        ) and not isinstance(self.core.error_type.value, out.PatternParseError):
            base = dataclasses.replace(base, path=self.core.location.path)

        if self.spans:
            base = dataclasses.replace(base, spans=self.spans)

        return base

    @property
    def is_special_interfile_analysis_error(self) -> bool:
        """
        These errors indicate that multifile analysis did not
        successfully ran, but we were able to get results anyway.
        They should not block, but they are still errors so that
        they display as errors

        TODO remove this when we remove the interfile specific errors
        """
        return isinstance(
            self.core.error_type.value, out.OutOfMemoryDuringInterfile
        ) or isinstance(self.core.error_type.value, out.TimeoutDuringInterfile)

    def is_timeout(self) -> bool:
        """
        Return if this error is a match timeout
        """
        return isinstance(self.core.error_type.value, out.Timeout)

    def is_missing_plugin(self) -> bool:
        """
        Return if this error is due to a missing plugin
        """
        return isinstance(self.core.error_type.value, out.MissingPlugin)

    @property
    def _error_message(self) -> str:
        """
        Generate error message exposed to user
        """
        if self.core.rule_id:
            # For rule errors, the path is a temporary JSON file containing
            # the rule(s).
            if isinstance(self.core.error_type.value, out.RuleParseError) or isinstance(
                self.core.error_type.value, out.PatternParseError
            ):
                error_context = f"in rule {self.core.rule_id.value}"
            elif isinstance(self.core.error_type.value, out.IncompatibleRule_):
                error_context = self.core.rule_id.value
            elif isinstance(self.core.error_type.value, out.MissingPlugin):
                error_context = f"for rule {self.core.rule_id.value}"
            else:
                # This message is suitable only if the error is in a target file:
                error_context = f"when running {self.core.rule_id.value} on {self.core.location.path.value}"
        else:
            error_context = f"at line {self.core.location.path.value}:{self.core.location.start.line}"

        return f"{error_type_string(self.core.error_type)} {error_context}:\n {self.core.message}"

    @property
    def _stack_trace(self) -> str:
        """
        Returns stack trace if error_type is Fatal error else returns empty strings
        """
        if isinstance(self.core.error_type.value, out.FatalError):
            error_trace = self.core.details or "<no stack trace returned>"
            return f"\n====[ BEGIN error trace ]====\n{error_trace}=====[ END error trace ]=====\n"
        else:
            return ""

    def __str__(self) -> str:
        return self._error_message + self._stack_trace

    # TODO: I didn't manage to get out.Error to be hashable because it contains lists or
    # objects (e.g., Error_) which are not hashable
    def __hash__(self) -> int:
        return hash(
            (
                self.code,
                self.level,
                self.core.rule_id,
                self.core.error_type.kind,
                self.core.location.path.value,
                self.core.location.start,
                self.core.location.end,
                self.core.message,
                self.core.details,
            )
        )


@attr.s(auto_attribs=True, frozen=True)
class FilesNotFoundError(SemgrepError):
    level = out.ErrorSeverity(out.Error_())
    code = FATAL_EXIT_CODE
    paths: Sequence[Path]

    def __str__(self) -> str:
        lines = (f"File not found: {pathname}" for pathname in self.paths)
        return "\n".join(lines)


def span_list_to_tuple(spans: List[Span]) -> Tuple[Span, ...]:
    """
    Helper converter so mypy can track that we are converting
    from list of spans to tuple of spans
    """
    return tuple(spans)


def add_to_line(pos: Position, num_lines: int) -> Position:
    return Position(col=pos.col, line=pos.line + num_lines, offset=-1)


def previous_line(pos: Position) -> Position:
    return add_to_line(pos, -1)


def next_line(pos: Position) -> Position:
    return add_to_line(pos, 1)


@attr.s(auto_attribs=True, eq=True, frozen=True)
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

    short_msg: str = attr.ib()
    long_msg: Optional[str] = attr.ib()
    spans: List[Span] = attr.ib(converter=span_list_to_tuple)
    help: Optional[str] = attr.ib(default=None)

    def __attrs_post_init__(self) -> None:
        if not hasattr(self, "code"):
            raise ValueError("Inheritors of SemgrepError must define an exit code")

        if not hasattr(self, "level"):
            raise ValueError("Inheritors of SemgrepError must define a level")

    def adjust_CliError(self, base: out.CliError) -> out.CliError:
        base = dataclasses.replace(
            base,
            short_msg=self.short_msg,
            long_msg=self.long_msg,
            level=self.level,
            spans=[s.to_ErrorSpan() for s in self.spans],
        )
        # otherwise, we end up with `help: null` in JSON
        if self.help:
            base = dataclasses.replace(base, help=self.help)
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
            return with_color(Colors.bright_blue, base_str.ljust(width) + "| ")
        else:
            return with_color(Colors.bright_blue, "".ljust(width) + "| ")

    def _format_code_segment(
        self,
        start: Position,
        end: Position,
        source: List[str],
        part_of_span: Span,
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
        return self.short_msg

    def format_for_terminal(self) -> str:
        """
        Format this exception into a pretty string with context and color
        """
        header = f"{with_color(Colors.red, 'semgrep ' + self.level.to_json())}: {self.short_msg}"
        snippets = []
        for span in self.spans:
            if span.file != "semgrep temp file":
                location_hint = f"  --> {span.file}:{span.start.line}"
                snippet = [location_hint]
            else:
                snippet = []

            # all the lines of code in the file this comes from
            source: List[str] = SourceTracker.source(span.source_hash)

            # First, print the span from `context_start` to `start`
            # Next, sprint the focus of the span from `start` to `end`
            # If the actual span is only 1 line long, use `column` information to highlight the exact problem
            # Finally, print end context from `end` to `context_end`
            if span.context_start:
                snippet += self._format_code_segment(
                    span.context_start, previous_line(span.start), source, span
                )
            snippet += self._format_code_segment(span.start, span.end, source, span)
            # Currently, only span highlighting if it's a one line span
            if span.start.line == span.end.line:
                error = with_color(Colors.red, (span.end.col - span.start.col) * "^")
                snippet.append(
                    self._format_line_number(span, None)
                    + " " * (span.start.col - 1)
                    + error
                )
            if span.context_end:
                snippet += self._format_code_segment(
                    next_line(span.end), span.context_end, source, span
                )

            snippets.append("\n".join(snippet))
        snippet_str = "\n".join(snippets)
        if self.help:
            help_str = f"= {with_color(Colors.cyan, 'help', bold=True)}: {self.help}"
        else:
            help_str = ""

        # TODO remove this when temp files are no longer in error messages
        if snippet_str == "":
            snippet_str_with_newline = ""
        else:
            snippet_str_with_newline = f"{snippet_str}\n"
        return f"{header}\n{snippet_str_with_newline}{help_str}\n{with_color(Colors.red, self.long_msg or '')}\n"


@attr.s(frozen=True, eq=True)
class InvalidRuleSchemaError(ErrorWithSpan):
    code = RULE_PARSE_FAILURE_EXIT_CODE
    level = out.ErrorSeverity(out.Error_())

    def type_(self) -> out.ErrorType:
        return out.ErrorType(out.InvalidRuleSchemaError())


@attr.s(frozen=True, eq=True)
class UnknownLanguageError(ErrorWithSpan):
    code = INVALID_LANGUAGE_EXIT_CODE
    level = out.ErrorSeverity(out.Error_())

    def type_(self) -> out.ErrorType:
        return out.ErrorType(out.UnknownLanguageError())


# cf. https://stackoverflow.com/questions/1796180/how-can-i-get-a-list-of-all-classes-within-current-module-in-python/1796247#1796247
# This is used only in join_rules.py
ERROR_MAP = {
    classname: classdef
    for classname, classdef in inspect.getmembers(
        sys.modules[__name__],
        lambda member: inspect.isclass(member) and member.__module__ == __name__,
    )
}
