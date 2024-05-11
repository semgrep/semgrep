"""
This module provides a series of helpful utilities for writing lockfile parsers.
Why are all the type annotations strings?
In Python, type annotations do not do anything,
but they are still expressions that get evaluated. The runtime class Parser, as implemented
by parsy, takes no parameters, but our type stubs for parsy give this class a generic type variable
parameter, so we can enforce staticly that parsers are combined in sensible ways. As a result, the
expression Parser[int] is perfectly fine for Mypy, but causes a runtime error. Thankfully, "Parser[int]"
is a perfectly acceptable type annotation for Mypy, and evaluates immediately to string,
causing no runtime errors.
"""
from __future__ import annotations

import re
from base64 import b16encode
from base64 import b64decode
from dataclasses import dataclass
from pathlib import Path
from re import escape
from typing import Any
from typing import Callable
from typing import cast
from typing import Dict
from typing import Generic
from typing import List
from typing import Tuple
from typing import TypeVar

from ruamel.yaml import YAMLError

from semdep.external.parsy import alt
from semdep.external.parsy import fail
from semdep.external.parsy import line_info
from semdep.external.parsy import ParseError
from semdep.external.parsy import Parser
from semdep.external.parsy import regex
from semdep.external.parsy import string
from semdep.external.parsy import success
from semgrep.console import console
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyChild
from semgrep.semgrep_interfaces.semgrep_output_v1 import DependencyParserError
from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import ScaParserName
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")

Pos = Tuple[int, int]


def not_any(*chars: str) -> Parser[str]:
    """
    [chars] must contain only single character strings.
    A parser which matches a series of any character that is *not* in [chars] and returns a string
    """
    return regex(f"[^{escape(''.join(chars))}]+").desc(f"Any char not in {list(chars)}")


def extract_npm_lockfile_hash(s: str | None) -> dict[str, list[str]]:
    """
    Go from:
        sha512-aePbxDmcYW++PaqBsJ+HYUFwCdv4LVvdnhBy78E57PIor8/OVvhMrADFFEDh8DHDFRv/O9i3lPhsENjO7QX0+A==
    To:
        sha512,[the hash encoded into base 16]
    """
    if s is None:
        return {}
    hashes = s.split(" ")
    output = {}
    for h in hashes:
        alg_rest = h.split("-")
        if len(alg_rest) != 2:
            continue
        algorithm, rest = alg_rest
        decode_base_64 = b64decode(rest)
        output[algorithm] = [b16encode(decode_base_64).decode("ascii").lower()]
    return output


# parsy line and column numbers are zero indexed, but editors are generally 1 indexed
# so we add one to the line number account for this, and discard the column number
line_number = line_info.map(lambda t: t[0] + 1)


def mark_line(p: Parser[A]) -> Parser[tuple[int, A]]:
    """
    Returns a parser which gets the current line number, runs [p] and then produces a pair of the line number and the result of [p]
    """
    return line_number.bind(lambda line: p.bind(lambda x: success((line, x))))


def pair(p1: Parser[A], p2: Parser[B]) -> Parser[tuple[A, B]]:
    """
    Returns a parser which runs [p1] then [p2] and produces a pair of the results
    """
    return p1.bind(lambda a: p2.bind(lambda b: success((a, b))))


def triple(p1: Parser[A], p2: Parser[B], p3: Parser[C]) -> Parser[tuple[A, B, C]]:
    """
    Returns a parser which runs [p1] then [p2] then [p3] and produces a triple of the results
    """
    return p1.bind(lambda a: p2.bind(lambda b: p3.bind(lambda c: success((a, b, c)))))


def transitivity(manifest_deps: set[A] | None, dep_sources: list[A]) -> Transitivity:
    """
    Computes the transitivity of a package, based on the set of dependencies from a manifest file
    [manifest_deps] can be None in the case where we did not find a manifest file
    [dep_sources] is a list to account for yarn lockfiles, where a package comes with a list of
    all the version constraints that produced it, and it's possible to have one package installed
    at multiple versions
    In other cases [dep_sources] should just be a list with one element.

    If dealing with a yarn.lock:
      [manifest_deps] will be something like {("foo",">=1.0.0),("bar",">2.1.3")}
      [dep_sources] will be something like [("foo",">=1.0.0"),("foo",">3.1.1")]
    Otherwise:
      [manifest_deps] will be something like {"foo","bar"}
      [dep_sources] will be something like ["foo"]
    """
    if manifest_deps:
        for dep_source in dep_sources:
            if dep_source in manifest_deps:
                return Transitivity(Direct())
        return Transitivity(Transitive())
    else:
        return Transitivity(Unknown())


def become(p1: Parser[A], p2: Parser[A]) -> None:
    """
    Gives [p1] the behavior of [p2] by side effect.
    Typed version of the [become] method on "forward delaration" parsers from semdep.external.parsy.
    You can use this if you need to declare a parser for use in some mutual recursion,
    and then give it an actual definition after delaring other parsers that use it
    """
    p1.__dict__ = p2.__dict__
    p1.__class__ = p2.__class__


def delay(p: Callable[[], Parser[A]]) -> Parser[A]:
    """
    For use when defining (mutually) recursive functions that return parsers. See yarn.py for an example.
    Basically if you have some mutually recursive functions that produce parsers, evaluating one of
    them can cause an infinite loop, even if running the parser would actually not. This lets you
    write an expression [delay(lambda: p(args))], which will immediately terminate, but will evaluate
    p(args) by one step when the parser actually runs.
    """
    return Parser(lambda x, y: p()(x, y))


def quoted(p: Parser[A]) -> Parser[A]:
    """
    Parse [p], surrounded by quotes, ignoring the quotes in the output
    """
    return string('"') >> p << string('"')


integer = regex(r"\d+").map(int)
any_str = regex(".*")
word = not_any(" ")
consume_word = word >> success(None)

line = not_any("\n")
consume_line = line >> success(None)


def upto(
    *s: str,
    include_other: bool = False,
    consume_other: bool = False,
    allow_newline: bool = False,
) -> Parser[str]:
    """
    [s] must be a list of single character strings. These should be all the possible delimiters
    you wanto to parse "up to"
    Useful when defining parsers in terms of delimiters
    Returns a parser which parses anything not in [s], and produces a string of those chars.
    [include_other] will parse the delimiter and append it to the result
    [consume_other] will parse the delimiter and throw it out
    Only one should be used, if you use both behavior is undefined
    [allow_newline] allows newlines to be consumed. Generally this happening is undesirable,
    and indicates that something has gone wrong, but sometimes it may be what you want
    """
    if not allow_newline:
        s = (*s, "\n")
    if include_other:
        return not_any(*s).bind(
            lambda x: alt(*(string(x) for x in s)).map(lambda y: x + y)
        )
    elif consume_other:
        return not_any(*s) << alt(*(string(x) for x in s))
    else:
        return not_any(*s)


def parse_error_to_str(e: ParseError) -> str:
    """
    Stolen from the __str__ method of ParseError in Parsy,
    but without the line information included at the end of the string
    and simply printing the expected list intead of joining it on commas
    in order to easily escape special characters
    """
    expected_list = sorted(e.expected)
    if len(expected_list) == 1:
        # Awful awful hack, we want to print special chars as escaped, but the one custom
        # message we use already has escaped chars in it, so they get double escaped :spiral_eyes:
        return f"expected {expected_list[0] if expected_list[0].startswith('Any char not in') else repr(expected_list[0])}"
    else:
        return f"expected one of {expected_list}"


def filter_on_marked_lines(result: list[Any]) -> list[tuple]:
    return [x for x in result if isinstance(x, tuple)]


@dataclass
class DependencyFileToParse(Generic[A]):
    path: Path
    parser: Parser[A] | Callable[[str], A]
    parser_name: ScaParserName
    preprocessor: Callable[[str], str] = lambda ξ: ξ  # noqa: E731


def parse_dependency_file(
    file_to_parse: DependencyFileToParse[A] | None,
) -> A | DependencyParserError | None:
    """
    Run [parser] on the text in [path]

    PARSER_NAME is used for error reporting to correctly identify which PARSER attempted to parse PATH

    Returns None if path does not exist

    Raises DependencyParserError if it fails to parse the file in PATH with PARSER
    """
    if not file_to_parse:
        return None

    text = file_to_parse.path.read_text(errors="replace")
    text = file_to_parse.preprocessor(text)

    try:
        if isinstance(file_to_parse.parser, Parser):
            return file_to_parse.parser.parse(text)
        else:
            return file_to_parse.parser(text)

    except YAMLError as e:
        return DependencyParserError(
            file_to_parse.path.name, file_to_parse.parser_name, str(e)
        )
    except RecursionError:
        reason = "Python recursion depth exceeded, try again with SEMGREP_PYTHON_RECURSION_LIMIT_INCREASE set higher than 500"
        console.print(f"Failed to parse {file_to_parse.path} - {reason}")
        return DependencyParserError(
            str(file_to_parse.path), file_to_parse.parser_name, reason
        )
    except ParseError as e:
        # These are zero indexed but most editors are one indexed
        line, col = e.index.line, e.index.column
        line_prefix = f"{line + 1} | "
        text_lines = text.splitlines() + (
            ["<trailing newline>"] if text.endswith("\n") else []
        )  # Error on trailing newline shouldn't blow us up
        error_str = parse_error_to_str(e)
        location = (
            f"[bold]{file_to_parse.path}[/bold] at [bold]{line + 1}:{col + 1}[/bold]"
        )

        if line < len(text_lines):
            offending_line = text_lines[line]
            console.print(
                f"Failed to parse {location} - {error_str}\n"
                f"{line_prefix}{offending_line}\n"
                f"{' ' * (col + len(line_prefix))}^"
            )
            return DependencyParserError(
                str(file_to_parse.path),
                file_to_parse.parser_name,
                error_str,
                line + 1,
                col + 1,
                offending_line,
            )
        else:
            reason = f"{error_str}\nInternal Error - line {line + 1} is past the end of {file_to_parse.path}?"
            console.print(f"Failed to parse {location} - {reason}")
            return DependencyParserError(
                str(file_to_parse.path),
                file_to_parse.parser_name,
                reason,
                line + 1,
                col + 1,
            )
    except Exception as e:
        return DependencyParserError(
            file_to_parse.path.name, file_to_parse.parser_name, str(e)
        )


def safe_parse_lockfile_and_manifest(
    lockfile_to_parse: DependencyFileToParse[A],
    manifest_to_parse: DependencyFileToParse[B] | None,
) -> tuple[A | None, B | None, list[DependencyParserError]]:
    """
    Parse a lockfile and a manifest file, returning the results along with a list of errors that occurred in either parser
    """
    errors = []
    parsed_manifest = parse_dependency_file(manifest_to_parse)
    parsed_lockfile = parse_dependency_file(lockfile_to_parse)
    if isinstance(parsed_manifest, DependencyParserError):
        errors.append(parsed_manifest)
        parsed_manifest = None
    if isinstance(parsed_lockfile, DependencyParserError):
        errors.append(parsed_lockfile)
        parsed_lockfile = None
    return parsed_lockfile, parsed_manifest, errors


@dataclass(eq=False)
class ParsedDependency:
    """
    A dependency parsed from a lockfile. Used for freezing dependency information after
    parsing and children addition.
    """

    line_number: int
    transitivity: Transitivity
    children: list[DependencyChild]
    package: str
    version: str

    @staticmethod
    def from_dict(d: dict[str, Any]) -> ParsedDependency:
        return ParsedDependency(
            line_number=d["line_number"],
            transitivity=d["transitivity"],
            children=[child for child in d["children"]],
            package=d["package"],
            version=d["version"],
        )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, ParsedDependency):
            return NotImplemented
        return (
            self.package == other.package
            and self.version == other.version
            and self.transitivity == other.transitivity
        )

    def __hash__(self) -> int:
        return hash((self.package, self.version, self.transitivity))


# A parser for JSON, using a line_number annotated JSON type. This is adapted from an example in the Parsy repo.
# It is almost identical except for the addition of types, line number tracking, and some minor renaming
# https://github.com/python-parsy/parsy/blob/master/examples/json.py


@dataclass
class JSON:
    line_number: int
    value: None | bool | str | float | int | list[JSON] | dict[str, JSON]

    @staticmethod
    def make(
        marked: tuple[
            Pos,
            None | bool | str | float | int | list[JSON] | dict[str, JSON],
            Pos,
        ]
    ) -> JSON:
        return JSON(marked[0][0] + 1, marked[1])

    def as_dict(self) -> dict[str, JSON]:
        return cast(Dict[str, "JSON"], self.value)

    def as_str(self) -> str:
        return cast(str, self.value)

    def as_list(self) -> list[JSON]:
        return cast(List["JSON"], self.value)

    def as_int(self) -> int:
        return cast(int, self.value)


# Utilities
whitespace = regex(r"\s*")
new_lines = regex("\n+", re.MULTILINE)


def lexeme(p: Parser[A]) -> Parser[A]:
    return p << whitespace


# Punctuation
lbrace = lexeme(string("{"))
rbrace = lexeme(string("}"))
lparen = lexeme(string("("))
rparen = lexeme(string(")"))
lbrack = lexeme(string("["))
rbrack = lexeme(string("]"))
colon = lexeme(string(":"))
comma = lexeme(string(","))

# Primitives
true = lexeme(string("true")).result(True)
false = lexeme(string("false")).result(False)
null = lexeme(string("null")).result(None)
number = lexeme(regex(r"-?(0|[1-9][0-9]*)([.][0-9]+)?([eE][+-]?[0-9]+)?")).map(float)
string_part = regex(r'[^"\\]+')
string_esc = string("\\") >> (
    string("\\")
    | string("/")
    | string('"')
    | string("b").result("\b")
    | string("f").result("\f")
    | string("n").result("\n")
    | string("r").result("\r")
    | string("t").result("\t")
    | regex(r"u[0-9a-fA-F]{4}").map(lambda s: chr(int(s[1:], 16)))
)
quoted_str = lexeme(quoted((string_part | string_esc).many().concat()))

# Data structures
json_value: Parser[JSON] = fail("forward ref")
object_pair = pair((quoted_str << colon), json_value)
json_object = lbrace >> object_pair.sep_by(comma).map(lambda x: dict(x)) << rbrace
array = lbrack >> json_value.sep_by(comma) << rbrack
# Everything
become(
    json_value,
    alt(
        quoted_str.mark().map(JSON.make),
        number.mark().map(JSON.make),
        json_object.mark().map(JSON.make),
        array.mark().map(JSON.make),
        true.mark().map(JSON.make),
        false.mark().map(JSON.make),
        null.mark().map(JSON.make),
    ),
)
json_doc = whitespace >> json_value
