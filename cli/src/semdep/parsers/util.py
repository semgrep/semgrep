from base64 import b16encode
from base64 import b64decode
from dataclasses import dataclass
from re import escape
from typing import Callable
from typing import cast
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple
from typing import TypeVar
from typing import Union

from parsy import alt
from parsy import fail
from parsy import line_info
from parsy import Parser
from parsy import regex
from parsy import string
from parsy import success

from semgrep.semgrep_interfaces.semgrep_output_v1 import Direct
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitive
from semgrep.semgrep_interfaces.semgrep_output_v1 import Transitivity
from semgrep.semgrep_interfaces.semgrep_output_v1 import Unknown


A = TypeVar("A")
B = TypeVar("B")

Pos = Tuple[int, int]


def not_any(chars: List[str]) -> "Parser[str]":
    return regex(f"[^{escape(''.join(chars))}]+")


def extract_npm_lockfile_hash(s: Optional[str]) -> Dict[str, List[str]]:
    """
    Go from:
        sha512-aePbxDmcYW++PaqBsJ+HYUFwCdv4LVvdnhBy78E57PIor8/OVvhMrADFFEDh8DHDFRv/O9i3lPhsENjO7QX0+A==
    To:
        sha512,
    """
    if s is None:
        return {}
    algorithm = s.split("-")[0]
    rest = s[len(algorithm) + 1 :]
    decode_base_64 = b64decode(rest)
    return {algorithm: [b16encode(decode_base_64).decode("ascii").lower()]}


# parsy line and column numbers are zero indexed, but editors are generally 1 indexed
# so we add one to account for this
line_number = line_info.map(lambda t: t[0] + 1)


def mark_line(p: "Parser[A]") -> "Parser[Tuple[int,A]]":
    return line_number.bind(lambda line: p.bind(lambda x: success((line, x))))


def any_str(strs: List[str]) -> "Parser[str]":
    return alt(*(string(s) for s in strs))


def pair(p1: "Parser[A]", p2: "Parser[B]") -> "Parser[Tuple[A,B]]":
    return p1.bind(lambda a: p2.bind(lambda b: success((a, b))))


def transitivity(manifest_deps: Optional[Set[A]], dep_sources: List[A]) -> Transitivity:
    if manifest_deps:
        for dep_source in dep_sources:
            if dep_source in manifest_deps:
                return Transitivity(Direct())
        return Transitivity(Transitive())
    else:
        return Transitivity(Unknown())


def become(p1: "Parser[A]", p2: "Parser[A]") -> None:
    """
    Take on the behavior of the given parser.
    """
    p1.__dict__ = p2.__dict__
    p1.__class__ = p2.__class__


def delay(p: Callable[[], "Parser[A]"]) -> "Parser[A]":
    return Parser(lambda x, y: p()(x, y))


def quoted(p: "Parser[A]") -> "Parser[A]":
    return string('"') >> p << string('"')


word = not_any([" "])
consume_word = word >> success(None)

line = not_any(["\n"])
consume_line = line >> success(None)


def upto(
    s: List[str], include_other: bool = False, consume_other: bool = False
) -> "Parser[str]":
    if include_other:
        return not_any(s).bind(
            lambda x: alt(*(string(x) for x in s)).map(lambda y: x + y)
        )
    elif consume_other:
        return not_any(s) << alt(*(string(x) for x in s))
    else:
        return not_any(s)


#### JSON Parser, adapted from parsy example ####


@dataclass
class JSON:
    line_number: int
    value: Union[None, bool, str, float, int, List["JSON"], Dict[str, "JSON"]]

    @staticmethod
    def make(
        marked: Tuple[
            Pos,
            Union[None, bool, str, float, int, List["JSON"], Dict[str, "JSON"]],
            Pos,
        ]
    ) -> "JSON":
        return JSON(marked[0][0] + 1, marked[1])

    def as_dict(self) -> Dict[str, "JSON"]:
        return cast(Dict[str, "JSON"], self.value)

    def as_str(self) -> str:
        return cast(str, self.value)

    def as_list(self) -> List["JSON"]:
        return cast(List["JSON"], self.value)


# Utilities
whitespace = regex(r"\s*")


def lexeme(p: "Parser[A]") -> "Parser[A]":
    return p << whitespace


# Punctuation
lbrace = lexeme(string("{"))
rbrace = lexeme(string("}"))
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
json_value: "Parser[JSON]" = fail("forward ref")
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
