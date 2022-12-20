from base64 import b16encode
from base64 import b64decode
from dataclasses import dataclass
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
    return regex(f"[^{''.join(chars)}]+")


def extract_npm_lockfile_hash(s: str) -> Dict[str, List[str]]:
    """
    Go from:
        sha512-aePbxDmcYW++PaqBsJ+HYUFwCdv4LVvdnhBy78E57PIor8/OVvhMrADFFEDh8DHDFRv/O9i3lPhsENjO7QX0+A==
    To:
        sha512,
    """
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


def transitivity(manifest_deps: Optional[Set[A]], dep_source: A) -> Transitivity:
    if manifest_deps:
        return Transitivity(Direct() if dep_source in manifest_deps else Transitive())
    else:
        return Transitivity(Unknown())


def become(p1: "Parser[A]", p2: "Parser[A]") -> None:
    """
    Take on the behavior of the given parser.
    """
    p1.__dict__ = p2.__dict__
    p1.__class__ = p2.__class__


#### JSON Parser, adapted from parsy example ####


@dataclass
class JSON:
    pos: Pos
    value: Union[None, bool, str, float, int, List["JSON"], Dict[str, "JSON"]]

    @staticmethod
    def make(
        marked: Tuple[
            Pos,
            Union[None, bool, str, float, int, List["JSON"], Dict[str, "JSON"]],
            Pos,
        ]
    ) -> "JSON":
        return JSON(marked[0], marked[1])

    def __getitem__(self, key: str) -> "JSON":
        if isinstance(self.value, dict):
            return self.value[key]
        else:
            return self


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
quoted = lexeme(
    string('"') >> (string_part | string_esc).many().concat() << string('"')
)

# Data structures
json_value: "Parser[JSON]" = fail("forward ref")
object_pair = pair((quoted << colon), json_value)
json_object = lbrace >> object_pair.sep_by(comma).map(lambda x: dict(x)) << rbrace
array = lbrack >> json_value.sep_by(comma) << rbrack
# Everything
become(
    json_value,
    alt(
        quoted.mark().map(JSON.make),
        number.mark().map(JSON.make),
        json_object.mark().map(JSON.make),
        array.mark().map(JSON.make),
        true.mark().map(JSON.make),
        false.mark().map(JSON.make),
        null.mark().map(JSON.make),
    ),
)
json_doc = whitespace >> json_value
