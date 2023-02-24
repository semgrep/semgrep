"""
Type stubs for the Parsy parser combinator library
"""

from typing import TypeVar
from typing import Generic
from typing import Callable
from typing import Iterable
from typing import Tuple
from typing import List
from typing import Optional
from typing import Protocol
from typing import overload
from enum import Enum

from re import Pattern

# The variance annotation is needed to appease mypy. I don't fully understand why honestly
T = TypeVar("T", covariant=True)
A = TypeVar("A")
B = TypeVar("B")

# Same deal
I = TypeVar("I", contravariant=True)
O = TypeVar("O", covariant=True)

class Combiner(Protocol, Generic[I, O]):
    """
    Used to describe the type of a function which takes a *args list of Is and produces an O
    """

    def __call__(self, *args: I) -> O: ...

class SupportsAdd(Protocol[A]):
    """
    Used to descibe the type of an object that has an __add__ method
    """

    def __add__(self: A, x: A) -> A: ...

Addable = TypeVar("Addable", bound=SupportsAdd)

Pos = Tuple[int, int]

class Position:
    offset: int
    line: int
    column: int

class ParseError(RuntimeError):
    stream: str
    index: Position
    expected: List[str]

class Result(Generic[T]):
    """
    What a parser actually returns. This is internal and shouldn't be touched by users
    """

    ...

class Parser(Generic[T]):
    """
    A Parser[A] is a parser that consumes some portion of a string and produces a value of type A.
    """

    def __init__(self: Parser[T], fn: Callable[[str, int], Result[T]]): ...
    def __call__(self: Parser[A], steram: str, index: int) -> Result[A]: ...
    def parse(self: Parser[A], stream: str) -> A:
        """
        Run the parser on [stream], raise a ParseError if the entire string is not consumed
        """
        ...
    def parse_partial(self: Parser[A], stream: str) -> Tuple[A, str]:
        """
        Run the parser on [stream], return a pair of the result, and whatever part of the string was not consumed by the parser
        """
        ...
    def bind(self: Parser[A], bind_fn: Callable[[A], Parser[B]]) -> Parser[B]:
        """
        Run [self], call [bind_fn] on the result to produce a new parser (whose behavior can depend on this result)
        then run that parser
        """
        ...
    def map(self: Parser[A], map_fn: Callable[[A], B]) -> Parser[B]:
        """
        Run [self] and call [map_fn] on the result
        """
        ...
    def combine(self: Parser[Iterable[A]], combine_fn: Combiner[A, B]) -> Parser[B]:
        """
        Run [self], producing [result], then run [map_fn(*result)]
        """
        ...
    def concat(self: Parser[Iterable[str]]) -> Parser[str]:
        """
        Run [self], then concatenate the results
        """
        ...
    def then(self: Parser[A], other: Parser[B]) -> Parser[B]:
        """
        Run [self] then run [other] and produce the result of [other]
        """
        ...
    def skip(self: Parser[A], other: Parser[B]) -> Parser[A]:
        """
        Run [self] then run [other] and produce the result of [self]
        """
        ...
    def result(self: Parser[A], value: B) -> Parser[B]:
        """
        Run [self], then produce [value]
        """
        ...
    def many(self: Parser[A]) -> Parser[List[A]]:
        """
        Run [self] zero or more times, collect the results in a list
        """
        ...
    def times(
        self: Parser[A], min: int, max: Optional[int | float] = None
    ) -> Parser[List[A]]:
        """
        Run [self] at least [min] times and at most [max] times.
        If [max] is None then [max] is set to [min], so it runs exactly [min] times
        """
        ...
    def at_most(self: Parser[A], n: int) -> Parser[List[A]]:
        """
        Run [self] at most [n] times
        """
        ...
    def at_least(self: Parser[A], n: int) -> Parser[List[A]]:
        """
        Run [self] at least [n] times
        """
        ...
    @overload
    def optional(self: Parser[A], default: B) -> Parser[A | B]: ...
    @overload
    def optional(self: Parser[A], default: None = None) -> Parser[A | None]:
        """
        Run [self], if it succeeds return the result, if it fails return [default]
        """
        ...
    def until(
        self: Parser[A],
        other: Parser[A],
        min: int = 0,
        max: int | float = float("inf"),
        consume_other: bool = False,
    ) -> Parser[List[A]]:
        """
        Run [self] until [other] succeeds, [self] is run at least [min] times and at most [max] times.
        If [consume_other] then the result of [other] is added to the list of results
        """
        ...
    def sep_by(
        self: Parser[A],
        sep: Parser[B],
        *,
        min: int = 0,
        max: int | float = float("inf")
    ) -> Parser[List[A]]:
        """
        Run [self] zero or more times, separated by [sep]. [self] is run at least [min] times and at most [max] times
        """
        ...
    def desc(self: Parser[A], description: str) -> Parser[A]:
        """
        Add a description to [self] for better error messages
        """
        ...
    def mark(self: Parser[A]) -> Parser[Tuple[Pos, A, Pos]]:
        """
        Run [self] but include the line+col number before and after running it
        """
        ...
    def tag(self: Parser[A], name: str) -> Parser[Tuple[str, A]]:
        """
        Run [self] and return a pair of [name] and the result
        """
        ...
    def should_fail(self: Parser[A], description: str) -> Parser[None]:
        """
        Run [self] and succeed with [None] if it fails
        """
        ...
    def __add__(self: Parser[Addable], other: Parser[Addable]) -> Parser[Addable]:
        """
        Run [self] and then [other], then run [+] on the results. The parsers must
        produce an object with the __add__ method.
        """
        ...
    def __mul__(self: Parser[A], other: int | range) -> Parser[List[A]]:
        """
        Run [self] [other] times if [other] is an [int], and once for each element in the range if it is a [range]
        """
        ...
    def __or__(self: Parser[A], other: Parser[B]) -> Parser[A | B]:
        """
        Run [self], if it succeeds return the result. If it fails run [other] and return the result.
        [foo | bar] parses "foo or bar"
        """
        ...
    def __rshift__(self: Parser[A], other: Parser[B]) -> Parser[B]:
        """
        Nice syntactic sugar for [self.then(other)]
        [foo >> bar] == [foo.then(bar)]
        """
        ...
    def __lshift__(self: Parser[A], other: Parser[B]) -> Parser[A]:
        """
        Nice syntactic sugar for [self.skip(other)]
        [foo << bar] == [foo.skip(bar)]
        """
        ...

def alt(*parsers: Parser[A]) -> Parser[A]:
    """
    Return the result of the first parser in the argument list that succeeds.
    This will be the only parser that actually consumes some of the stream
    This is multi-argument __or__
    [alt(foo,bar,baz)] "foo or bar or baz"
    """
    ...

def seq(*parsers: Parser[A]) -> Parser[List[A]]:
    """
    Run each parser in sequence, all of them consuming the stream
    [seq(foo,bar,baz)] is "foo and bar and baz"
    """
    ...

def success(value: A) -> Parser[A]:
    """
    Returns a parser that always succeeds, consumes no input, and produces [value]
    """
    ...

def fail(expected: str) -> Parser[A]:
    """
    Returns a parser that always fails, claiming it wanted [expected]
    """
    ...

def string(
    expected_string: str, transform: Callable[[str], str] = lambda x: x
) -> Parser[str]:
    """
    Returns a parser that expects [expected_string], and optionally calls [transform] on the result
    """
    ...

@overload
def regex(
    exp: str | bytes | Pattern, flags: int = 0, group: int | str = 0
) -> Parser[str]: ...
@overload
def regex(
    exp: str | bytes | Pattern, flags: int, group: tuple[int, ...] | tuple[str, ...]
) -> Parser[tuple[str, ...]]:
    """
    Returns a parser which expects a string that matches the regex [exp]
    [flags] are regex flags
    [group] can cause the function to return an n-tuple separated into capture groups
    """
    ...

def test_char(func: Callable[[str], bool], description: str) -> Parser[str]:
    """
    Returns a parser that succeeds on a char for which [func(char)] returns True
    """
    ...

def match_char(item: str, description: Optional[str] = None) -> Parser[str]:
    """
    Returns a parser that success on the char [item]
    """
    ...

def string_from(
    *strings: str, transform: Callable[[str], str] = lambda x: x
) -> Parser[str]:
    """
    Returns a parser that succeeds on any of the strings in [strings], optionally
    calling [transform] on the result
    """
    ...

def char_from(string: str) -> Parser[str]:
    """
    Returns a parser which succeeds on any single character that is in [string]
    """
    ...

def peek(parser: Parser[A]) -> Parser[A]:
    """
    Returns a parser which runs [parser] and succeeds if it succeeds, but consumes no input
    """
    ...

any_char: Parser[str]
whitespace: Parser[str]
letter: Parser[str]
digit: Parser[str]
decimal_digit: Parser[str]
eof: Parser[None]
line_info: Parser[Pos]
index: Parser[int]

E = TypeVar("E", bound=Enum)

def from_enum(
    enum_cls: type[E], transform: Callable[[str], str] = lambda x: x
) -> Parser[E]:
    """
    Returns a parser for elements of the enum [enum_cls], via some reflection magic.
    Don't tell the PA team I said this but dynamic types can be cool
    """
    ...

def line_info_at(stream: str, index: int) -> Tuple[int, int]:
    """
    Returns the line and column number of [index] within [stream]
    Used for producing some nice error messages
    """
    ...
