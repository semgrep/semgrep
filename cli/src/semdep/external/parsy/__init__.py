from __future__ import annotations

import enum
import operator
import re
from dataclasses import dataclass
from functools import wraps
from typing import Any, Callable, FrozenSet

from .version import __version__  # noqa: F401

noop = lambda x: x


class ParseError(RuntimeError):
    def __init__(self, expected, stream, index):
        self.expected = expected
        self.stream = stream
        self.index = index

    def line_info(self):
        if isinstance(self.stream, str):
            return f"{self.index.line}:{self.index.column}"
        else:
            return str(self.index.offset)

    def __str__(self):
        expected_list = sorted(repr(e) for e in self.expected)

        if len(expected_list) == 1:
            return f"expected {expected_list[0]} at {self.line_info()}"
        else:
            return f"expected one of {', '.join(expected_list)} at {self.line_info()}"


@dataclass(frozen=True)
class Position:
    offset: int
    line: int
    column: int


@dataclass(frozen=True)
class Result:
    status: bool
    index: Position
    value: Any
    furthest: Position
    expected: FrozenSet[str]

    @staticmethod
    def success(index, value):
        return Result(True, index, value, Position(-1, -1, -1), frozenset())

    @staticmethod
    def failure(index, expected):
        return Result(False, Position(-1, -1, -1), None, index, frozenset([expected]))

    # collect the furthest failure from self and other
    def aggregate(self, other):
        if not other:
            return self

        if self.furthest.offset > other.furthest.offset:
            return self
        elif self.furthest.offset == other.furthest.offset:
            # if we both have the same failure index, we combine the expected messages.
            return Result(
                self.status,
                self.index,
                self.value,
                self.furthest,
                self.expected | other.expected,
            )
        else:
            return Result(
                self.status, self.index, self.value, other.furthest, other.expected
            )


class Parser:
    """
    A Parser is an object that wraps a function whose arguments are
    a string to be parsed and the index on which to begin parsing.
    The function should return either Result.success(next_index, value),
    where the next index is where to continue the parse and the value is
    the yielded value, or Result.failure(index, expected), where expected
    is a string indicating what was expected, and the index is the index
    of the failure.
    """

    def __init__(self, wrapped_fn: Callable[[str | bytes | list, Position], Result]):
        """
        Creates a new Parser from a function that takes a stream
        and returns a Result.
        """
        self.wrapped_fn = wrapped_fn

    def __call__(self, stream: str | bytes | list, index: Position):
        return self.wrapped_fn(stream, index)

    def parse(self, stream: str | bytes | list) -> Any:
        """Parses a string or list of tokens and returns the result or raise a ParseError."""
        (result, _) = (self << eof).parse_partial(stream)
        return result

    def parse_partial(
        self, stream: str | bytes | list
    ) -> tuple[Any, str | bytes | list]:
        """
        Parses the longest possible prefix of a given string.
        Returns a tuple of the result and the unparsed remainder,
        or raises ParseError
        """
        result = self(
            stream,
            Position(0, 0, 0) if isinstance(stream, str) else Position(0, -1, -1),
        )

        if result.status:
            return (result.value, stream[result.index.offset :])
        else:
            raise ParseError(result.expected, stream, result.furthest)

    def bind(self, bind_fn):
        @Parser
        def bound_parser(stream, index):
            result = self(stream, index)

            if result.status:
                next_parser = bind_fn(result.value)
                return next_parser(stream, result.index).aggregate(result)
            else:
                return result

        return bound_parser

    def map(self, map_function: Callable) -> Parser:
        """
        Returns a parser that transforms the produced value of the initial parser with map_function.
        """
        return self.bind(lambda res: success(map_function(res)))

    def combine(self, combine_fn: Callable) -> Parser:
        """
        Returns a parser that transforms the produced values of the initial parser
        with ``combine_fn``, passing the arguments using ``*args`` syntax.

        The initial parser should return a list/sequence of parse results.
        """
        return self.bind(lambda res: success(combine_fn(*res)))

    def combine_dict(self, combine_fn: Callable) -> Parser:
        """
        Returns a parser that transforms the value produced by the initial parser
        using the supplied function/callable, passing the arguments using the
        ``**kwargs`` syntax.

        The value produced by the initial parser must be a mapping/dictionary from
        names to values, or a list of two-tuples, or something else that can be
        passed to the ``dict`` constructor.

        If ``None`` is present as a key in the dictionary it will be removed
        before passing to ``fn``, as will all keys starting with ``_``.
        """
        return self.bind(
            lambda res: success(
                combine_fn(
                    **{
                        k: v
                        for k, v in dict(res).items()
                        if k is not None
                        and not (isinstance(k, str) and k.startswith("_"))
                    }
                )
            )
        )

    def concat(self) -> Parser:
        """
        Returns a parser that concatenates together (as a string) the previously
        produced values.
        """
        return self.map("".join)

    def then(self, other: Parser) -> Parser:
        """
        Returns a parser which, if the initial parser succeeds, will
        continue parsing with ``other``. This will produce the
        value produced by ``other``.

        """
        return seq(self, other).combine(lambda left, right: right)

    def skip(self, other: Parser) -> Parser:
        """
        Returns a parser which, if the initial parser succeeds, will
        continue parsing with ``other``. It will produce the
        value produced by the initial parser.
        """
        return seq(self, other).combine(lambda left, right: left)

    def result(self, value: Any) -> Parser:
        """
        Returns a parser that, if the initial parser succeeds, always produces
        the passed in ``value``.
        """
        return self >> success(value)

    def many(self) -> Parser:
        """
        Returns a parser that expects the initial parser 0 or more times, and
        produces a list of the results.
        """
        return self.times(0, float("inf"))

    def times(self, min: int, max: int = None) -> Parser:
        """
        Returns a parser that expects the initial parser at least ``min`` times,
        and at most ``max`` times, and produces a list of the results. If only one
        argument is given, the parser is expected exactly that number of times.
        """
        if max is None:
            max = min

        @Parser
        def times_parser(stream, index):
            values = []
            times = 0
            result = None

            while times < max:
                result = self(stream, index).aggregate(result)
                if result.status:
                    values.append(result.value)
                    index = result.index
                    times += 1
                elif times >= min:
                    break
                else:
                    return result

            return Result.success(index, values).aggregate(result)

        return times_parser

    def at_most(self, n: int) -> Parser:
        """
        Returns a parser that expects the initial parser at most ``n`` times, and
        produces a list of the results.
        """
        return self.times(0, n)

    def at_least(self, n: int) -> Parser:
        """
        Returns a parser that expects the initial parser at least ``n`` times, and
        produces a list of the results.
        """
        return self.times(n) + self.many()

    def optional(self, default: Any = None) -> Parser:
        """
        Returns a parser that expects the initial parser zero or once, and maps
        the result to a given default value in the case of no match. If no default
        value is given, ``None`` is used.
        """
        return self.times(0, 1).map(lambda v: v[0] if v else default)

    def until(
        self,
        other: Parser,
        min: int = 0,
        max: int = float("inf"),
        consume_other: bool = False,
    ) -> Parser:
        """
        Returns a parser that expects the initial parser followed by ``other``.
        The initial parser is expected at least ``min`` times and at most ``max`` times.
        By default, it does not consume ``other`` and it produces a list of the
        results excluding ``other``. If ``consume_other`` is ``True`` then
        ``other`` is consumed and its result is included in the list of results.
        """

        @Parser
        def until_parser(stream, index):
            values = []
            times = 0
            while True:
                # try parser first
                res = other(stream, index)
                if res.status and times >= min:
                    if consume_other:
                        # consume other
                        values.append(res.value)
                        index = res.index
                    return Result.success(index, values)

                # exceeded max?
                if times >= max:
                    # return failure, it matched parser more than max times
                    return Result.failure(index, f"at most {max} items")

                # failed, try parser
                result = self(stream, index)
                if result.status:
                    # consume
                    values.append(result.value)
                    index = result.index
                    times += 1
                elif times >= min:
                    # return failure, parser is not followed by other
                    return Result.failure(index, "did not find other parser")
                else:
                    # return failure, it did not match parser at least min times
                    return Result.failure(
                        index, f"at least {min} items; got {times} item(s)"
                    )

        return until_parser

    def sep_by(self, sep: Parser, *, min: int = 0, max: int = float("inf")) -> Parser:
        """
        Returns a new parser that repeats the initial parser and
        collects the results in a list. Between each item, the ``sep`` parser
        is run (and its return value is discarded). By default it
        repeats with no limit, but minimum and maximum values can be supplied.
        """
        zero_times = success([])
        if max == 0:
            return zero_times
        res = self.times(1) + (sep >> self).times(min - 1, max - 1)
        if min == 0:
            res |= zero_times
        return res

    def desc(self, description: str) -> Parser:
        """
        Returns a new parser with a description added, which is used in the error message
        if parsing fails.
        """

        @Parser
        def desc_parser(stream, index):
            result = self(stream, index)
            if result.status:
                return result
            else:
                return Result.failure(index, description)

        return desc_parser

    def mark(self) -> Parser:
        """
        Returns a parser that wraps the initial parser's result in a value
        containing column and line information of the match, as well as the
        original value. The new value is a 3-tuple:

        ((start_row, start_column),
         original_value,
         (end_row, end_column))
        """

        @generate
        def marked():
            start = yield line_info
            body = yield self
            end = yield line_info
            return (start, body, end)

        return marked

    def tag(self, name: str) -> Parser:
        """
        Returns a parser that wraps the produced value of the initial parser in a
        2 tuple containing ``(name, value)``. This provides a very simple way to
        label parsed components
        """
        return self.map(lambda v: (name, v))

    def should_fail(self, description: str) -> Parser:
        """
        Returns a parser that fails when the initial parser succeeds, and succeeds
        when the initial parser fails (consuming no input). A description must
        be passed which is used in parse failure messages.

        This is essentially a negative lookahead
        """

        @Parser
        def fail_parser(stream, index):
            res = self(stream, index)
            if res.status:
                return Result.failure(index, description)
            return Result.success(index, res)

        return fail_parser

    def __add__(self, other: Parser) -> Parser:
        return seq(self, other).combine(operator.add)

    def __mul__(self, other: Parser) -> Parser:
        if isinstance(other, range):
            return self.times(other.start, other.stop - 1)
        return self.times(other)

    def __or__(self, other: Parser) -> Parser:
        return alt(self, other)

    # haskelley operators, for fun #

    # >>
    def __rshift__(self, other: Parser) -> Parser:
        return self.then(other)

    # <<
    def __lshift__(self, other: Parser) -> Parser:
        return self.skip(other)


def alt(*parsers: Parser) -> Parser:
    """
    Creates a parser from the passed in argument list of alternative
    parsers, which are tried in order, moving to the next one if the
    current one fails.
    """
    if not parsers:
        return fail("<empty alt>")

    @Parser
    def alt_parser(stream, index):
        result = None
        for parser in parsers:
            result = parser(stream, index).aggregate(result)
            if result.status:
                return result

        return result

    return alt_parser


def seq(*parsers: Parser, **kw_parsers: Parser) -> Parser:
    """
    Takes a list of parsers, runs them in order,
    and collects their individuals results in a list,
    or in a dictionary if you pass them as keyword arguments.
    """
    if not parsers and not kw_parsers:
        return success([])

    if parsers and kw_parsers:
        raise ValueError(
            "Use either positional arguments or keyword arguments with seq, not both"
        )

    if parsers:

        @Parser
        def seq_parser(stream, index):
            result = None
            values = []
            for parser in parsers:
                result = parser(stream, index).aggregate(result)
                if not result.status:
                    return result
                index = result.index
                values.append(result.value)
            return Result.success(index, values).aggregate(result)

        return seq_parser
    else:

        @Parser
        def seq_kwarg_parser(stream, index):
            result = None
            values = {}
            for name, parser in kw_parsers.items():
                result = parser(stream, index).aggregate(result)
                if not result.status:
                    return result
                index = result.index
                values[name] = result.value
            return Result.success(index, values).aggregate(result)

        return seq_kwarg_parser


def generate(fn) -> Parser:
    """
    Creates a parser from a generator function
    """
    if isinstance(fn, str):
        return lambda f: generate(f).desc(fn)

    @Parser
    @wraps(fn)
    def generated(stream, index):
        # start up the generator
        iterator = fn()

        result = None
        value = None
        try:
            while True:
                next_parser = iterator.send(value)
                result = next_parser(stream, index).aggregate(result)
                if not result.status:
                    return result
                value = result.value
                index = result.index
        except StopIteration as stop:
            returnVal = stop.value
            if isinstance(returnVal, Parser):
                return returnVal(stream, index).aggregate(result)

            return Result.success(index, returnVal).aggregate(result)

    return generated


index = Parser(lambda _, index: Result.success(index, index.offset))
line_info = Parser(lambda _, index: Result.success(index, (index.line, index.column)))


def success(value: Any) -> Parser:
    """
    Returns a parser that does not consume any of the stream, but
    produces ``value``.
    """
    return Parser(lambda _, index: Result.success(index, value))


def fail(expected: str) -> Parser:
    """
    Returns a parser that always fails with the provided error message.
    """
    return Parser(lambda _, index: Result.failure(index, expected))


def make_index_update(consumed: str) -> Callable[[Position], Position]:
    slen = len(consumed)
    line_count = consumed.count("\n")
    last_nl = consumed.rfind("\n")
    return lambda index: Position(
        offset=index.offset + slen,
        line=index.line + line_count,
        column=slen - (last_nl + 1) if last_nl >= 0 else index.column + slen,
    )


def string(expected_string: str, transform: Callable[[str], str] = noop) -> Parser:
    """
    Returns a parser that expects the ``expected_string`` and produces
    that string value.

    Optionally, a transform function can be passed, which will be used on both
    the expected string and tested string.
    """

    slen = len(expected_string)
    transformed_s = transform(expected_string)
    index_update = make_index_update(expected_string)

    @Parser
    def string_parser(stream, index):
        if transform(stream[index.offset : index.offset + slen]) == transformed_s:
            return Result.success(index_update(index), expected_string)
        else:
            return Result.failure(index, expected_string)

    return string_parser


def regex(exp: str, flags=0, group: int | str | tuple = 0) -> Parser:
    """
    Returns a parser that expects the given ``exp``, and produces the
    matched string. ``exp`` can be a compiled regular expression, or a
    string which will be compiled with the given ``flags``.

    Optionally, accepts ``group``, which is passed to re.Match.group
    https://docs.python.org/3/library/re.html#re.Match.group> to
    return the text from a capturing group in the regex instead of the
    entire match.
    """

    if isinstance(exp, (str, bytes)):
        exp = re.compile(exp, flags)
    if isinstance(group, (str, int)):
        group = (group,)

    @Parser
    def regex_parser(stream, index):
        match = exp.match(stream, index.offset)
        if match:
            index = (
                make_index_update(stream[match.start() : match.end()])(index)
                if isinstance(stream, str)
                else Position(match.end(), -1, -1)
            )
            return Result.success(index, match.group(*group))
        else:
            return Result.failure(index, exp.pattern)

    return regex_parser


def test_item(func: Callable[..., bool], description: str) -> Parser:
    """
    Returns a parser that tests a single item from the list of items being
    consumed, using the callable ``func``. If ``func`` returns ``True``, the
    parse succeeds, otherwise the parse fails with the description
    ``description``.
    """

    @Parser
    def test_item_parser(stream, index):
        if index.offset < len(stream):
            if isinstance(stream, bytes):
                # Subscripting bytes with `[index]` instead of
                # `[index:index + 1]` returns an int
                item = stream[index.offset : index.offset + 1]
            else:
                item = stream[index.offset]
            if func(item):
                if isinstance(stream, str):
                    index = make_index_update(item)(index)
                else:
                    index = Position(index.offset + 1, index.line, index.column)
                return Result.success(index, item)
        return Result.failure(index, description)

    return test_item_parser


def test_char(func: Callable[..., bool], description: str) -> Parser:
    """
    Returns a parser that tests a single character with the callable
    ``func``. If ``func`` returns ``True``, the parse succeeds, otherwise
    the parse fails with the description ``description``.
    """
    # Implementation is identical to test_item
    return test_item(func, description)


def match_item(item: Any, description: str = None) -> Parser:
    """
    Returns a parser that tests the next item (or character) from the stream (or
    string) for equality against the provided item. Optionally a string
    description can be passed.
    """

    if description is None:
        description = str(item)
    return test_item(lambda i: item == i, description)


def string_from(*strings: str, transform: Callable[[str], str] = noop):
    """
    Accepts a sequence of strings as positional arguments, and returns a parser
    that matches and returns one string from the list. The list is first sorted
    in descending length order, so that overlapping strings are handled correctly
    by checking the longest one first.
    """
    # Sort longest first, so that overlapping options work correctly
    return alt(*(string(s, transform) for s in sorted(strings, key=len, reverse=True)))


def char_from(string: str | bytes):
    """
    Accepts a string and returns a parser that matches and returns one character
    from the string.
    """
    if isinstance(string, bytes):
        return test_char(lambda c: c in string, b"[" + string + b"]")
    else:
        return test_char(lambda c: c in string, "[" + string + "]")


def peek(parser: Parser) -> Parser:
    """
    Returns a lookahead parser that parses the input stream without consuming
    chars.
    """

    @Parser
    def peek_parser(stream, index):
        result = parser(stream, index)
        if result.status:
            return Result.success(index, result.value)
        else:
            return result

    return peek_parser


any_char = test_char(lambda c: True, "any character")

whitespace = regex(r"\s+")

letter = test_char(lambda c: c.isalpha(), "a letter")

digit = test_char(lambda c: c.isdigit(), "a digit")

decimal_digit = char_from("0123456789")


@Parser
def eof(stream, index):
    """
    A parser that only succeeds if the end of the stream has been reached.
    """

    if index.offset >= len(stream):
        return Result.success(index, None)
    else:
        return Result.failure(index, "EOF")


def from_enum(enum_cls: type[enum.Enum], transform=noop) -> Parser:
    """
    Given a class that is an enum.Enum class
    https://docs.python.org/3/library/enum.html , returns a parser that
    will parse the values (or the string representations of the values)
    and return the corresponding enum item.
    """

    items = sorted(
        ((str(enum_item.value), enum_item) for enum_item in enum_cls),
        key=lambda t: len(t[0]),
        reverse=True,
    )
    return alt(
        *(
            string(value, transform=transform).result(enum_item)
            for value, enum_item in items
        )
    )


class forward_declaration(Parser):
    """
    An empty parser that can be used as a forward declaration,
    especially for parsers that need to be defined recursively.

    You must use `.become(parser)` before using.
    """

    def __init__(self):
        pass

    def _raise_error(self, *args, **kwargs):
        raise ValueError(
            "You must use 'become' before attempting to call `parse` or `parse_partial`"
        )

    parse = _raise_error
    parse_partial = _raise_error

    def become(self, other: Parser):
        """
        Take on the behavior of the given parser.
        """
        self.__dict__ = other.__dict__
        self.__class__ = other.__class__
