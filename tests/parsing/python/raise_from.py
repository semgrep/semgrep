# PEP 3134 exception chaining via `raise X from Y` (Python 3.0).


def basic():
    try:
        1 / 0
    except ZeroDivisionError as e:
        raise ValueError("wrapped") from e


def from_none():
    # `raise ... from None` suppresses implicit context.
    try:
        1 / 0
    except ZeroDivisionError:
        raise ValueError("hidden") from None


def from_expression():
    try:
        1 / 0
    except ZeroDivisionError as e:
        raise RuntimeError(str(e)) from e.__cause__


def re_raise():
    try:
        1 / 0
    except ZeroDivisionError:
        # Bare re-raise still parses normally.
        raise
