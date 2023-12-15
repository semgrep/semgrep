from enum import auto
from enum import Enum
from typing import Any
from typing import Optional


class BetterEnum(Enum):
    """
    A base class for enums that provides some additional functionality.
    """

    # NOTE: Ideally we could just write `from typing import Self` above,
    # (see https://docs.python.org/3/whatsnew/3.11.html#whatsnew311-pep673)
    # but since we have to support Python 3.8, 3.9, and 3.10 for backwards compatibility,
    # we require the use of the `cast` helper function for callers to override the inferred
    # type of an expression. This is only for mypy -- there's no runtime check.
    # See https://mypy.readthedocs.io/en/stable/cheat_sheet_py3.html#when-you-re-puzzled-or-when-things-are-complicated

    @classmethod
    def _from(cls, raw_value: str, fallback: Optional[Any] = None) -> Any:
        # Ideally we would use the following signature (requires Python 3.11+):
        # (cls, raw_value: str, fallback: Optional[Self] = None) -> Self
        try:
            return cls.__getitem__(raw_value.upper())
        except KeyError:
            if fallback is not None:
                return fallback
            allowed_values = ", ".join([x.name for x in cls])
            raise ValueError(
                f"Invalid {cls.__name__} value: {raw_value}, allowed values are: {allowed_values}"
            )


class Confidence(BetterEnum):
    """
    The confidence levels for a given rule.

    This is an optional field within rule metadata and is expected to be one
    of the following values:
        - LOW
        - MEDIUM
        - HIGH

    See https://semgrep.dev/docs/contributing/contributing-to-semgrep-rules-repository/#confidence
    """

    UNKNOWN = auto()  # Missing confidence in metadata
    LOW = auto()
    MEDIUM = auto()
    HIGH = auto()


class Impact(BetterEnum):
    """
    Indicates how much damage can a vulnerability cause.

    This is an optional field within rule metadata and is expected to be one
    of the following values:
        - LOW
        - MEDIUM
        - HIGH

    See https://semgrep.dev/docs/contributing/contributing-to-semgrep-rules-repository/#impact
    """

    UNKNOWN = auto()  # Missing impact in metadata
    LOW = auto()
    MEDIUM = auto()
    HIGH = auto()


class Likelihood(BetterEnum):
    """
    Specifies how likely it is that an attacker can exploit the issue that has been found.

    This is an optional field within rule metadata and is expected to be one
    of the following values:
        - LOW
        - MEDIUM
        - HIGH

    See https://semgrep.dev/docs/contributing/contributing-to-semgrep-rules-repository/#likelihood
    """

    UNKNOWN = auto()  # Missing likelihood in metadata
    LOW = auto()
    MEDIUM = auto()
    HIGH = auto()
