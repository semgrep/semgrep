from enum import auto
from enum import Enum
from typing import Optional
from typing import Self


class BetterEnum(Enum):
    """
    A base class for enums that provides some additional functionality.
    """

    @classmethod
    def _from(cls, raw_value: str, fallback: Optional[Self] = None) -> Self:
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
