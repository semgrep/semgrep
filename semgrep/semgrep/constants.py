import os
import re
from enum import auto
from enum import Enum
from typing import Type

from semgrep import __VERSION__

RULES_KEY = "rules"
ID_KEY = "id"
CLI_RULE_ID = "-"
SEMGREP_URL = os.environ.get("SEMGREP_URL", "https://semgrep.dev/")
PLEASE_FILE_ISSUE_TEXT = "An error occurred while invoking the semgrep engine; please help us fix this by creating an issue at https://github.com/returntocorp/semgrep"

DEFAULT_SEMGREP_CONFIG_NAME = "semgrep"
DEFAULT_CONFIG_FILE = f".{DEFAULT_SEMGREP_CONFIG_NAME}.yml"
DEFAULT_CONFIG_FOLDER = f".{DEFAULT_SEMGREP_CONFIG_NAME}"

DEFAULT_TIMEOUT = 30  # seconds

USER_DATA_FOLDER = ".semgrep"
SETTINGS_FILE = "settings.yml"
SEMGREP_SETTINGS_FILE = os.environ.get("SEMGREP_SETTINGS_FILE")

SEMGREP_USER_AGENT = f"Semgrep/{__VERSION__}"
SEMGREP_USER_AGENT_APPEND = os.environ.get("SEMGREP_USER_AGENT_APPEND")
if SEMGREP_USER_AGENT_APPEND is not None:
    SEMGREP_USER_AGENT = f"{SEMGREP_USER_AGENT} {SEMGREP_USER_AGENT_APPEND}"

SEMGREP_CDN_BASE_URL = os.environ.get("SEMGREP_CDN_BASE_URL", "https://cdn.semgrep.dev")

YML_EXTENSIONS = {".yml", ".yaml"}
YML_SUFFIXES = [[ext] for ext in YML_EXTENSIONS]
YML_TEST_SUFFIXES = [[".test", ext] for ext in YML_EXTENSIONS]


class OutputFormat(Enum):
    TEXT = auto()
    JSON = auto()
    JUNIT_XML = auto()
    SARIF = auto()
    EMACS = auto()
    VIM = auto()

    def is_json(self) -> bool:
        return self in [OutputFormat.JSON, OutputFormat.SARIF]


# Ensure consistency with 'severity' in 'rule_schema.yaml'
class RuleSeverity(Enum):
    INFO = "INFO"
    WARNING = "WARNING"
    ERROR = "ERROR"
    INVENTORY = "INVENTORY"

    @classmethod
    def _missing_(cls: Type[Enum], value: object) -> Enum:
        if not isinstance(value, str):
            raise TypeError(f"invalid rule severity type: {type(value)}")
        for member in cls:
            if member.value.lower() == value:
                return member
        raise ValueError(f"invalid rule severity value: {value}")


RULE_ID_RE_STR = r"(?:[:=][\s]?(?P<ids>([^,\s](?:[,\s]+)?)+))?"

SEMGREP_OFF_RE = re.compile(r" semgrep-off" + RULE_ID_RE_STR, re.IGNORECASE)
SEMGREP_ON_RE = re.compile(r" semgrep-on" + RULE_ID_RE_STR, re.IGNORECASE)

# Inline 'noqa' implementation modified from flake8:
# https://github.com/PyCQA/flake8/blob/master/src/flake8/defaults.py
# We're looking for items that look like this:
# ' nosem'
# ' nosemgrep: example-pattern-id'
# ' nosem: pattern-id1,pattern-id2'
# ' NOSEMGREP:pattern-id1,pattern-id2'
#
# * We do not want to capture the ': ' that follows 'nosem'
# * We do not care about the casing of 'nosem'
# * We want a comma-separated list of ids
# * We want multi-language support, so we cannot strictly look for
#   Python comments that begin with '# '
# * nosem and nosemgrep should be interchangeable
#
NOSEM_INLINE_RE = re.compile(
    r" nosem(?:grep)?" + RULE_ID_RE_STR,
    re.IGNORECASE,
)
COMMA_SEPARATED_LIST_RE = re.compile(r"[,\s]")

MAX_LINES_FLAG_NAME = "--max-lines-per-finding"
DEFAULT_MAX_LINES_PER_FINDING = 10
BREAK_LINE_WIDTH = 80
BREAK_LINE_CHAR = "-"
BREAK_LINE = BREAK_LINE_CHAR * BREAK_LINE_WIDTH

MAX_CHARS_FLAG_NAME = "--max-chars-per-line"
DEFAULT_MAX_CHARS_PER_LINE = 160
ELLIPSIS_STRING = " ... "
DEFAULT_MAX_TARGET_SIZE = 1000000  # 1 MB
