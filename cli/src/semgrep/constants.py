import re
from enum import auto
from enum import Enum
from typing import Type


RULES_KEY = "rules"
ID_KEY = "id"
CLI_RULE_ID = "-"
PLEASE_FILE_ISSUE_TEXT = "An error occurred while invoking the Semgrep engine. Please help us fix this by creating an issue at https://github.com/returntocorp/semgrep"

DEFAULT_SEMGREP_CONFIG_NAME = "semgrep"
DEFAULT_CONFIG_FILE = f".{DEFAULT_SEMGREP_CONFIG_NAME}.yml"
DEFAULT_CONFIG_FOLDER = f".{DEFAULT_SEMGREP_CONFIG_NAME}"
DEFAULT_SEMGREP_APP_CONFIG_URL = "api/agent/deployments/scans/config"

DEFAULT_TIMEOUT = 30  # seconds
DEFAULT_PRO_TIMEOUT_CI = 108000  # seconds
DEFAULT_MAX_MEMORY_PRO_CI = 5000  # MiB

SETTINGS_FILENAME = "settings.yml"

YML_EXTENSIONS = {".yml", ".yaml"}
YML_SUFFIXES = [[ext] for ext in YML_EXTENSIONS]
YML_TEST_SUFFIXES = [[".test", ext] for ext in YML_EXTENSIONS]
FIXTEST_SUFFIX = ".fixed"

RETURNTOCORP_LEVER_URL = "https://api.lever.co/v0/postings/returntocorp?mode=json"

UNSUPPORTED_EXT_IGNORE_LANGS = {"generic", "regex"}


class EngineType(Enum):
    OSS = auto()
    PRO = auto()
    INTERPROC = auto()
    INTERFILE = auto()

    @property
    def is_pro(self) -> bool:
        return self.value >= EngineType.PRO.value


class OutputFormat(Enum):
    TEXT = auto()
    JSON = auto()
    GITLAB_SAST = auto()
    GITLAB_SECRETS = auto()
    JUNIT_XML = auto()
    SARIF = auto()
    EMACS = auto()
    VIM = auto()

    def is_json(self) -> bool:
        return self in [OutputFormat.JSON, OutputFormat.SARIF]


# Ensure consistency with 'severity' in 'rule_schema_v1.yaml'
class RuleSeverity(Enum):
    INFO = "INFO"
    WARNING = "WARNING"
    ERROR = "ERROR"
    INVENTORY = "INVENTORY"
    EXPERIMENT = "EXPERIMENT"

    @classmethod
    def _missing_(cls: Type[Enum], value: object) -> Enum:
        if not isinstance(value, str):
            raise TypeError(f"invalid rule severity type: {type(value)}")
        for member in cls:
            if member.value.lower() == value:
                return member
        raise ValueError(f"invalid rule severity value: {value}")


RULE_ID_RE_STR = r"(?:[:=][\s]?(?P<ids>([^,\s](?:[,\s]+)?)+))?"

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
NOSEM_INLINE_RE_STR = r" nosem(?:grep)?" + RULE_ID_RE_STR
NOSEM_INLINE_RE = re.compile(NOSEM_INLINE_RE_STR, re.IGNORECASE)

# As a hack adapted from semgrep-agent,
# we assume comment markers are one of these special characters
NOSEM_INLINE_COMMENT_RE = re.compile(rf"[:#/]+{NOSEM_INLINE_RE_STR}$", re.IGNORECASE)

# A nosemgrep comment alone on its line.
# Since we don't know the comment syntax for the particular language, we
# assume it's enough that there isn't any word or number character before
# 'nosemgrep'.
# The following will not match:
#   hello(); // nosemgrep
#   + 42 // nosemgrep
# The following will match:
#   # nosemgrep
#   print('nosemgrep');
NOSEM_PREVIOUS_LINE_RE = re.compile(
    r"^[^a-zA-Z0-9]* nosem(?:grep)?" + RULE_ID_RE_STR,
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


class Colors(Enum):
    # these colors come from user's terminal theme
    foreground = 0
    white = 7
    black = 256
    cyan = "cyan"  # for filenames
    green = "green"  # for autofix
    yellow = "yellow"  # TODO: benchmark timing output?
    red = "red"  # for errors
    bright_blue = "bright_blue"  # TODO: line numbers?

    # these colors ignore user's terminal theme
    forced_black = 16  # #000
    forced_white = 231  # #FFF
