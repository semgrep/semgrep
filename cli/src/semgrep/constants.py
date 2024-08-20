import re
from enum import auto
from enum import Enum

import semgrep.semgrep_interfaces.semgrep_output_v1 as out

RULES_KEY = "rules"
MISSED_KEY = "missed"  # The number of Pro rules missed out on
ID_KEY = "id"
CLI_RULE_ID = "-"
PLEASE_FILE_ISSUE_TEXT = "An error occurred while invoking the Semgrep engine. Please help us fix this by creating an issue at https://github.com/semgrep/semgrep"

DEFAULT_SEMGREP_APP_CONFIG_URL = "api/agent/deployments/scans/config"

DEFAULT_TIMEOUT = (
    5  # seconds, coupling: keep up-to-date with Scan_CLI.ml and User_settings.ml
)
DEFAULT_PRO_TIMEOUT_CI = 10800  # seconds
DEFAULT_MAX_MEMORY_PRO_CI = 5000  # MiB

# The default depth has been configured as -1 in order to prevent unexpected
# behavior for current users of pro intra-file diff scanning. To enable pro
# inter-file diff scanning, users are required to manually specify the
# `-diff-depth` parameter with a value equal to or greater than 0.
# TODO update the default depth to a non-negative value, such as 2, once this
# new feature has reached a stable state.
DEFAULT_DIFF_DEPTH = -1

SETTINGS_FILENAME = "settings.yml"

YML_EXTENSIONS = {".yml", ".yaml"}
YML_SUFFIXES = [[ext] for ext in YML_EXTENSIONS]
YML_TEST_SUFFIXES = [[".test", ext] for ext in YML_EXTENSIONS]
FIXTEST_SUFFIX = ".fixed"

RETURNTOCORP_LEVER_URL = "https://api.lever.co/v0/postings/returntocorp?mode=json"

UNSUPPORTED_EXT_IGNORE_LANGS = {"generic", "regex"}


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


class RuleScanSource(Enum):
    unannotated = auto()
    unchanged = auto()
    new_version = auto()
    new_rule = auto()
    previous_scan = auto()


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

DEFAULT_MAX_LINES_PER_FINDING = 10
BREAK_LINE_WIDTH = 80
BREAK_LINE_CHAR = "-"
BREAK_LINE = BREAK_LINE_CHAR * BREAK_LINE_WIDTH

DEFAULT_MAX_CHARS_PER_LINE = 160
ELLIPSIS_STRING = " ... "

# Must be kept in sync w/ osemgrep
# coupling: src/targeting/Find_targets.ml default_conf.max_target_bytes
DEFAULT_MAX_TARGET_SIZE = 1000000  # 1 MB

# Number of entries (rules, targets) beyond we're not logging anymore
# coupling: with Output.ml
DEFAULT_MAX_LOG_LIST_ENTRIES = 100
TOO_MUCH_DATA = "<SKIPPED DATA (too many entries; use --max-log-list-entries)>"


class Colors(Enum):
    # these colors come from user's terminal theme
    foreground = 0
    white = 7
    black = 256
    cyan = "cyan"  # for filenames
    gray = "bright_black"  # for commands
    green = "green"  # for autofix
    yellow = "yellow"  # TODO: benchmark timing output?
    red = "red"  # for errors
    bright_blue = "bright_blue"  # TODO: line numbers?
    magenta = "magenta"
    # these colors ignore user's terminal theme
    forced_black = 16  # #000
    forced_white = 231  # #FFF


# Maps from product names used in our ATD files to product names
# used in as command line options that users are more familiar with.
USER_FRIENDLY_PRODUCT_NAMES = {
    out.Product(out.SAST()): "code",
    out.Product(out.SCA()): "supply-chain",
    out.Product(out.Secrets()): "secrets",
}
