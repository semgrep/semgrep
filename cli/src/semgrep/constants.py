"""Constant config values and data."""
import re
from enum import auto
from enum import Enum
from typing import List
from typing import Optional
from typing import Type

from typing_extensions import TypedDict

RULES_KEY = "rules"
ID_KEY = "id"
CLI_RULE_ID = "-"
PLEASE_FILE_ISSUE_TEXT = "An error occurred while invoking the Semgrep engine. Please help us fix this by creating an issue at https://github.com/returntocorp/semgrep"

DEFAULT_SEMGREP_CONFIG_NAME = "semgrep"
DEFAULT_CONFIG_FILE = f".{DEFAULT_SEMGREP_CONFIG_NAME}.yml"
DEFAULT_CONFIG_FOLDER = f".{DEFAULT_SEMGREP_CONFIG_NAME}"
DEFAULT_SEMGREP_APP_CONFIG_URL = "api/agent/deployments/scans/config"

DEFAULT_TIMEOUT = 30  # seconds
DEFAULT_PRO_TIMEOUT_CI = 10800  # seconds
DEFAULT_MAX_MEMORY_PRO_CI = 5000  # MiB

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


class PackageManagerInfo(TypedDict):
    language: str
    package_manager: str
    manifest_path: str
    lockfile_path: Optional[str]


# ChatGPT prompt to generate:

# --
# Please give me the top 20 package managers and their lockfiles' names in a JSON array format with the following keys: "language", "package_manager", "lockfile_path".

# Example entry:

# {"language": "python", "package_manager": "Poetry", "lockfile_path": "poetry.lock"}

# Add a field for "manifest_path". This points to the file where you specify dependencies without locking to a specific version. "manifest_path" can be null. "lockfile_path" can also be null.
# --

# Manually converted to Python format and removed supported or weird entries

# fmt: off
UNSUPPORTED_PACKAGE_MANAGERS: List[PackageManagerInfo] = [
    {"language": "c", "package_manager": "Conan", "manifest_path": "conanfile.txt", "lockfile_path": "conan.lock"},
    {"language": "c", "package_manager": "pkg-config", "manifest_path": ".pc", "lockfile_path": None},
    {"language": "c", "package_manager": "Vcpkg", "manifest_path": "vcpkg.json", "lockfile_path": None},
    {"language": "clojure", "package_manager": "Boot", "manifest_path": "build.boot", "lockfile_path": None},
    {"language": "clojure", "package_manager": "deps.edn", "manifest_path": "deps.edn", "lockfile_path": None},
    {"language": "clojure", "package_manager": "Leiningen", "manifest_path": "project.clj", "lockfile_path": None},
    {"language": "cplusplus", "package_manager": "Conan", "manifest_path": "conanfile.txt", "lockfile_path": "conan.lock"},
    {"language": "cplusplus", "package_manager": "pkg-config", "manifest_path": ".pc", "lockfile_path": None},
    {"language": "cplusplus", "package_manager": "Vcpkg", "manifest_path": "vcpkg.json", "lockfile_path": None},
    {"language": "crystal", "package_manager": "Shards", "manifest_path": "shard.yml", "lockfile_path": "shard.lock"},
    {"language": "dart", "package_manager": "Flutter", "manifest_path": "pubspec.yaml", "lockfile_path": "pubspec.lock"},
    {"language": "dart", "package_manager": "Pub", "manifest_path": "pubspec.yaml", "lockfile_path": "pubspec.lock"},
    {"language": "dotnet", "package_manager": "NuGet", "manifest_path": ".csproj", "lockfile_path": "packages.lock.json"},
    {"language": "elixir", "package_manager": "Mix", "manifest_path": "mix.exs", "lockfile_path": "mix.lock"},
    {"language": "elm", "package_manager": "Elm", "manifest_path": "elm.json", "lockfile_path": None},
    {"language": "erlang", "package_manager": "Rebar3", "manifest_path": "rebar.config", "lockfile_path": "rebar.lock"},
    {"language": "fortran", "package_manager": "fpm", "manifest_path": "fpm.toml", "lockfile_path": None},
    {"language": "fsharp", "package_manager": "Paket", "manifest_path": "paket.dependencies", "lockfile_path": "paket.lock"},
    {"language": "groovy", "package_manager": "Apache Ivy", "manifest_path": "ivy.xml", "lockfile_path": None},
    {"language": "groovy", "package_manager": "Grape", "manifest_path": "grapeConfig.xml", "lockfile_path": None},
    {"language": "haskell", "package_manager": "Cabal", "manifest_path": "cabal.project", "lockfile_path": "cabal.project.freeze"},
    {"language": "haskell", "package_manager": "Stack", "manifest_path": "stack.yaml", "lockfile_path": "stack.yaml.lock"},
    {"language": "idris", "package_manager": "Elba", "manifest_path": "elba.toml", "lockfile_path": None},
    {"language": "julia", "package_manager": "Pkg", "manifest_path": "Project.toml", "lockfile_path": "Manifest.toml"},
    {"language": "kotlin", "package_manager": "Kobalt", "manifest_path": "Build.kt", "lockfile_path": None},
    {"language": "kotlin", "package_manager": "Kotlin/JS", "manifest_path": "build.gradle.kts", "lockfile_path": None},
    {"language": "kotlin", "package_manager": "Kotlin/Native", "manifest_path": "build.gradle.kts", "lockfile_path": None},
    {"language": "lua", "package_manager": "LuaRocks", "manifest_path": "rockspec", "lockfile_path": None},
    {"language": "nim", "package_manager": "Nimble", "manifest_path": ".nimble", "lockfile_path": None},
    {"language": "nim", "package_manager": "NimScript", "manifest_path": "config.nims", "lockfile_path": None},
    {"language": "ocaml", "package_manager": "OPAM", "manifest_path": "opam", "lockfile_path": None},
    {"language": "perl", "package_manager": "Carton", "manifest_path": "cpanfile", "lockfile_path": "cpanfile.snapshot"},
    {"language": "perl", "package_manager": "CPANfile", "manifest_path": "cpanfile", "lockfile_path": None},
    {"language": "php", "package_manager": "Composer", "manifest_path": "composer.json", "lockfile_path": "composer.lock"},
    {"language": "php", "package_manager": "PEAR", "manifest_path": "package.xml", "lockfile_path": None},
    {"language": "r", "package_manager": "Packrat", "manifest_path": "packrat.lock", "lockfile_path": None},
    {"language": "r", "package_manager": "renv", "manifest_path": "renv.lock", "lockfile_path": None},
    {"language": "reason", "package_manager": "Esy", "manifest_path": "esy.json", "lockfile_path": "esy.lock"},
    {"language": "scala", "package_manager": "Bazel", "manifest_path": "WORKSPACE", "lockfile_path": None},
    {"language": "scala", "package_manager": "Bloop", "manifest_path": "bloop.settings.json", "lockfile_path": None},
    {"language": "scala", "package_manager": "Coursier", "manifest_path": "build.sbt", "lockfile_path": None},
    {"language": "scala", "package_manager": "Mill", "manifest_path": "build.sc", "lockfile_path": None},
    {"language": "scala", "package_manager": "Pants", "manifest_path": "pants.toml", "lockfile_path": None},
    {"language": "scala", "package_manager": "sbt", "manifest_path": "build.sbt", "lockfile_path": None},
    {"language": "swift", "package_manager": "Swift Package Manager", "manifest_path": "Package.swift", "lockfile_path": "Package.resolved"},
]
# fmt: on
