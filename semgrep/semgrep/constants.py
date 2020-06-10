import os
import subprocess
import sys
from enum import auto
from enum import Enum

RCE_RULE_FLAG = "--dangerously-allow-arbitrary-code-execution-from-rules"
RULES_KEY = "rules"
SEMGREP_USER_AGENT = "semgrep-give-me-yaml"
ID_KEY = "id"
SEMGREP_URL = "https://semgrep.dev/"
PLEASE_FILE_ISSUE_TEXT = "An error occurred while invoking the semgrep engine; please help us fix this by creating an issue at https://semgrep.dev"

DEFAULT_SEMGREP_CONFIG_NAME = "semgrep"
DEFAULT_CONFIG_FILE = f".{DEFAULT_SEMGREP_CONFIG_NAME}.yml"
DEFAULT_CONFIG_FOLDER = f".{DEFAULT_SEMGREP_CONFIG_NAME}"

YML_EXTENSIONS = {".yml", ".yaml"}

__VERSION__ = "0.10.1"


def compute_semgrep_path() -> str:
    exec_name = "semgrep-core"
    if subprocess.run(["which", exec_name], stdout=subprocess.DEVNULL).returncode != 0:
        # look for something in the same dir as the Python interpreter
        relative_path = os.path.join(os.path.dirname(sys.executable), exec_name)
        if os.path.exists(relative_path):
            exec_name = relative_path
    return exec_name


SEMGREP_PATH = compute_semgrep_path()


class OutputFormat(Enum):
    TEXT = auto()
    JSON = auto()
    JSON_DEBUG = auto()
    SARIF = auto()
