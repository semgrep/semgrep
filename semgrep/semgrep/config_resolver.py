import os
import time
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional

from ruamel.yaml import YAMLError

from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import DEFAULT_CONFIG_FOLDER
from semgrep.constants import DEFAULT_SEMGREP_CONFIG_NAME
from semgrep.constants import ID_KEY
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import RULES_KEY
from semgrep.constants import SEMGREP_USER_AGENT
from semgrep.constants import YML_EXTENSIONS
from semgrep.error import SemgrepError
from semgrep.rule_lang import parse_yaml_preserve_spans
from semgrep.rule_lang import YamlTree
from semgrep.util import debug_print
from semgrep.util import is_url
from semgrep.util import print_error
from semgrep.util import print_msg

IN_DOCKER = "SEMGREP_IN_DOCKER" in os.environ
IN_GH_ACTION = "GITHUB_WORKSPACE" in os.environ
REPO_HOME_DOCKER = "/home/repo/"

TEMPLATE_YAML_URL = (
    "https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml"
)

RULES_REGISTRY = {
    "r2c": "https://semgrep.live/c/p/r2c",
}
DEFAULT_REGISTRY_KEY = "r2c"


def manual_config(pattern: str, lang: str) -> Dict[str, Optional[YamlTree]]:
    # TODO remove when using sgrep -e ... -l ... instead of this hacked config
    pattern_tree = parse_yaml_preserve_spans(pattern, filename=None)
    error_span = parse_yaml_preserve_spans(
        f"Semgrep bug generating manual config {PLEASE_FILE_ISSUE_TEXT}", filename=None
    ).span
    return {
        "manual": YamlTree.wrap(
            {
                RULES_KEY: [
                    {
                        ID_KEY: "-",
                        "pattern": pattern_tree,
                        "message": pattern,
                        "languages": [lang],
                        "severity": "ERROR",
                    }
                ]
            },
            span=error_span,
        )
    }


def resolve_targets(targets: List[str]) -> List[Path]:
    base_path = get_base_path()
    return [
        Path(target) if Path(target).is_absolute() else base_path.joinpath(target)
        for target in targets
    ]


def adjust_for_docker(in_precommit: bool = False) -> None:
    # change into this folder so that all paths are relative to it
    if IN_DOCKER and not IN_GH_ACTION and not in_precommit:
        if not Path(REPO_HOME_DOCKER).exists():
            raise SemgrepError(
                f'you are running semgrep in docker, but you forgot to mount the current directory in Docker: missing: -v "${{PWD}}:{REPO_HOME_DOCKER}"'
            )
    if Path(REPO_HOME_DOCKER).exists():
        os.chdir(REPO_HOME_DOCKER)


def get_base_path() -> Path:
    return Path(".")


def indent(msg: str) -> str:
    return "\n".join(["\t" + line for line in msg.splitlines()])


def parse_config_at_path(
    loc: Path, base_path: Optional[Path] = None
) -> Dict[str, Optional[YamlTree]]:
    config_id = str(loc)
    if base_path:
        config_id = str(loc).replace(str(base_path), "")
    try:
        with loc.open() as f:
            return parse_config_string(config_id, f.read(), str(loc))
    except FileNotFoundError:
        print_error(f"YAML file at {loc} not found")
        return {str(loc): None}


def parse_config_string(
    config_id: str, contents: str, filename: Optional[str]
) -> Dict[str, Optional[YamlTree]]:
    try:
        data = parse_yaml_preserve_spans(contents, filename)
        return {config_id: data}
    except YAMLError as se:
        print_error(f"Invalid yaml file {config_id}:\n{indent(str(se))}")
        return {config_id: None}


def parse_config_folder(
    loc: Path, relative: bool = False
) -> Dict[str, Optional[YamlTree]]:
    configs = {}
    for l in loc.rglob("*"):
        # Allow manually specified paths with ".", but don't auto-expand them
        if not _is_hidden_config(l.relative_to(loc)) and l.suffix in YML_EXTENSIONS:
            configs.update(parse_config_at_path(l, loc if relative else None))
    return configs


def _is_hidden_config(loc: Path) -> bool:
    """
    Want to keep rules/.semgrep.yml but not path/.github/foo.yml
    Also want to keep src/.semgrep/bad_pattern.yml but not ./.pre-commit-config.yaml
    """
    return any(
        part != "."
        and part != ".."
        and part.startswith(".")
        and DEFAULT_SEMGREP_CONFIG_NAME not in part
        for part in loc.parts
    )


def load_config_from_local_path(
    location: Optional[str] = None,
) -> Dict[str, Optional[YamlTree]]:
    """
        Return config file(s) as dictionary object
    """
    base_path = get_base_path()
    if location is None:
        default_file = base_path.joinpath(DEFAULT_CONFIG_FILE)
        default_folder = base_path.joinpath(DEFAULT_CONFIG_FOLDER)
        if default_file.exists():
            return parse_config_at_path(default_file)
        elif default_folder.exists():
            return parse_config_folder(default_folder, relative=True)
        else:
            return {}
    else:
        loc = base_path.joinpath(location)
        if loc.exists():
            if loc.is_file():
                return parse_config_at_path(loc)
            elif loc.is_dir():
                return parse_config_folder(loc)
            else:
                raise SemgrepError(f"config location `{loc}` is not a file or folder!")
        else:
            addendum = ""
            if IN_DOCKER:
                addendum = " (since you are running in docker, you cannot specify arbitary paths on the host; they must be mounted into the container)"
            raise SemgrepError(
                f"unable to find a config; path `{loc}` does not exist{addendum}"
            )
    raise Exception


def download_config(config_url: str) -> Dict[str, Optional[YamlTree]]:
    import requests  # here for faster startup times

    DOWNLOADING_MESSAGE = f"downloading config..."
    SCANNING_MESSAGE = "scanning code...\033[K"
    debug_print(f"trying to download from {config_url}")
    print_msg(
        f"using config from {config_url}. See https://semgrep.live/registry for more options."
    )
    print_msg(DOWNLOADING_MESSAGE, end="\r")
    headers = {"User-Agent": SEMGREP_USER_AGENT}

    try:
        r = requests.get(config_url, stream=True, headers=headers, timeout=10)
        if r.status_code == requests.codes.ok:
            content_type = r.headers.get("Content-Type")
            yaml_types = [
                "text/plain",
                "application/x-yaml",
                "text/x-yaml",
                "text/yaml",
                "text/vnd.yaml",
            ]
            if content_type and any((ct in content_type for ct in yaml_types)):
                print_msg(SCANNING_MESSAGE)
                return parse_config_string(
                    "remote-url", r.content.decode("utf-8"), filename=None
                )
            else:
                raise SemgrepError(
                    f"unknown content-type: {content_type} returned by config url: {config_url}. Can not parse"
                )
        else:
            raise SemgrepError(
                f"bad status code: {r.status_code} returned by config url: {config_url}"
            )
    except Exception as e:
        print_error(str(e))
    return {config_url: None}


def resolve_config(config_str: Optional[str]) -> Dict[str, Optional[YamlTree]]:
    """ resolves if config arg is a registry entry, a url, or a file, folder, or loads from defaults if None"""
    start_t = time.time()
    if config_str is None:
        config = load_config_from_local_path()
    elif config_str in RULES_REGISTRY:
        config = download_config(RULES_REGISTRY[config_str])
    elif is_url(config_str):
        config = download_config(config_str)
    else:
        config = load_config_from_local_path(config_str)
    if config:
        debug_print(f"loaded {len(config)} configs in {time.time() - start_t}")
    return config


def generate_config() -> None:
    import requests  # here for faster startup times

    # defensive coding
    if Path(DEFAULT_CONFIG_FILE).exists():
        raise SemgrepError(
            f"{DEFAULT_CONFIG_FILE} already exists. Please remove and try again"
        )
    try:
        r = requests.get(TEMPLATE_YAML_URL, timeout=10)
        r.raise_for_status()
        template_str = r.text
    except Exception as e:
        debug_print(str(e))
        print_msg(
            f"There was a problem downloading the latest template config. Using fallback template"
        )
        template_str = """rules:
  - id: eqeq-is-bad
    pattern: $X == $X
    message: "$X == $X is a useless equality check"
    languages: [python]
    severity: ERROR"""
    try:
        with open(DEFAULT_CONFIG_FILE, "w") as template:
            template.write(template_str)
            print_msg(f"Template config successfully written to {DEFAULT_CONFIG_FILE}")
    except Exception as e:
        raise SemgrepError(str(e))
