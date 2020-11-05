import logging
import os
import time
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

from ruamel.yaml import YAMLError

from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import DEFAULT_CONFIG_FOLDER
from semgrep.constants import DEFAULT_SEMGREP_CONFIG_NAME
from semgrep.constants import ID_KEY
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import RULES_KEY
from semgrep.constants import SEMGREP_URL
from semgrep.constants import SEMGREP_USER_AGENT
from semgrep.constants import YML_EXTENSIONS
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import SemgrepError
from semgrep.error import UNPARSEABLE_YAML_EXIT_CODE
from semgrep.rule import Rule
from semgrep.rule_lang import parse_yaml_preserve_spans
from semgrep.rule_lang import Span
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.util import is_url

logger = logging.getLogger(__name__)

IN_DOCKER = "SEMGREP_IN_DOCKER" in os.environ
IN_GH_ACTION = "GITHUB_WORKSPACE" in os.environ

SRC_DIRECTORY = Path(os.environ.get("SEMGREP_SRC_DIRECTORY", Path("/") / "src"))
OLD_SRC_DIRECTORY = Path("/") / "home" / "repo"

TEMPLATE_YAML_URL = (
    "https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml"
)

RULES_REGISTRY = {"r2c": "https://semgrep.dev/c/p/r2c"}
DEFAULT_REGISTRY_KEY = "r2c"

MISSING_RULE_ID = "no-rule-id"


class Config:
    def __init__(self, valid_configs: Dict[str, List[Rule]]) -> None:
        """
            Handles parsing and validating of config files
            and exposes ability to get all rules in parsed config files
        """
        self.valid = valid_configs

    @classmethod
    def from_pattern_lang(
        cls, pattern: str, lang: str
    ) -> Tuple["Config", List[SemgrepError]]:
        config_dict = manual_config(pattern, lang)
        valid, errors = cls._validate(config_dict)
        return cls(valid), errors

    @classmethod
    def from_config_list(
        cls, configs: List[str]
    ) -> Tuple["Config", List[SemgrepError]]:
        """
            Takes in list of files/directories and returns Config object as well as
            list of errors parsing said config files

            If empty list is passed, tries to read config file at default locations
        """
        config_dict: Dict[str, YamlTree] = {}
        errors: List[SemgrepError] = []

        if not configs:
            try:
                config_dict.update(load_default_config())
            except SemgrepError as e:
                errors.append(e)

        for i, config in enumerate(configs):
            try:
                # Patch config_id to fix https://github.com/returntocorp/semgrep/issues/1912
                resolved_config = resolve_config(config)
                if not resolved_config:
                    logger.debug(f"Could not resolve config for {config}. Skipping.")
                    continue
                # Extract key and value
                resolved_config_key, resolved_config_yaml_tree = next(
                    iter(resolved_config.items())
                )
                patched_resolved_config: Dict[str, YamlTree] = {}
                patched_resolved_config[
                    f"{resolved_config_key}_{i}"
                ] = resolved_config_yaml_tree

                config_dict.update(patched_resolved_config)
            except SemgrepError as e:
                errors.append(e)

        valid, parse_errors = cls._validate(config_dict)
        errors.extend(parse_errors)
        return cls(valid), errors

    def get_rules(self, no_rewrite_rule_ids: bool) -> List[Rule]:
        """
            Return list of rules

            If no_rewrite_rule_ids is True will not add
            path to config file to start of rule_ids
        """
        configs = self.valid
        if not no_rewrite_rule_ids:
            # re-write the configs to have the hierarchical rule ids
            configs = self._rename_rule_ids(configs)

        return [rule for rules in configs.values() for rule in rules]

    @staticmethod
    def _safe_relative_to(a: Path, b: Path) -> Path:
        try:
            return a.relative_to(b)
        except ValueError:
            # paths had no common prefix; not possible to relativize
            return a

    @staticmethod
    def _convert_config_id_to_prefix(config_id: str) -> str:
        at_path = Path(config_id)
        at_path = Config._safe_relative_to(at_path, Path.cwd())

        prefix = ".".join(at_path.parts[:-1]).lstrip("./").lstrip(".")
        if len(prefix):
            prefix += "."
        return prefix

    @staticmethod
    def _rename_rule_ids(valid_configs: Dict[str, List[Rule]]) -> Dict[str, List[Rule]]:
        transformed = {}
        for config_id, rules in valid_configs.items():
            transformed[config_id] = [
                rule.with_id(
                    f"{Config._convert_config_id_to_prefix(config_id)}{rule.id or MISSING_RULE_ID}"
                )
                for rule in rules
            ]
        return transformed

    @staticmethod
    def _validate(
        config_dict: Dict[str, YamlTree]
    ) -> Tuple[Dict[str, List[Rule]], List[SemgrepError]]:
        """
            Take configs and separate into valid and list of errors parsing the invalid ones
        """
        errors: List[SemgrepError] = []
        valid: Dict[str, Any] = {}
        for config_id, config_yaml_tree in config_dict.items():
            config = config_yaml_tree.value
            if not isinstance(config, YamlMap):
                errors.append(SemgrepError(f"{config_id} was not a mapping"))
                continue

            rules = config.get(RULES_KEY)
            if rules is None:
                errors.append(
                    InvalidRuleSchemaError(
                        short_msg="missing keys",
                        long_msg=f"{config_id} is missing `{RULES_KEY}` as top-level key",
                        spans=[config_yaml_tree.span.truncate(lines=5)],
                    )
                )
                continue
            valid_rules = []
            for rule_dict in rules.value:

                try:
                    rule = validate_single_rule(config_id, rule_dict)
                except InvalidRuleSchemaError as ex:
                    errors.append(ex)
                else:
                    valid_rules.append(rule)

            if valid_rules:
                valid[config_id] = valid_rules
        return valid, errors


def validate_single_rule(
    config_id: str, rule_yaml: YamlTree[YamlMap]
) -> Optional[Rule]:
    """
        Validate that a rule dictionary contains all necessary keys
        and can be correctly parsed.
    """
    rule: YamlMap = rule_yaml.value

    # Defaults to search mode if mode is not specified
    return Rule.from_yamltree(rule_yaml)


def manual_config(pattern: str, lang: str) -> Dict[str, YamlTree]:
    # TODO remove when using sgrep -e ... -l ... instead of this hacked config
    pattern_span = Span.from_string(pattern, filename="CLI Input")
    pattern_tree = YamlTree[str](value=pattern, span=pattern_span)
    error_span = Span.from_string(
        f"Semgrep bug generating manual config {PLEASE_FILE_ISSUE_TEXT}", filename=None
    )
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


def adjust_for_docker() -> None:
    # change into this folder so that all paths are relative to it
    if IN_DOCKER and not IN_GH_ACTION:
        if OLD_SRC_DIRECTORY.exists():
            raise SemgrepError(
                f"Detected Docker environment using old code volume, please use '{SRC_DIRECTORY}' instead of '{OLD_SRC_DIRECTORY}'"
            )
        if not SRC_DIRECTORY.exists():
            raise SemgrepError(
                f"Detected Docker environment without a code volume, please include '-v \"${{PWD}}:{SRC_DIRECTORY}\"'"
            )
    if SRC_DIRECTORY.exists():
        os.chdir(SRC_DIRECTORY)


def get_base_path() -> Path:
    return Path(".")


def indent(msg: str) -> str:
    return "\n".join(["\t" + line for line in msg.splitlines()])


def parse_config_at_path(
    loc: Path, base_path: Optional[Path] = None
) -> Dict[str, YamlTree]:
    """
        Assumes file at loc exists
    """
    config_id = str(loc)
    if base_path:
        config_id = str(loc).replace(str(base_path), "")

    with loc.open() as f:
        return parse_config_string(config_id, f.read(), str(loc))


def parse_config_string(
    config_id: str, contents: str, filename: Optional[str]
) -> Dict[str, YamlTree]:
    if not contents:
        raise SemgrepError(
            f"Empty configuration file {filename}", code=UNPARSEABLE_YAML_EXIT_CODE,
        )
    try:
        data = parse_yaml_preserve_spans(contents, filename)
        return {config_id: data}
    except YAMLError as se:
        raise SemgrepError(
            f"Invalid YAML file {config_id}:\n{indent(str(se))}",
            code=UNPARSEABLE_YAML_EXIT_CODE,
        )


def parse_config_folder(loc: Path, relative: bool = False) -> Dict[str, YamlTree]:
    configs = {}
    for l in loc.rglob("*"):
        # Allow manually specified paths with ".", but don't auto-expand them
        if not _is_hidden_config(l.relative_to(loc)) and l.suffix in YML_EXTENSIONS:
            if l.is_file():
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


def load_default_config() -> Dict[str, YamlTree]:
    """
        Load config from DEFAULT_CONFIG_FILE or DEFAULT_CONFIG_FOLDER
    """
    base_path = get_base_path()
    default_file = base_path.joinpath(DEFAULT_CONFIG_FILE)
    default_folder = base_path.joinpath(DEFAULT_CONFIG_FOLDER)
    if default_file.exists():
        return parse_config_at_path(default_file)
    elif default_folder.exists():
        return parse_config_folder(default_folder, relative=True)
    else:
        return {}


def load_config_from_local_path(location: str) -> Dict[str, YamlTree]:
    """
        Return config file(s) as dictionary object
    """
    base_path = get_base_path()
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


def nice_semgrep_url(url: str) -> str:
    """
    Alters semgrep.dev urls to let user
    click through to the nice display page instead
    of raw YAML.
    Replaces '/c/' in semgrep urls with '/'.
    """
    import urllib

    parsed = urllib.parse.urlparse(url)
    if "semgrep.dev" in parsed.netloc and parsed.path.startswith("/c"):
        return url.replace("/c/", "/")
    return url


def download_config(config_url: str) -> Dict[str, YamlTree]:
    import requests  # here for faster startup times

    DOWNLOADING_MESSAGE = f"downloading config..."
    logger.debug(f"trying to download from {config_url}")
    logger.info(
        f"using config from {nice_semgrep_url(config_url)}. Visit https://semgrep.dev/registry to see all public rules."
    )
    logger.info(DOWNLOADING_MESSAGE)
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
                return parse_config_string(
                    "remote-url",
                    r.content.decode("utf-8"),
                    filename=f"{config_url[:20]}...",
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
        raise SemgrepError(f"Failed to download config from {config_url}: {str(e)}")

    return None


def is_registry_id(config_str: str) -> bool:
    """
        Starts with r/, p/, s/ for registry, pack, and snippet respectively
    """
    return config_str[:2] in {"r/", "p/", "s/"}


def is_saved_snippet(config_str: str) -> bool:
    """
        config_str is saved snippet which has format username:snippetname
    """
    return len(config_str.split(":")) == 2


def registry_id_to_url(registry_id: str) -> str:
    """
        Convert from registry_id to semgrep.dev url
    """
    return f"{SEMGREP_URL}{registry_id}"


def saved_snippet_to_url(snippet_id: str) -> str:
    """
        Convert from username:snippetname to semgrep.dev url
    """
    return registry_id_to_url(f"s/{snippet_id}")


def resolve_config(config_str: str) -> Dict[str, YamlTree]:
    """ resolves if config arg is a registry entry, a url, or a file, folder, or loads from defaults if None"""
    start_t = time.time()
    if config_str in RULES_REGISTRY:
        config = download_config(RULES_REGISTRY[config_str])
    elif is_url(config_str):
        config = download_config(config_str)
    elif is_registry_id(config_str):
        config = download_config(registry_id_to_url(config_str))
    elif is_saved_snippet(config_str):
        config = download_config(saved_snippet_to_url(config_str))
    else:
        config = load_config_from_local_path(config_str)
    if config:
        logger.debug(f"loaded {len(config)} configs in {time.time() - start_t}")
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
        logger.debug(str(e))
        logger.warning(
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
            logger.info(
                f"Template config successfully written to {DEFAULT_CONFIG_FILE}"
            )
    except Exception as e:
        raise SemgrepError(str(e))
