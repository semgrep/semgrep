import json
import os
import time
import urllib.parse
from collections import OrderedDict
from enum import auto
from enum import Enum
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Tuple

import requests
from ruamel.yaml import YAMLError

from semgrep.app import auth
from semgrep.constants import CLI_RULE_ID
from semgrep.constants import Colors
from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import DEFAULT_CONFIG_FOLDER
from semgrep.constants import DEFAULT_SEMGREP_CONFIG_NAME
from semgrep.constants import ID_KEY
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import RULES_KEY
from semgrep.constants import RuleSeverity
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import SemgrepError
from semgrep.error import UNPARSEABLE_YAML_EXIT_CODE
from semgrep.rule import Rule
from semgrep.rule import rule_without_metadata
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import EmptyYamlException
from semgrep.rule_lang import parse_yaml_preserve_spans
from semgrep.rule_lang import Span
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.state import get_state
from semgrep.util import is_config_suffix
from semgrep.util import is_url
from semgrep.util import terminal_wrap
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

AUTO_CONFIG_KEY = "auto"
AUTO_CONFIG_LOCATION = "c/auto"

MISSING_RULE_ID = "no-rule-id"

DEFAULT_CONFIG = {
    "rules": [
        {
            "id": "eqeq-is-bad",
            "pattern": "$X == $X",
            "message": "$X == $X is a useless equality check",
            "languages": ["python"],
            "severity": RuleSeverity.ERROR.value,
        },
    ],
}


class ConfigType(Enum):
    REGISTRY = auto()
    LOCAL = auto()


class ConfigPath:
    _origin = ConfigType.LOCAL
    _config_path = ""
    _project_url = None
    _extra_headers: Dict[str, str] = {}

    def __init__(self, config_str: str, project_url: Optional[str] = None) -> None:
        """
        Mutates Metrics state!
        Takes a user's inputted config_str and transforms it into the appropriate
        path, checking whether the config string is a registry url or not. If it
        is, also set the appropriate Metrics flag
        """
        state = get_state()
        self._project_url = project_url
        self._origin = ConfigType.REGISTRY
        if config_str == "r2c":
            state.metrics.add_feature("config", "r2c")
            self._config_path = "https://semgrep.dev/c/p/r2c"
        elif is_url(config_str):
            state.metrics.add_feature("config", "url")
            self._config_path = config_str
        elif is_policy_id(config_str):
            state.metrics.add_feature("config", "policy")
            self._config_path = url_for_policy(config_str)
        elif is_sca(config_str):
            state.metrics.add_feature("config", "sca")
            self._config_path = url_for_sca()
        elif is_registry_id(config_str):
            state.metrics.add_feature("config", f"registry:prefix-{config_str[0]}")
            self._config_path = registry_id_to_url(config_str)
        elif is_saved_snippet(config_str):
            state.metrics.add_feature("config", f"registry:snippet-id")
            self._config_path = saved_snippet_to_url(config_str)
        elif config_str == AUTO_CONFIG_KEY:
            state.metrics.add_feature("config", "auto")
            self._config_path = f"{state.env.semgrep_url}/{AUTO_CONFIG_LOCATION}"
        else:
            state.metrics.add_feature("config", "local")
            self._origin = ConfigType.LOCAL
            self._config_path = str(Path(config_str).expanduser())

        if self.is_registry_url():
            state.metrics.is_using_registry = True
            state.metrics.add_registry_url(self._config_path)

    def resolve_config(self) -> Mapping[str, YamlTree]:
        """resolves if config arg is a registry entry, a url, or a file, folder, or loads from defaults if None"""
        start_t = time.time()

        if self._origin == ConfigType.REGISTRY:
            config = self._download_config()
        else:
            config = self._load_config_from_local_path()

        if config:
            logger.debug(f"loaded {len(config)} configs in {time.time() - start_t}")
        return config

    def _nice_semgrep_url(self, url: str) -> str:
        """
        Alters semgrep.dev urls to let user
        click through to the nice display page instead
        of raw YAML.
        Replaces '/c/' in semgrep urls with '/'.
        """
        parsed = urllib.parse.urlparse(url)
        if "semgrep.dev" in parsed.netloc and parsed.path.startswith("/c"):
            return url.replace("/c/", "/")
        return url

    def _download_config(self) -> Mapping[str, YamlTree]:
        """
        Download a configuration from semgrep.dev
        """
        config_url = self._config_path
        logger.debug(f"trying to download from {self._nice_semgrep_url(config_url)}")
        try:
            config = parse_config_string(
                "remote-url",
                self._make_config_request(),
                filename=f"{config_url[:20]}...",
            )
            logger.debug(f"finished downloading from {config_url}")
            return config
        except InvalidRuleSchemaError as e:
            notice = f"\nRules downloaded from {config_url} failed to parse.\nThis is likely because rules have been added that use functionality introduced in later versions of semgrep.\nPlease upgrade to latest version of semgrep (see https://semgrep.dev/docs/upgrading/) and try again.\n"
            notice_color = with_color(Colors.red, notice, bold=True)
            logger.error(notice_color)
            raise e
        except Exception as e:
            raise SemgrepError(
                terminal_wrap(f"Failed to download config from {config_url}: {str(e)}")
            )

    def _load_config_from_local_path(self) -> Dict[str, YamlTree]:
        """
        Return config file(s) as dictionary object
        """
        loc = Path(self._config_path)

        logger.debug(f"Loading local config from {loc}")
        if loc.exists():
            if loc.is_file():
                config = parse_config_at_path(loc)
            elif loc.is_dir():
                config = parse_config_folder(loc)
            else:
                raise SemgrepError(f"config location `{loc}` is not a file or folder!")
        else:
            env = get_state().env
            addendum = ""
            if env.in_docker:
                addendum = " (since you are running in docker, you cannot specify arbitrary paths on the host; they must be mounted into the container)"
            raise SemgrepError(
                f"WARNING: unable to find a config; path `{loc}` does not exist{addendum}"
            )
        logger.debug(f"Done loading local config from {loc}")
        return config

    def _make_config_request(self) -> str:
        app_session = get_state().app_session
        r = app_session.get(
            self._config_path,
            headers={"Accept": "application/json", **self._extra_headers},
        )
        if r.status_code == requests.codes.ok:
            return r.content.decode("utf-8", errors="replace")
        else:
            raise SemgrepError(
                f"bad status code: {r.status_code} returned by config url: {self._config_path}"
            )

    def is_registry_url(self) -> bool:
        return self._origin == ConfigType.REGISTRY

    def __str__(self) -> str:
        return self._config_path


class Config:
    def __init__(self, valid_configs: Mapping[str, Sequence[Rule]]) -> None:
        """
        Handles parsing and validating of config files
        and exposes ability to get all rules in parsed config files
        """
        self.valid = valid_configs

    @classmethod
    def from_pattern_lang(
        cls, pattern: str, lang: str, replacement: Optional[str] = None
    ) -> Tuple["Config", Sequence[SemgrepError]]:
        config_dict = manual_config(pattern, lang, replacement)
        valid, errors = cls._validate(config_dict)
        return cls(valid), errors

    @classmethod
    def from_config_list(
        cls, configs: Sequence[str], project_url: Optional[str]
    ) -> Tuple["Config", Sequence[SemgrepError]]:
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
                resolved_config = ConfigPath(config, project_url).resolve_config()
                if not resolved_config:
                    logger.verbose(f"Could not resolve config for {config}. Skipping.")
                    continue

                for (
                    resolved_config_key,
                    resolved_config_yaml_tree,
                ) in resolved_config.items():
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
            self._rename_rule_ids(configs)

        # deduplicate rules, ignoring metadata, which is not displayed
        # in the result
        return list(
            OrderedDict(
                (rule_without_metadata(rule), rule)
                for rules in configs.values()
                for rule in rules
            ).values()
        )

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
        try:
            at_path = Config._safe_relative_to(at_path, Path.cwd())
        except FileNotFoundError:
            pass

        prefix = ".".join(at_path.parts[:-1]).lstrip("./").lstrip(".")
        if len(prefix):
            prefix += "."
        return prefix

    @staticmethod
    def _rename_rule_ids(valid_configs: Mapping[str, Sequence[Rule]]) -> None:
        for config_id, rules in valid_configs.items():
            for rule in rules:
                rule.rename_id(
                    f"{Config._convert_config_id_to_prefix(config_id)}{rule.id or MISSING_RULE_ID}"
                )

    @staticmethod
    def _validate(
        config_dict: Mapping[str, YamlTree]
    ) -> Tuple[Mapping[str, Sequence[Rule]], Sequence[SemgrepError]]:
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


def manual_config(
    pattern: str, lang: str, replacement: Optional[str]
) -> Dict[str, YamlTree]:
    """Create a fake rule when we only have a pattern and language

    This is used when someone calls `semgrep scan -e print -l py`
    """
    pattern_span = Span.from_string(pattern, filename="CLI Input")
    pattern_tree = YamlTree[str](value=pattern, span=pattern_span)
    error_span = Span.from_string(
        f"Semgrep bug generating manual config {PLEASE_FILE_ISSUE_TEXT}", filename=None
    )
    rules_key = {
        ID_KEY: CLI_RULE_ID,
        "pattern": pattern_tree,
        "message": pattern,
        "languages": [lang],
        "severity": RuleSeverity.ERROR.value,
    }

    if replacement:
        rules_key["fix"] = replacement

    return {
        "manual": YamlTree.wrap(
            {RULES_KEY: [rules_key]},
            span=error_span,
        )
    }


def adjust_for_docker() -> None:
    """change into this folder so that all paths are relative to it"""
    env = get_state().env
    if env.in_docker and not env.in_gh_action:
        try:
            # check if there's at least one file in /src
            next(env.src_directory.iterdir())
        except (NotADirectoryError, StopIteration):
            raise SemgrepError(
                f"Detected Docker environment without a code volume, please include '-v \"${{PWD}}:{env.src_directory}\"'"
            )
        else:
            os.chdir(env.src_directory)


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

    return parse_config_string(config_id, loc.read_text(), str(loc))


def parse_config_string(
    config_id: str, contents: str, filename: Optional[str]
) -> Dict[str, YamlTree]:
    if not contents:
        raise SemgrepError(
            f"Empty configuration file {filename}", code=UNPARSEABLE_YAML_EXIT_CODE
        )
    try:
        # we pretend it came from YAML so we can keep later code simple
        data = YamlTree.wrap(json.loads(contents), EmptySpan)
        return {config_id: data}
    except json.decoder.JSONDecodeError:
        pass

    try:
        data = parse_yaml_preserve_spans(contents, filename)
    except EmptyYamlException:
        raise SemgrepError(
            f"Empty configuration file {filename}", code=UNPARSEABLE_YAML_EXIT_CODE
        )
    except YAMLError as se:
        raise SemgrepError(
            f"Invalid YAML file {config_id}:\n{indent(str(se))}",
            code=UNPARSEABLE_YAML_EXIT_CODE,
        )
    return {config_id: data}


def parse_config_folder(loc: Path, relative: bool = False) -> Dict[str, YamlTree]:
    configs = {}
    for l in loc.rglob("*"):
        # Allow manually specified paths with ".", but don't auto-expand them
        correct_suffix = is_config_suffix(l)
        if not _is_hidden_config(l.relative_to(loc)) and correct_suffix:
            if l.is_file():
                configs.update(parse_config_at_path(l, loc if relative else None))
    return configs


def _is_hidden_config(loc: Path) -> bool:
    """
    Want to keep rules/.semgrep.yml but not path/.github/foo.yml
    Also want to keep src/.semgrep/bad_pattern.yml but not ./.pre-commit-config.yaml
    """
    return any(
        part != os.curdir
        and part != os.pardir
        and part.startswith(".")
        and DEFAULT_SEMGREP_CONFIG_NAME not in part
        for part in loc.parts
    )


def load_default_config() -> Dict[str, YamlTree]:
    """
    Load config from DEFAULT_CONFIG_FILE or DEFAULT_CONFIG_FOLDER
    """
    default_file = Path(DEFAULT_CONFIG_FILE)
    default_folder = Path(DEFAULT_CONFIG_FOLDER)
    if default_file.exists():
        return parse_config_at_path(default_file)
    elif default_folder.exists():
        return parse_config_folder(default_folder, relative=True)
    else:
        return {}


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
    env = get_state().env
    return f"{env.semgrep_url}/{registry_id}"


def url_for_policy(config_str: str) -> str:
    """
    Return url to download a policy for a given repo_name

    For now uses envvar to know what repo_name is

    Set SEMGREP_POLICY_INCLUDE_CAI env var to include CAI Rules
    """
    deployment_id = auth.get_deployment_id()

    if deployment_id is None:
        raise SemgrepError(
            "Invalid API Key. Run `semgrep logout` and `semgrep login` again."
        )

    repo_name = os.environ.get("SEMGREP_REPO_NAME")
    include_cai = os.environ.get("SEMGREP_POLICY_INCLUDE_CAI")

    if repo_name is None:
        raise SemgrepError(
            "Need to set env var SEMGREP_REPO_NAME to use `--config policy`"
        )

    env = get_state().env
    request_url = f"{env.semgrep_url}/api/agent/deployments/{deployment_id}/repos/{repo_name}/rules.yaml"

    if include_cai:
        return f"{request_url}?include_cai=1"
    return request_url


def is_policy_id(config_str: str) -> bool:
    return config_str == "policy"


def url_for_sca() -> str:
    deployment_id = auth.get_deployment_id()
    if deployment_id is None:
        raise SemgrepError(
            "Invalid API Key. Run `semgrep logout` and `semgrep login` again."
        )
    env = get_state().env
    repo_name = os.environ.get("SEMGREP_REPO_NAME")
    return f"{env.semgrep_url}/api/agent/deployments/{deployment_id}/repos/{repo_name}/rules.yaml?sca=true"


def is_sca(config_str: str) -> bool:
    return config_str == "sca"


def saved_snippet_to_url(snippet_id: str) -> str:
    """
    Convert from username:snippetname to semgrep.dev url
    """
    return registry_id_to_url(f"s/{snippet_id}")


def is_pack_id(config_str: str) -> bool:
    return config_str[:2] == "p/"


def get_config(
    pattern: Optional[str],
    lang: Optional[str],
    config_strs: Sequence[str],
    *,
    project_url: Optional[str],
    replacement: Optional[str] = None,
) -> Tuple[Config, Sequence[SemgrepError]]:
    if pattern:
        if not lang:
            raise SemgrepError("language must be specified when a pattern is passed")
        config, errors = Config.from_pattern_lang(pattern, lang, replacement)
    else:
        if replacement:
            raise SemgrepError(
                "command-line replacement flag can only be used with command-line pattern; when using a config file add the fix: key instead"
            )
        config, errors = Config.from_config_list(config_strs, project_url)

    if not config:
        raise SemgrepError(
            f"No config given and {DEFAULT_CONFIG_FILE} was not found. Try running with --help to debug or if you want to download a default config, try running with --config r2c"
        )

    return config, errors
