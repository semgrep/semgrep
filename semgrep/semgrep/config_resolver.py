import json
import os
import time
from collections import OrderedDict
from enum import auto
from enum import Enum
from pathlib import Path
from typing import Any
from typing import Dict
from typing import IO
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Tuple

import requests
from ruamel.yaml import YAML
from ruamel.yaml import YAMLError

from semgrep.app import app_session
from semgrep.app import auth
from semgrep.app.metrics import metric_manager
from semgrep.constants import CLI_RULE_ID
from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import DEFAULT_CONFIG_FOLDER
from semgrep.constants import DEFAULT_SEMGREP_CONFIG_NAME
from semgrep.constants import ID_KEY
from semgrep.constants import IN_DOCKER
from semgrep.constants import IN_GH_ACTION
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import RULES_KEY
from semgrep.constants import RuleSeverity
from semgrep.constants import SEMGREP_CDN_BASE_URL
from semgrep.constants import SEMGREP_URL
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import SemgrepError
from semgrep.error import UNPARSEABLE_YAML_EXIT_CODE
from semgrep.rule import Rule
from semgrep.rule import rule_without_metadata
from semgrep.rule_lang import EmptyYamlException
from semgrep.rule_lang import parse_yaml_preserve_spans
from semgrep.rule_lang import Span
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.types import JsonObject
from semgrep.util import is_config_suffix
from semgrep.util import is_url
from semgrep.util import terminal_wrap
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

SRC_DIRECTORY = Path(os.environ.get("SEMGREP_SRC_DIRECTORY", Path("/") / "src"))
OLD_SRC_DIRECTORY = Path("/") / "home" / "repo"

AUTO_CONFIG_KEY = "auto"
AUTO_CONFIG_LOCATION = "c/auto"
RULES_REGISTRY = {"r2c": "https://semgrep.dev/c/p/r2c"}

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
    CDN = auto()
    LOCAL = auto()


class ConfigPath:
    _origin = ConfigType.LOCAL
    _config_path = ""
    _project_url = None
    _extra_headers: Dict[str, str] = {}

    def __init__(self, config_str: str, project_url: Optional[str]) -> None:
        """
        Mutates MetricManager state!
        Takes a user's inputted config_str and transforms it into the appropriate
        path, checking whether the config string is a registry url or not. If it
        is, also set the appropriate MetricManager flag
        """
        self._project_url = project_url
        self._origin = ConfigType.REGISTRY

        if config_str in RULES_REGISTRY:
            self._config_path = RULES_REGISTRY[config_str]
        elif is_url(config_str):
            self._config_path = config_str
        elif is_policy_id(config_str):
            self._config_path = url_for_policy(config_str)
        elif is_registry_id(config_str):
            self._config_path = registry_id_to_url(config_str)
        elif is_saved_snippet(config_str):
            self._config_path = saved_snippet_to_url(config_str)
        elif config_str == AUTO_CONFIG_KEY:
            logger.warning(
                terminal_wrap(
                    "Auto config uses Semgrep rules to scan your codebase and the Semgrep Registry"
                    " to generate recommended rules based on your languages and frameworks."
                ),
            )
            if self._project_url is not None:
                self._extra_headers["X-Semgrep-Project"] = self._project_url
                logger.warning(
                    terminal_wrap(
                        f"Looking up '{self._project_url}' in Registry to see if recommendations exist..."
                    )
                )
            self._config_path = f"{SEMGREP_URL}/{AUTO_CONFIG_LOCATION}"
        else:
            self._origin = ConfigType.LOCAL
            self._config_path = str(Path(config_str).expanduser())

        if self.is_registry_url():
            metric_manager.set_using_server_true()

    def resolve_config(self) -> Mapping[str, YamlTree]:
        """resolves if config arg is a registry entry, a url, or a file, folder, or loads from defaults if None"""
        start_t = time.time()

        if self._origin == ConfigType.REGISTRY:
            config = self._download_config()
        elif self._origin == ConfigType.CDN:
            config = download_pack_config(self._config_path)
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
        import urllib

        parsed = urllib.parse.urlparse(url)
        if "semgrep.dev" in parsed.netloc and parsed.path.startswith("/c"):
            return url.replace("/c/", "/")
        return url

    def _download_config(self) -> Mapping[str, YamlTree]:
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
            addendum = ""
            if IN_DOCKER:
                addendum = " (since you are running in docker, you cannot specify arbitrary paths on the host; they must be mounted into the container)"
            raise SemgrepError(
                f"WARNING: unable to find a config; path `{loc}` does not exist{addendum}"
            )
        logger.debug(f"Done loading local config from {loc}")
        return config

    def _make_config_request(self) -> str:
        r = app_session.get(self._config_path, headers=self._extra_headers)
        if r.status_code == requests.codes.ok:
            content_type = r.headers.get("Content-Type")
            yaml_types = [
                "text/plain",
                "application/x-yaml",
                "text/x-yaml",
                "text/yaml",
                "text/vnd.yaml",
            ]
            if content_type and any(ct in content_type for ct in yaml_types):
                return r.content.decode("utf-8", errors="replace")
            else:
                raise SemgrepError(
                    f"unknown content-type: {content_type} returned by config url: {self._config_path}. Can not parse"
                )
        else:
            raise SemgrepError(
                f"bad status code: {r.status_code} returned by config url: {self._config_path}"
            )

    def is_registry_url(self) -> bool:
        return self._origin == ConfigType.REGISTRY or self._origin == ConfigType.CDN

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
        at_path = Config._safe_relative_to(at_path, Path.cwd())

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
    # TODO remove when using sgrep -e ... -l ... instead of this hacked config
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
            f"Empty configuration file {filename}",
            code=UNPARSEABLE_YAML_EXIT_CODE,
        )
    try:
        data = parse_yaml_preserve_spans(contents, filename)
        return {config_id: data}
    except EmptyYamlException:
        raise SemgrepError(
            f"Empty configuration file {filename}",
            code=UNPARSEABLE_YAML_EXIT_CODE,
        )
    except YAMLError as se:
        raise SemgrepError(
            f"Invalid YAML file {config_id}:\n{indent(str(se))}",
            code=UNPARSEABLE_YAML_EXIT_CODE,
        )


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
    return f"{SEMGREP_URL}/{registry_id}"


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

    request_url = f"{SEMGREP_URL}/api/agent/deployments/{deployment_id}/repos/{repo_name}/rules.yaml"

    if include_cai:
        return f"{request_url}?include_cai=1"
    return request_url


def is_policy_id(config_str: str) -> bool:
    return config_str == "policy"


def saved_snippet_to_url(snippet_id: str) -> str:
    """
    Convert from username:snippetname to semgrep.dev url
    """
    return registry_id_to_url(f"s/{snippet_id}")


def download_pack_config(ruleset_name: str) -> Dict[str, YamlTree]:
    from packaging.version import Version
    import time
    import concurrent.futures
    from io import StringIO

    config_url_base = f"{SEMGREP_CDN_BASE_URL}/ruleset/{ruleset_name}"
    config_versions_url = f"{config_url_base}/versions"

    def get_latest_version(ruleset_name: str) -> Version:
        """
        Get the latest version of ruleset_name by getting all existing versions
        and returning max semver
        """
        logger.debug(
            f"Retrieving versions file of {ruleset_name} at {config_versions_url}"
        )
        try:
            r = app_session.get(config_versions_url)
        except Exception as e:
            raise SemgrepError(
                f"Failed to get available versions of {ruleset_name}: {str(e)}"
            )

        if not r.ok:
            raise SemgrepError(
                f"Bad status code: {r.status_code} returned by url: {config_versions_url}"
            )

        logger.debug(f"Avalabile versions for {ruleset_name}: {r.text}")
        try:
            versions_json = json.loads(r.text)
        except json.decoder.JSONDecodeError as e:
            raise SemgrepError(
                f"Failed to parse versions file of pack {ruleset_name}: Invalid Json"
            )

        versions_parsed = []
        for version_string in versions_json:
            try:
                versions_parsed.append(Version(version_string))
            except ValueError:
                logger.info(
                    f"Could not parse {version_string} in versions of {ruleset_name} pack as valid semver. Ignoring that version string."
                )
        latest_version = max(versions_parsed)
        logger.debug(f"Lastest version of {ruleset_name} is {latest_version}")

        return latest_version

    def get_ruleset(ruleset_name: str, version: Version) -> JsonObject:
        """
        Returns json blop of given ruleset_name/version
        """
        config_full_url = f"{config_url_base}/{version}"
        try:
            r = app_session.get(config_full_url)
        except Exception as e:
            raise SemgrepError(
                f"Failed to download config for {ruleset_name}/{version}: {str(e)}"
            )

        if not r.ok:
            raise SemgrepError(
                f"Bad status code: {r.status_code} returned by url: {config_full_url}"
            )

        logger.debug(f"Retrieved ruleset definition: {r.text}")
        try:
            ruleset_json = json.loads(r.text)
        except json.decoder.JSONDecodeError as e:
            raise SemgrepError(
                f"Failed to parse ruleset {ruleset_name}/{version} as valid json"
            )

        return ruleset_json  # type: ignore

    def get_rule(rule_id: str, version: str) -> JsonObject:
        """
        Returns yaml rule definition of rule_id/version
        """
        rule_url = f"{SEMGREP_CDN_BASE_URL}/public/rule/{rule_id}/{version}"
        try:
            r = app_session.get(rule_url)
        except Exception as e:
            raise SemgrepError(
                f"Failed to download rule {rule_id}/{version} from {rule_url}: {e}"
            )

        if r.status_code != requests.codes.ok:
            raise SemgrepError(
                f"Bad status code: {r.status_code} returned by url: {rule_url}"
            )

        try:
            yaml = YAML()
            rules = yaml.load(r.text)
        except Exception as e:
            raise SemgrepError(
                f"Could not parse yaml for {rule_id}/{version} as valid yaml {e}"
            )

        return rules["rules"][0]  # type:ignore

    def hydrate_ruleset(ruleset_json: JsonObject) -> JsonObject:
        """
        Takes ruleset json object and expands the rules key list
        to be a list of rules instead of a list of rule identifiers
        """
        hydrated = []
        start = time.time()
        with concurrent.futures.ThreadPoolExecutor() as executor:
            futures = []
            for rule in ruleset_json["rules"]:
                futures.append(
                    executor.submit(
                        get_rule,
                        rule_id=rule["rule_id"],
                        version=rule["version"],
                    )
                )

            for future in concurrent.futures.as_completed(futures):
                try:
                    hydrated_rule = future.result()
                except Exception as e:
                    raise SemgrepError(f"Error resolving rule in ruleset: {e}")

                hydrated.append(hydrated_rule)
        logger.debug(
            f"Retrieved ruleset {ruleset_name}/{latest_version} in {time.time()- start}s"
        )
        return {"rules": hydrated}

    DOWNLOADING_MESSAGE = f"downloading config from {SEMGREP_CDN_BASE_URL}..."
    logger.info(DOWNLOADING_MESSAGE)
    logger.debug(f"Resolving latest version of {ruleset_name}")
    latest_version = get_latest_version(ruleset_name)
    ruleset_definition = get_ruleset(ruleset_name, latest_version)
    hydrated_ruleset = hydrate_ruleset(ruleset_definition)

    # Hack for now to get config in format expected by rest of codebase
    yaml = YAML()
    string_stream = StringIO()
    yaml.dump(hydrated_ruleset, string_stream)
    return parse_config_string(
        "remote-url",
        string_stream.getvalue(),
        filename=f"{ruleset_name[:20]}...",
    )


def is_pack_id(config_str: str) -> bool:
    return config_str[:2] == "p/"


def generate_config(fd: IO, lang: Optional[str], pattern: Optional[str]) -> None:
    config = DEFAULT_CONFIG

    if lang:
        config["rules"][0]["languages"] = [lang]
    if pattern:
        config["rules"][0]["pattern"] = pattern

    yaml = YAML()

    try:
        yaml.dump(config, fd)
        logger.info(f"Template config successfully written to {fd.name}")
    except Exception as e:
        raise SemgrepError(str(e))


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
