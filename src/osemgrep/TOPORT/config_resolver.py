from semgrep.app import auth
from semgrep.constants import CLI_RULE_ID
from semgrep.constants import DEFAULT_CONFIG_FILE
from semgrep.constants import DEFAULT_CONFIG_FOLDER
from semgrep.constants import DEFAULT_SEMGREP_APP_CONFIG_URL
from semgrep.constants import DEFAULT_SEMGREP_CONFIG_NAME
from semgrep.constants import ID_KEY
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import RULES_KEY
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import UNPARSEABLE_YAML_EXIT_CODE
from semgrep.rule import rule_without_metadata
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import EmptyYamlException
from semgrep.rule_lang import parse_yaml_preserve_spans
from semgrep.rule_lang import Span
from semgrep.state import get_state
from semgrep.util import is_config_suffix
from semgrep.util import is_url
from semgrep.util import terminal_wrap
from semgrep.util import with_color

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

class ConfigLoader:
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
        if config_str == "r2c":
            state.metrics.add_feature("config", "r2c")
            ...
        elif is_url(config_str):
            state.metrics.add_feature("config", "url")
            ...
        elif config_str == "policy":
            state.metrics.add_feature("config", "policy")
            self._config_path = url_for_policy(config_str)
        elif config_str == "supply-chain":
            state.metrics.add_feature("config", "sca")
            self._config_path = url_for_supply_chain()
        elif is_registry_id(config_str):
            state.metrics.add_feature("config", f"registry:prefix-{config_str[0]}")
            ...
        elif is_saved_snippet(config_str):
            state.metrics.add_feature("config", f"registry:snippet-id")
            ...
        elif config_str == "auto":
            state.metrics.add_feature("config", "auto")
            ...
        else:
            state.metrics.add_feature("config", "local")
            self._config_path = str(Path(config_str).expanduser())

        if self.is_registry_url():
            state.metrics.is_using_registry = True
            state.metrics.add_registry_url(self._config_path)

    def _nice_semgrep_url(self, url: str) -> str:
        """
        Alters semgrep.dev urls to let user
        click through to the nice display page instead
        of raw YAML.
        Replaces '/c/' in semgrep urls with '/'.
        """
        parsed = urlparse(url)
        if "semgrep.dev" in parsed.netloc and parsed.path.startswith("/c"):
            return url.replace("/c/", "/")
        return url


    def _make_config_request(self) -> str:
        app_session = get_state().app_session
        resp = app_session.get(
            self._config_path,
            headers={"Accept": "application/json", **self._extra_headers},
        )
        if resp.status_code == requests.codes.ok:
            try:
                rule_config = resp.json()["rule_config"]
                assert isinstance(rule_config, str)
                return rule_config
            except Exception:  # catch JSONDecodeError, AssertionError, etc.
                return resp.content.decode("utf-8", errors="replace")
        else:
            raise SemgrepError(
                f"bad status code: {resp.status_code} returned by config url: {self._config_path}"
            )

    def is_registry_url(self) -> bool:
        return self._origin == ConfigType.REGISTRY



def parse_config_files(
    loaded_config_infos: List[ConfigFile],
) -> Dict[str, YamlTree]:
    """
    Parse a list of config files into rules
    This assumes that config_id is set for local rules
    but is None for registry rules
    """
    config = {}
    for (config_id, contents, config_path) in loaded_config_infos:
        try:
            if not config_id:  # registry rules don't have config ids
                config_id = "remote-url"
                filename = f"{config_path[:20]}..."
            else:
                filename = config_path
            config.update(parse_config_string(config_id, contents, filename))
        except InvalidRuleSchemaError as e:
            if config_id == "remote-url":
                notice = f"\nRules downloaded from {config_path} failed to parse.\nThis is likely because rules have been added that use functionality introduced in later versions of semgrep.\nPlease upgrade to latest version of semgrep (see https://semgrep.dev/docs/upgrading/) and try again.\n"
                notice_color = with_color(Colors.red, notice, bold=True)
                logger.error(notice_color)
                raise e
            else:
                raise e
    return config


class ConfigPath:
    def __init__(self, config_str: str, project_url: Optional[str] = None) -> None:
        self._config_str = config_str
        self._project_url = project_url

    def resolve_config(self) -> Dict[str, YamlTree]:
        """resolves if config arg is a registry entry, a url, or a file, folder, or loads from defaults if None"""
        start_t = time.time()

        config = parse_config_files(
            ConfigLoader(self._config_str, self._project_url).load_config()
        )

        if config:
            logger.debug(f"loaded {len(config)} configs in {time.time() - start_t}")
        return config

    def __str__(self) -> str:
        # TODO return the resolved config_path
        return self._config_str


class Config:
    def __init__(self, valid_configs: Mapping[str, Sequence[Rule]]) -> None:
        """
        Handles parsing and validating of config files
        and exposes ability to get all rules in parsed config files
        """
        self.valid = valid_configs

    @classmethod
    def from_rules_yaml(cls, config: str) -> Tuple["Config", Sequence[SemgrepError]]:
        config_dict: Dict[str, YamlTree] = {}
        errors: List[SemgrepError] = []

        try:
            resolved_config_key = "semgrep-app-rules"
            config_dict.update(
                parse_config_string(resolved_config_key, config, filename=None)
            )
        except SemgrepError as e:
            errors.append(e)

        valid, parse_errors = cls._validate(config_dict)
        errors.extend(parse_errors)
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
    def _convert_config_id_to_prefix(config_id: str) -> str:
        at_path = Path(config_id)
        ...
        prefix = ".".join(at_path.parts[:-1]).lstrip("./").lstrip(".")
        if len(prefix):
            prefix += "."
        return prefix

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


def manual_config(xxx) -> Dict[str, YamlTree]:
    error_span = Span.from_string(
        f"Semgrep bug generating manual config {PLEASE_FILE_ISSUE_TEXT}", filename=None
    )
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




def load_default_config() -> Dict[str, YamlTree]:
    """
    Load config from DEFAULT_CONFIG_FILE or DEFAULT_CONFIG_FOLDER
    """
    default_file = Path(DEFAULT_CONFIG_FILE)
    default_folder = Path(DEFAULT_CONFIG_FOLDER)
    config_infos = []
    if default_file.exists():
        config_infos = [read_config_at_path(default_file)]
    elif default_folder.exists():
        config_infos = read_config_folder(default_folder, relative=True)
    return parse_config_files(config_infos)

def url_for_policy(config_str: str) -> str:
    """
    Return url to download a policy for a given repo_name

    For now uses envvar to know what repo_name is
    """
    deployment_id = auth.get_deployment_id()
    if deployment_id is None:
        raise SemgrepError(
            "Invalid API Key. Run `semgrep logout` and `semgrep login` again."
        )

    repo_name = os.environ.get("SEMGREP_REPO_NAME")
    if repo_name is None:
        raise SemgrepError(
            "Need to set env var SEMGREP_REPO_NAME to use `--config policy`"
        )

    env = get_state().env

    # The app considers anything that will not POST back to it to be a dry_run
    params = {
        "sca": False,
        "dry_run": True,
        "full_scan": True,
        "repo_name": repo_name,
        "semgrep_version": __VERSION__,
    }
    params_str = urlencode(params)
    return f"{env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{params_str}"


def url_for_supply_chain() -> str:
    env = get_state().env
    repo_name = os.environ.get("SEMGREP_REPO_NAME")

    # The app considers anything that will not POST back to it to be a dry_run
    params = {
        "sca": True,
        "dry_run": True,
        "full_scan": True,
        "repo_name": repo_name,
        "semgrep_version": __VERSION__,
    }
    params_str = urlencode(params)
    return f"{env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{params_str}"
