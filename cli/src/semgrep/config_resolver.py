#
# Copyright (c) 2020-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import concurrent.futures
import json
import os
import re
import time
from collections import OrderedDict
from dataclasses import dataclass
from dataclasses import field
from enum import auto
from enum import Enum
from io import StringIO
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Mapping
from typing import NamedTuple
from typing import Optional
from typing import Sequence
from typing import Tuple
from urllib.parse import urlencode

import click
import requests
from ruamel.yaml import YAML
from ruamel.yaml import YAMLError

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import __VERSION__
from semgrep import telemetry
from semgrep.app import auth
from semgrep.constants import CLI_RULE_ID
from semgrep.constants import Colors
from semgrep.constants import DEFAULT_SEMGREP_APP_CONFIG_URL
from semgrep.constants import ID_KEY
from semgrep.constants import MISSED_KEY
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import RULES_KEY
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import RULE_PARSE_FAILURE_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.error import UnknownLanguageError
from semgrep.error import UNPARSEABLE_YAML_EXIT_CODE
from semgrep.error_location import SourceTracker
from semgrep.error_location import Span
from semgrep.rule import Rule
from semgrep.rule import rule_without_metadata
from semgrep.rule_lang import EmptyYamlException
from semgrep.rule_lang import has_patterns_key
from semgrep.rule_lang import parse_json_and_filter_versions
from semgrep.rule_lang import parse_yaml_and_filter_versions
from semgrep.rule_lang import prepend_rule_path
from semgrep.rule_lang import project_depends_on
from semgrep.rule_lang import run_jsonschema_validation
from semgrep.rule_lang import validate_via_rpc_with_fallback
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.state import get_state
from semgrep.telemetry import scan_info_to_attrs
from semgrep.util import is_config_suffix
from semgrep.util import is_semgrep_url
from semgrep.util import is_url
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

AUTO_CONFIG_KEY = "auto"
AUTO_CONFIG_LOCATION = "c/auto"

CLOUD_PLATFORM_CONFIG_ID = "semgrep-app-rules"
REGISTRY_CONFIG_ID = "remote-registry"
NON_REGISTRY_REMOTE_CONFIG_ID = "remote-url"


@dataclass
class ParsedConfig:
    """Result of parsing a single config string into rules."""

    rules: List[Rule]
    errors: List[SemgrepError] = field(default_factory=list)
    missed_rule_count: int = 0


class ConfigFile(NamedTuple):
    config_id: Optional[str]  # None for remote files
    contents: str
    config_path: str


class ConfigType(Enum):
    # e.g p/<packname>, supply-chain, ...
    REGISTRY = auto()
    SEMGREP_CLOUD_PLATFORM = auto()
    # 3rd party config sites (e.g https://mywebsite.com/rules.yaml)
    REMOTE = auto()
    LOCAL = auto()


class ConfigLoader:
    _origin = ConfigType.LOCAL
    _config_path = ""
    _project_url = None

    def __init__(
        self,
        config_str: str,
        project_url: Optional[str] = None,
    ) -> None:
        """
        Mutates Metrics state!
        Takes a user's inputted config_str and transforms it into the appropriate
        path, checking whether the config string is a registry url or not. If it
        is, also set the appropriate Metrics flag
        """
        state = get_state()
        self._project_url = project_url
        self._origin = ConfigType.REMOTE
        self._supports_fallback_config = False

        if config_str == "r2c":
            # Hardcoded Registry rule pack
            state.metrics.add_feature("config", "r2c")
            state.metrics.is_using_registry = True
            self._config_path = "https://semgrep.dev/c/p/r2c"
            self._origin = ConfigType.REGISTRY
        elif is_url(config_str):
            # This could still be either a 3rd party REMOTE rule pack or a url
            # to semgrep.dev
            state.metrics.add_feature("config", "url")
            self._config_path = config_str
        elif is_product_names(config_str):
            self._origin = ConfigType.SEMGREP_CLOUD_PLATFORM
            add_metrics_for_products(config_str)
            self._config_path = config_str
            self._supports_fallback_config = True
        elif is_registry_id(config_str):
            state.metrics.add_feature("config", f"registry:prefix-{config_str[0]}")
            state.metrics.is_using_registry = True
            self._config_path = registry_id_to_url(config_str)
        elif config_str == AUTO_CONFIG_KEY:
            state.metrics.add_feature("config", "auto")
            state.metrics.is_using_registry = True
            self._config_path = f"{state.env.semgrep_url}/{AUTO_CONFIG_LOCATION}"
        else:
            state.metrics.add_feature("config", "local")
            self._origin = ConfigType.LOCAL
            self._config_path = str(Path(config_str).expanduser())

        # We still have to modify metrics metadata in case the config was a
        # registry URL
        if is_semgrep_url(config_str, state.env.semgrep_url):
            state.metrics.is_using_registry = True
            state.metrics.add_registry_url(self._config_path)
            self._origin = ConfigType.REGISTRY

    @classmethod
    def includes_remote_config(cls, configs: Optional[Sequence[str]]) -> bool:
        """
        Returns True if any of the configs are remote
        """
        if not configs:
            return False

        return any(
            config == AUTO_CONFIG_KEY
            or config == "r2c"
            or is_product_names(config)
            or is_registry_id(config)
            or is_url(config)
            for config in configs
        )

    def load_config(self) -> List[ConfigFile]:
        """
        Loads a config based on self's state.
        A config path produces a list of ConfigFiles because
        it may be a path to a folders of configs, each of
        which produces a file
        """
        if self._origin == ConfigType.REGISTRY or self._origin == ConfigType.REMOTE:
            return [self._download_config()]
        elif self._origin == ConfigType.SEMGREP_CLOUD_PLATFORM:
            return [self._fetch_semgrep_cloud_platform_scan_config()]
        else:
            return self._load_config_from_local_path()

    def _download_config(self) -> ConfigFile:
        """
        Download a configuration from semgrep.dev
        """
        try:
            return self._download_config_from_url(self._config_path)
        except Exception:
            if self._supports_fallback_config:
                try:
                    fallback_url = re.sub(
                        r"^[^?]*",  # replace everything but query params
                        f"{get_state().env.fail_open_url}/config",
                        self._config_path,
                    )
                    return self._download_config_from_url(fallback_url)
                except Exception:
                    pass

            raise  # error from first fetch

    def _download_config_from_url(self, url: str) -> ConfigFile:
        app_session = get_state().app_session
        logger.debug("Downloading config from %s", url)
        error = f"Failed to download configuration from {url}"
        try:
            resp = app_session.get(url, headers={"Accept": "application/json"})
            if resp.status_code == requests.codes.ok:
                try:
                    # can be either rule_config or rules depending on the endpoint we hit
                    json_config = resp.json()
                    rule_config = (
                        json_config
                        if "rules" in json_config
                        else json_config["rule_config"]
                    )

                    # TODO don't dump and re-load json
                    # The backend wants to return native json, but we support a json string here too
                    config_str = (
                        rule_config
                        if isinstance(rule_config, str)
                        else json.dumps(rule_config)
                    )

                    return ConfigFile(None, config_str, url)
                except Exception as ex:
                    # catch JSONDecodeError, AssertionError, etc. is this needed?
                    logger.debug("Failed to decode JSON: %s", repr(ex))
                    return ConfigFile(
                        None, resp.content.decode("utf-8", errors="replace"), url
                    )
                finally:
                    logger.debug(f"Downloaded config from %s", url)

            error += f" HTTP {resp.status_code}."
        except requests.exceptions.RetryError as ex:
            error += f" Failed after multiple attempts ({ex.args[0].reason})"

        logger.debug(error)  # since the raised exception may be caught and suppressed
        raise SemgrepError(error)

    def _load_config_from_local_path(self) -> List[ConfigFile]:
        """
        Return config file(s) as dictionary object
        """
        loc = Path(self._config_path)

        logger.debug(f"Loading local config from {loc}")
        if loc.exists():
            if loc.is_file():
                config = [read_config_at_path(loc)]
            elif loc.is_dir():
                config = read_config_folder(loc)
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

    def _project_metadata_for_standalone_scan(
        self, require_repo_name: bool
    ) -> out.ProjectMetadata:
        repo_name = os.environ.get("SEMGREP_REPO_NAME")
        repo_display_name = os.environ.get("SEMGREP_REPO_DISPLAY_NAME")
        project_id = os.environ.get("SEMGREP_PROJECT_ID")
        if repo_display_name:
            if project_id:
                raise SemgrepError(
                    "The environment variables SEMGREP_PROJECT_ID and SEMGREP_REPO_DISPLAY_NAME cannot both be set at the same time."
                )
        else:
            repo_display_name = repo_name

        if repo_name is None:
            if require_repo_name:
                raise SemgrepError(
                    f"Need to set env var SEMGREP_REPO_NAME to use `--config {self._config_path}`"
                )
            else:
                repo_name = "unknown"

        return out.ProjectMetadata(
            scan_environment="semgrep-scan",
            repository=repo_name,
            repo_display_name=repo_display_name,
            repo_url=None,
            branch=None,
            commit=None,
            commit_title=None,
            commit_author_email=None,
            commit_author_name=None,
            commit_author_username=None,
            commit_author_image_url=None,
            ci_job_url=None,
            on="unknown",
            pull_request_author_username=None,
            pull_request_author_image_url=None,
            pull_request_id=None,
            pull_request_title=None,
            is_full_scan=True,  # always true for standalone scan
            project_id=project_id,
        )

    def _fetch_semgrep_cloud_platform_scan_config(self) -> ConfigFile:
        """
        Download a configuration from semgrep.dev using new /api/cli/scans endpoint
        """
        state = get_state()

        products = [
            out.Product.from_json(PRODUCT_NAMES[p])
            for p in self._config_path.split(",")
        ]

        # Require SEMGREP_REPO_NAME env var if SAST or Secrets are requested
        require_repo_name = any(
            p.value in [out.SAST(), out.Secrets()] for p in products
        )

        request = out.ScanRequest(
            scan_metadata=out.ScanMetadata(
                cli_version=out.Version(__VERSION__),
                unique_id=out.Uuid(str(state.local_scan_id)),
                requested_products=products,
                dry_run=True,  # semgrep scan never submits findings, so always a dry run
            ),
            project_metadata=self._project_metadata_for_standalone_scan(
                require_repo_name
            ),
        )

        try:
            return self._download_semgrep_cloud_platform_scan_config(request)
        except Exception:
            if self._supports_fallback_config:
                try:
                    return self._download_semgrep_cloud_platform_fallback_scan_config()
                except Exception:
                    pass

            raise  # error from first fetch

    def _download_semgrep_cloud_platform_scan_config(
        self, request: out.ScanRequest
    ) -> ConfigFile:
        state = get_state()
        url = f"{state.env.semgrep_url}/api/cli/scans"
        logger.debug("Downloading config from %s", url)
        error = f"Failed to download configuration from {url}"
        try:
            response = state.app_session.post(
                f"{state.env.semgrep_url}/api/cli/scans",
                json=request.to_json(),
            )

            if response.status_code == requests.codes.unauthorized:
                raise SemgrepError(
                    "Invalid API Key. Run `semgrep logout` and `semgrep login` again.",
                    code=INVALID_API_KEY_EXIT_CODE,
                )

            try:
                response.raise_for_status()
            except requests.RequestException:
                raise Exception(
                    f"API server at {state.env.semgrep_url} returned this error: {response.text}"
                )

            scan_response = out.ScanResponse.from_json(response.json())
            get_state().telemetry.add_resource_attrs(
                scan_info_to_attrs(scan_response.info)
            )
            return ConfigFile(None, scan_response.config.rules.to_json_string(), url)

        except requests.exceptions.RetryError as ex:
            error += f" Failed after multiple attempts ({ex.args[0].reason})"

            logger.debug(
                error
            )  # since the raised exception may be caught and suppressed

            raise SemgrepError(error)

    def _download_semgrep_cloud_platform_fallback_scan_config(self) -> ConfigFile:
        """
        This function decides what fallback url to call if the semgrep cloud platform
        scan config endpoint fails

        ! This will manually rebuild the url until we have a better solution
        """
        fallback_url = None

        if is_code(self._config_path):
            fallback_url = url_for_code()
        elif is_supply_chain(self._config_path):
            fallback_url = url_for_supply_chain()
        elif is_secrets(self._config_path):
            fallback_url = url_for_secrets()
        elif is_policy_id(self._config_path):
            fallback_url = url_for_policy()
        else:
            raise

        fallback_url = re.sub(
            r"^[^?]*",  # replace everything but query params
            f"{get_state().env.fail_open_url}/config",
            fallback_url,
        )

        return self._download_config_from_url(fallback_url)


def read_config_at_path(loc: Path, base_path: Optional[Path] = None) -> ConfigFile:
    """
    Assumes file at loc exists
    """
    config_id = str(loc)
    if base_path:
        config_id = str(loc).replace(str(base_path), "")

    return ConfigFile(config_id, loc.read_text(), str(loc))


def read_config_folder(loc: Path, relative: bool = False) -> List[ConfigFile]:
    configs = []
    for l in loc.rglob("*"):
        if is_config_suffix(l) and l.is_file():
            configs.append(read_config_at_path(l, loc if relative else None))
    return configs


@telemetry.trace()
def parse_config_files(
    loaded_config_infos: List[ConfigFile],
    force_jsonschema: bool = False,
    no_python_schema_validation: bool = False,
) -> Tuple[Dict[str, List[Rule]], List[SemgrepError], int]:
    """
    Parse a list of config files into rules.
    This assumes that config_id is set for local rules
    but is None for registry rules.
    Returns (config_id -> rules, errors, missed_rule_count).
    """
    config: Dict[str, List[Rule]] = {}
    errors: List[SemgrepError] = []
    missed_rule_count = 0
    future_to_config_id_and_path: Dict[
        concurrent.futures.Future[ParsedConfig],
        Tuple[str, str],
    ] = {}

    ctx = click.get_current_context(silent=True)
    configured_semgrep_url = None
    if ctx is not None:
        with ctx.scope():
            configured_semgrep_url = get_state().env.semgrep_url

    def context_aware_parse_config_string(
        *args: Any,
        **kwargs: Any,
    ) -> ParsedConfig:
        """
        Wrapper to propagate Click context to ThreadPoolExecutor threads

        Click context is not available in the ThreadPoolExecutor threads,
        so we need to propagate it manually.

        See https://pocoo-click.readthedocs.io/en/latest/advanced/#global-context-access
        """
        if ctx is not None:
            with ctx.scope():
                return parse_config_string(*args, **kwargs)
        else:
            return parse_config_string(*args, **kwargs)

    # !WARNING(sal): ThreadPoolExecutor landmine!
    # Click's context is thread-local. If you use ThreadPoolExecutor,
    # any Click-dependent operations will fail or behave unexpectedly
    # in the worker threads unless the context is explicitly propagated
    # using `with ctx.scope():` as shown in `context_aware_parse_config_string`.
    #
    # TODO(sal): Abstract the use of threadpool with a context-aware wrapper
    # to prevent this issue from recurring.
    with concurrent.futures.ThreadPoolExecutor() as executor:
        for config_id, contents, config_path in loaded_config_infos:
            if not config_id:  # registry rules don't have config ids
                # Note: we must disambiguate registry sourced remote rules from
                # non-registry sourced ones for security purposes. Namely, we
                # want to avoid running postprocessors from untrusted remote
                # sources (unless a local flag disabiling the relevant check is
                # used).
                config_id = (
                    REGISTRY_CONFIG_ID
                    if is_semgrep_url(config_path, configured_semgrep_url)
                    else NON_REGISTRY_REMOTE_CONFIG_ID
                )
                filename = f"{config_path[:20]}..."
            else:
                filename = config_path
            validation_future = executor.submit(
                context_aware_parse_config_string,
                config_id,
                contents,
                filename,
                no_python_schema_validation=no_python_schema_validation,
                force_jsonschema=force_jsonschema,
            )
            future_to_config_id_and_path[validation_future] = config_id, config_path
        for future in concurrent.futures.as_completed(
            future_to_config_id_and_path
            # Timeout temporarily removed as part of SAF-2099.  Once completed,
            # reintroduce and possibly readjust.
            # , timeout=5 * 60
        ):
            config_id, config_path = future_to_config_id_and_path[future]
            try:
                result = future.result()
                config[config_id] = result.rules
                errors.extend(result.errors)
                missed_rule_count += result.missed_rule_count
            except InvalidRuleSchemaError as e:
                if (
                    config_id == REGISTRY_CONFIG_ID
                    or config_id == NON_REGISTRY_REMOTE_CONFIG_ID
                ):
                    notice = f"\nRules downloaded from {config_path} failed to parse.\nThis is likely because rules have been added that use functionality introduced in later versions of semgrep.\nPlease upgrade to latest version of semgrep (see https://semgrep.dev/docs/upgrading/) and try again.\n"
                    notice_color = with_color(Colors.red, notice, bold=True)
                    logger.error(notice_color)
                    raise e
                else:
                    raise e
    return config, errors, missed_rule_count


@telemetry.trace()
def resolve_config(
    config_str: str,
    project_url: Optional[str] = None,
    force_jsonschema: bool = False,
    no_python_schema_validation: bool = False,
) -> Tuple[Dict[str, List[Rule]], List[SemgrepError], int]:
    """resolves if config arg is a registry entry, a url, or a file, folder, or loads from defaults if None"""
    start_t = time.time()
    config_loader = ConfigLoader(config_str, project_url)
    config, errors, missed_rule_count = parse_config_files(
        config_loader.load_config(),
        force_jsonschema=force_jsonschema,
        no_python_schema_validation=no_python_schema_validation,
    )
    if config:
        logger.debug(
            f"loaded {len(config)} configs in {time.time() - start_t} with {len(errors)} errors and {missed_rule_count} missed rules"
        )
    return config, errors, missed_rule_count


def _apply_secrets_security_check(rule: Rule, config_id: str) -> None:
    """SECURITY: Set metadata for non-registry secrets rules so that
    postprocessors are not run. The default requirement is that the rule
    be served from the pro origin. Without this, local rules could use
    postprocessors which may exfiltrate data from source code."""
    if (
        isinstance(rule.product.value, out.Secrets)
        # In some instances we might append config_id with `_{i}` where
        # i is an integer
        and not config_id.startswith(REGISTRY_CONFIG_ID)
        and not config_id.startswith(CLOUD_PLATFORM_CONFIG_ID)
    ):
        rule.metadata.get("semgrep.dev", {}).get("rule", {})["origin"] = "local"


class Config:
    def __init__(
        self,
        valid_configs: Mapping[str, Sequence[Rule]],
        *,
        # TODO: Use an array of semgrep_output_v1.Product instead of booleans flags for secrets, code, and supply chain
        with_code_rules: bool = False,
        with_supply_chain: bool = False,
        with_secrets: bool = False,
        missed_rule_count: int = 0,
    ) -> None:
        """
        Handles parsing and validating of config files
        and exposes ability to get all rules in parsed config files
        """
        self.valid = valid_configs
        self.with_code_rules = with_code_rules
        self.with_supply_chain = with_supply_chain
        self.with_secrets = with_secrets
        self.missed_rule_count = missed_rule_count

    @classmethod
    @telemetry.trace()
    def from_pattern_lang(
        cls, pattern: str, lang: str, replacement: Optional[str] = None
    ) -> Tuple["Config", List[SemgrepError]]:
        config_dict = manual_config(pattern, lang, replacement)
        # manual_config returns Dict[str, YamlTree]; do inline Rule construction
        # for this synthetic rule (no schema validation needed)
        rules: List[Rule] = []
        errors: List[SemgrepError] = []
        for _config_id, yaml_tree in config_dict.items():
            for rule_yaml in yaml_tree.value[RULES_KEY].value:
                try:
                    rules.append(Rule.from_yamltree(rule_yaml))
                except InvalidRuleSchemaError as ex:
                    errors.append(ex)
        return cls({"manual": rules}), errors

    @classmethod
    @telemetry.trace()
    def from_rules_string(
        cls,
        raw_rules: str,
        force_jsonschema: bool = False,
        no_python_schema_validation: bool = False,
        defer_core_rule_validation: bool = False,
    ) -> Tuple["Config", List[SemgrepError]]:
        if not raw_rules:
            return cls({}), [
                SemgrepError(
                    "Empty rule string cannot be loaded",
                    code=RULE_PARSE_FAILURE_EXIT_CODE,
                )
            ]

        try:
            result = parse_config_string(
                CLOUD_PLATFORM_CONFIG_ID,
                raw_rules,
                "rules.json",
                force_jsonschema=force_jsonschema,
                no_python_schema_validation=no_python_schema_validation,
                defer_core_rule_validation=defer_core_rule_validation,
            )
            return cls({CLOUD_PLATFORM_CONFIG_ID: result.rules}), result.errors
        except SemgrepError as e:
            return cls({}), [e]

    @classmethod
    @telemetry.trace()
    def from_config_list(
        cls,
        configs: Sequence[str],
        project_url: Optional[str],
        force_jsonschema: bool = False,
        no_python_schema_validation: bool = False,
    ) -> Tuple["Config", List[SemgrepError]]:
        """
        Takes in list of files/directories and returns Config object as well as
        list of errors parsing said config files

        If empty list is passed, tries to read config file at default locations
        """
        config_dict: Dict[str, List[Rule]] = {}
        errors: List[SemgrepError] = []
        missed_rule_count = 0
        with_supply_chain = False
        with_code_rules = False
        with_secrets = False

        for i, config in enumerate(configs):
            try:
                # Patch config_id to fix
                # https://github.com/semgrep/semgrep/issues/1912
                resolved_config, config_errors, missed = resolve_config(
                    config,
                    project_url,
                    force_jsonschema=force_jsonschema,
                    no_python_schema_validation=no_python_schema_validation,
                )
                errors.extend(config_errors)
                missed_rule_count += missed
                if not resolved_config:
                    logger.verbose(f"Could not resolve config for {config}. Skipping.")
                    continue

                with_code_rules = with_code_rules or not is_supply_chain(config)
                with_supply_chain = with_supply_chain or is_supply_chain(config)
                with_secrets = with_secrets or is_secrets(config)

                for resolved_config_key, rules in resolved_config.items():
                    patched_key = f"{resolved_config_key}_{i}"
                    for rule in rules:
                        _apply_secrets_security_check(rule, resolved_config_key)
                    config_dict[patched_key] = rules
            except UnknownLanguageError:
                # UnknownLanguageError must propagate so the CLI exits with
                # INVALID_LANGUAGE_EXIT_CODE (8). Other SemgrepError subtypes
                # are appended to `errors` and trigger a generic
                # MISSING_CONFIG_EXIT_CODE (7) via sanity_check_resolved_config.
                raise
            except SemgrepError as e:
                errors.append(e)

        return (
            cls(
                config_dict,
                with_code_rules=with_code_rules,
                with_supply_chain=with_supply_chain,
                with_secrets=with_secrets,
                missed_rule_count=missed_rule_count,
            ),
            errors,
        )

    @telemetry.trace()
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

        # Deduplicate rules, ignoring metadata, which is not displayed
        # in the result.
        # Deduplication occurs from left to right so as to have the same
        # behavior as osemgrep i.e. the first occurrence of each rule
        # if preserved and subsequent occurrences are discarded.
        return list(
            reversed(
                list(
                    OrderedDict(
                        (rule_without_metadata(rule), rule)
                        for rules in reversed(configs.values())
                        for rule in reversed(rules)
                    ).values()
                )
            )
        )

    @staticmethod
    def _rename_rule_ids(valid_configs: Mapping[str, Sequence[Rule]]) -> None:
        for config_id, rules in valid_configs.items():
            for rule in rules:
                rule.rename_id(prepend_rule_path(config_id, rule.id))


def validate_single_rule(rule_yaml: YamlTree[YamlMap]) -> Rule:
    """
    Validate that a rule dictionary contains all necessary keys
    and can be correctly parsed.
    """
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
        "severity": out.Error().to_json(),
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
            # This used to raise a SemgrepError but it was resulting
            # in a silent 'exit 2'. Raising a generic Exception
            # avoids the silence.
            # TODO: fix the problem at the root: the SemgrepError exception
            #  shouldn't be silent.
            raise Exception(
                f"Detected Docker environment without a code volume, please include '-v \"${{PWD}}:{env.src_directory}\"'"
            )
        else:
            os.chdir(env.src_directory)


def indent(msg: str) -> str:
    return "\n".join(["\t" + line for line in msg.splitlines()])


def _create_rules(
    data: Dict[str, Any],
    config_id: str,
) -> Tuple[List[Rule], List[SemgrepError]]:
    """Extract and construct Rule objects from a parsed config dict."""
    errors: List[SemgrepError] = []
    rules: List[Rule] = []
    raw_rules = data.get(RULES_KEY)
    if raw_rules is None:
        raise InvalidRuleSchemaError(
            short_msg="missing keys",
            long_msg=f"{config_id} is missing `{RULES_KEY}` as top-level key",
            spans=[],
        )
    for raw_rule in raw_rules:
        rules.append(Rule.from_json(raw_rule))
    return rules, errors


@telemetry.trace()
def parse_config_string(
    config_id: str,
    contents: str,
    filename: Optional[str],
    force_jsonschema: bool = False,
    no_python_schema_validation: bool = False,
    defer_core_rule_validation: bool = False,
) -> ParsedConfig:
    """Parse config contents (JSON or YAML), validate, and create Rule objects.

    Pipeline: parse → version filter → drop SCA-only rules → validate core-
    bound rules → build Rule objects. Validation runs either via semgrep-
    core's RPC (preferred) or Python's jsonschema (fallback or when forced).
    SCA-only rules (r2c-internal-project-depends-on with no pattern keys)
    are excluded from validation because they don't conform to the semgrep-
    core rule schema.

    When defer_core_rule_validation is set, both the SCA-only filter and the
    schema validation step are skipped here; validation is left to the scan
    subprocess that consumes the same rules.
    """
    if not contents:
        raise SemgrepError(
            f"Empty configuration file {filename}", code=UNPARSEABLE_YAML_EXIT_CODE
        )

    errors: List[SemgrepError] = []

    # 1. PARSE + VERSION FILTER (always — Python needs parsed rules for Rule()).
    #
    # We check if it's JSON or not because JSON parsing on the Python side is
    # much faster than YAML parsing.
    is_json = contents.lstrip().startswith("{")
    # Needed so we can reconstruct error messages with accurate line numbers.
    # Only populated for YAML since we use the jsonschema validator even if we
    # start with yaml, since that's much faster
    yaml_tree: Optional[YamlTree] = None
    try:
        if is_json:
            logger.debug(f"Parsing {filename} as JSON")
            data, ver_errors = parse_json_and_filter_versions(contents, filename)
            errors.extend(ver_errors)
        else:
            logger.debug(f"Parsing {filename} as YAML")
            yaml_tree, parse_errors = parse_yaml_and_filter_versions(contents, filename)
            errors.extend(parse_errors)
            # Guard against YAML that parses to a non-mapping (e.g. a bare
            # string or list). Downstream code assumes a mapping at the top.
            if not isinstance(yaml_tree.value, YamlMap):
                raise SemgrepError(f"{config_id} was not a mapping")
            data = yaml_tree.unroll_dict()
    except EmptyYamlException:
        raise SemgrepError(
            f"Empty configuration file {filename}", code=UNPARSEABLE_YAML_EXIT_CODE
        )
    except json.decoder.JSONDecodeError as je:
        raise SemgrepError(
            f"Invalid JSON file {config_id}:\n{indent(str(je))}",
            code=UNPARSEABLE_YAML_EXIT_CODE,
        )
    except YAMLError as se:
        raise SemgrepError(
            f"Invalid YAML file {config_id}:\n{indent(str(se))}",
            code=UNPARSEABLE_YAML_EXIT_CODE,
        )

    if defer_core_rule_validation:
        logger.debug(f"Skipping semgrep-core RPC rule validation for {filename}")
    else:
        # 2. FILTER: drop SCA-only rules (r2c-internal-project-depends-on with
        #    no pattern keys). Those rules are handled by the Python Supply
        #    Chain pipeline and don't conform to the semgrep-core schema, so
        #    they must not reach validation. Only re-serialize `contents` when
        #    something was actually filtered out; otherwise pass the original
        #    through so semgrep-core sees accurate line numbers.
        all_rule_dicts = data.get(RULES_KEY, [])
        core_rule_dicts = [
            r
            for r in all_rule_dicts
            if has_patterns_key(r) or not project_depends_on(r)
        ]
        core_rules_data = {RULES_KEY: core_rule_dicts}
        if is_json:
            contents = json.dumps(core_rules_data, ensure_ascii=False)
        else:
            stream = StringIO()
            YAML().dump(core_rules_data, stream)
            contents = stream.getvalue()

        # 3. VALIDATE: decide path up-front. RPC gets the (possibly filtered)
        #    contents with a format-matching suffix; jsonschema fallback builds
        #    the YAML span bridge from yaml_tree. no_python_schema_validation
        #    is a hard constraint — when set, we must not fall back to
        #    jsonschema even if force_jsonschema is also set.
        source_hash = SourceTracker.add_source(contents)
        if force_jsonschema and not no_python_schema_validation:
            run_jsonschema_validation(core_rules_data, yaml_tree, source_hash, filename)
        else:
            validate_via_rpc_with_fallback(
                contents,
                is_json,
                core_rules_data,
                yaml_tree,
                source_hash,
                filename,
                no_python_schema_validation,
            )

    # 4. CREATE RULES
    rules, rule_errors = _create_rules(data, config_id)
    errors.extend(rule_errors)

    missed_rule_count = int(data.get(MISSED_KEY, 0))
    return ParsedConfig(rules=rules, errors=errors, missed_rule_count=missed_rule_count)


def is_registry_id(config_str: str) -> bool:
    """
    Starts with r/, p/, s/ for registry, pack, and snippet respectively
    """
    return config_str[:2] in {"r/", "p/", "s/"}


def registry_id_to_url(registry_id: str) -> str:
    """
    Convert from registry_id to semgrep.dev url
    """
    env = get_state().env
    return f"{env.semgrep_url}/c/{registry_id}"


def url_for_policy() -> str:
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
        "is_secrets_scan": False,
        "dry_run": True,
        "full_scan": True,
        "repo_name": repo_name,
        "semgrep_version": __VERSION__,
    }
    params_str = urlencode(params)
    return f"{env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{params_str}"


PRODUCT_NAMES = {
    "code": "sast",
    "policy": "sast",  # although policy isn't a product, it's effectively an alias for code
    "secrets": "secrets",
    "supply-chain": "sca",
}


def is_product_names(config_str: str) -> bool:
    allowed = set(PRODUCT_NAMES.keys())
    names = set(config_str.split(","))
    return names <= allowed


def add_metrics_for_products(config_str: str) -> None:
    state = get_state()
    for product_name in config_str.split(","):
        if is_policy_id(product_name):
            state.metrics.add_feature("config", "policy")
        else:
            state.metrics.add_feature("config", PRODUCT_NAMES[product_name])


def is_policy_id(config_str: str) -> bool:
    return config_str == "policy"


def legacy_url_for_scan(extra_params: Optional[dict] = None) -> str:
    """
    Generates a legacy scan url (api/agent/deployments/scans/config) to
    fetch a scan configuration.
    """
    env = get_state().env

    # Common parameters for all scans
    # - The app considers anything that will not POST back to it to be a dry_run
    params = {
        "dry_run": True,
        "full_scan": True,
        "semgrep_version": __VERSION__,
    }

    if extra_params:
        params.update(extra_params)

    if "SEMGREP_REPO_NAME" in os.environ:
        params["repo_name"] = os.environ.get("SEMGREP_REPO_NAME")

    params_str = urlencode(params)
    return f"{env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{params_str}"


def url_for_code() -> str:
    return legacy_url_for_scan()


def url_for_supply_chain() -> str:
    return legacy_url_for_scan({"sca": True})


def url_for_secrets() -> str:
    return legacy_url_for_scan({"is_secrets_scan": True})


def is_code(config_str: str) -> bool:
    return config_str == "code"


def is_supply_chain(config_str: str) -> bool:
    return config_str == "supply-chain"


def is_secrets(config_str: str) -> bool:
    return config_str == "secrets"


def is_pack_id(config_str: str) -> bool:
    return config_str[:2] == "p/"
