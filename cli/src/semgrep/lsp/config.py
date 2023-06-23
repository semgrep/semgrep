import json
import urllib
from typing import Any
from typing import Dict
from typing import List
from typing import Mapping
from typing import Optional
from typing import Union

from semgrep.app import auth
from semgrep.app.scans import ScanHandler
from semgrep.config_resolver import get_config
from semgrep.constants import DEFAULT_MAX_TARGET_SIZE
from semgrep.constants import DEFAULT_TIMEOUT
from semgrep.engine import EngineType
from semgrep.error import SemgrepError
from semgrep.meta import generate_meta_from_environment
from semgrep.meta import GitMeta
from semgrep.metrics import MetricsState
from semgrep.project import get_project_url
from semgrep.rule import Rule
from semgrep.semgrep_main import get_file_ignore
from semgrep.state import get_state
from semgrep.target_manager import TargetManager
from semgrep.types import JsonObject


class LSPConfig:
    def __init__(
        self, lsp_config: JsonObject, workspace_folders: List[JsonObject]
    ) -> None:
        lsp_config = dict(lsp_config)
        # Ensure that we have these config keys
        # This is easier than checking if they are None everywhere
        if "scan" not in lsp_config:
            lsp_config["scan"] = {}

        self._settings: Mapping[str, Mapping[str, Any]] = lsp_config
        self._workspace_folders: List[JsonObject] = workspace_folders
        self.rules: List[Rule] = []
        self.refresh_target_manager()

    # =====================
    # Semgrep scan settings
    # =====================

    @property
    def jobs(self) -> int:
        return self._settings["scan"].get("jobs", 1)

    @property
    def exclude(self) -> List[str]:
        return self._settings["scan"].get("exclude", [])

    @property
    def include(self) -> List[str]:
        return self._settings["scan"].get("include", [])

    @property
    def max_memory(self) -> int:
        return self._settings["scan"].get("maxMemory", 0)

    @property
    def max_target_bytes(self) -> int:
        return self._settings["scan"].get("maxTargetBytes", DEFAULT_MAX_TARGET_SIZE)

    @property
    def timeout(self) -> int:
        return self._settings["scan"].get("timeout", DEFAULT_TIMEOUT)

    @property
    def timeout_threshold(self) -> int:
        return self._settings["scan"].get("timeoutThreshold", 3)

    @property
    def project_url(self) -> Union[str, None]:
        return get_project_url()

    @property
    def ci_rules(self) -> List[Rule]:
        if len(self._workspace_folders) == 0:
            return []
        metadata_dict = {}
        try:
            metadata = generate_meta_from_environment(None)
            metadata_dict = metadata.to_dict()
        except SemgrepError:  # This fails if a user is logged in, and is not in a git repo
            return []
        scan_handler = ScanHandler(True)
        scan_handler.fetch_and_init_scan_config(metadata_dict)
        json_rules = json.loads(scan_handler.rules).get("rules", [])
        rules = [Rule.from_json(r) for r in json_rules]
        return rules

    @property
    def local_rules(self) -> List[Rule]:
        configs = []
        settings_configs = self._settings["scan"].get("configuration")

        if settings_configs is not None:
            configs.extend(settings_configs)
        if len(configs) == 0:
            return []

        configs_obj, _ = get_config(None, None, configs, project_url=self.project_url)
        rules = configs_obj.get_rules(True)
        return rules

    @property
    def auto_rules(self) -> List[Rule]:
        configs_obj, _ = get_config(None, None, ["auto"], project_url=self.project_url)
        rules = configs_obj.get_rules(True)
        return rules

    # =====================
    # Semgrep LSP settings
    # =====================
    @property
    def extension_metrics(self) -> Dict[str, Any]:
        extension_metrics = self._settings.get("extensionMetrics", {})
        if isinstance(extension_metrics, dict):
            return extension_metrics
        elif isinstance(extension_metrics, str):
            # for backwards compatibility, older versions of the extension may send metrics: "on" or "off"
            return {
                "enabled": extension_metrics == "on",
            }
        else:
            return {}

    @property
    def metrics_state(self) -> MetricsState:
        choice = self.extension_metrics.get("enabled", True)
        if choice:
            return MetricsState.ON
        else:
            return MetricsState.OFF

    # =====================
    # Useful properties
    # =====================

    @property
    def settings(self) -> JsonObject:
        return self._settings

    @property
    def head_commit(self) -> Optional[str]:
        return GitMeta().commit_sha

    @property
    def folders(self) -> List[str]:
        """All workspace folders by URI"""
        return [f["uri"] for f in self._workspace_folders]

    @property
    def folder_paths(self) -> List[str]:
        """All workspace folders by path"""
        folder_paths = []
        for f in self.folders:
            uri = urllib.parse.urlparse(f)
            target_name = urllib.request.url2pathname(uri.path)
            folder_paths.append(target_name)
        return folder_paths

    @property
    def token(self) -> Optional[str]:
        return auth.get_token()

    @property
    def logged_in(self) -> bool:
        return (
            self.token is not None
            and auth.get_deployment_from_token(self.token) is not None
        )

    @property
    def debug(self) -> bool:
        trace = self._settings.get("trace")
        debug = trace.get("server") if trace else None
        return debug is not None and debug == "verbose"

    @property
    def engine_type(self) -> EngineType:
        # Pro engine is too slow right now for editor integration
        return EngineType.OSS

    # =====================
    # Config management
    # =====================

    def refresh_target_manager(self) -> None:
        self.target_manager = TargetManager(
            includes=self.include,
            excludes=self.exclude,
            max_target_bytes=self.max_target_bytes,
            respect_git_ignore=True,
            file_ignore=get_file_ignore(),
            target_strings=self.folder_paths,
        )

    def refresh_rules(self) -> None:
        self.rules = []
        logged_in = self.logged_in

        self.rules.extend(self.local_rules)
        if logged_in:
            self.rules.extend(self.ci_rules)
        elif len(self.rules) == 0:
            self.rules.extend(self.auto_rules)

    def send_metrics(
        self, exit_code: Optional[int] = None, error: Optional[SemgrepError] = None
    ) -> None:
        state = get_state()
        metrics = state.metrics
        metrics.configure(self.metrics_state, None)
        metrics.add_engine_type(self.engine_type)
        metrics.add_project_url(self.project_url)
        metrics.add_token(self.token)
        if exit_code is not None:
            metrics.add_exit_code(exit_code)
        if error is not None:
            metrics.add_errors([error])
        extension_metrics = self.extension_metrics
        machine_id = extension_metrics.get("machineId")
        new_install = extension_metrics.get("isNewAppInstall")
        session_id = extension_metrics.get("sessionId")
        version = extension_metrics.get("extensionVersion")
        type = extension_metrics.get("extensionType")
        metrics.add_extension(machine_id, new_install, session_id, version, type)
        metrics.send()
