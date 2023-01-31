import glob
import urllib
from functools import partial
from pathlib import Path
from typing import Any
from typing import Callable
from typing import Collection
from typing import Dict
from typing import List
from typing import Mapping
from typing import Optional
from typing import Set
from typing import Tuple
from typing import Union

import semgrep.commands.ci
import semgrep.semgrep_main
from semgrep.app.scans import ScanHandler
from semgrep.config_resolver import get_config
from semgrep.constants import EngineType
from semgrep.constants import OutputFormat
from semgrep.constants import RuleSeverity
from semgrep.error import SemgrepError
from semgrep.meta import generate_meta_from_environment
from semgrep.metrics import MetricsState
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.output_extra import OutputExtra
from semgrep.profile_manager import ProfileManager
from semgrep.project import get_project_url
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatchMap
from semgrep.state import get_state
from semgrep.target_manager import FileTargetingLog
from semgrep.target_manager import TargetManager
from semgrep.types import JsonObject
from semgrep.util import git_check_output


class LSPConfig:
    def __init__(
        self, lsp_config: JsonObject, workspace_folders: List[JsonObject]
    ) -> None:
        lsp_config = dict(lsp_config)
        # Ensure that we have these config keys
        # This is easier than checking if they are None everywhere
        if "scan" not in lsp_config:
            lsp_config["scan"] = {}
        if "lsp" not in lsp_config:
            lsp_config["lsp"] = {}
        self._settings: Mapping[str, Mapping[str, Any]] = lsp_config
        self._workspace_folders = workspace_folders
        self.update_workspace(added=None, removed=None)
        self._update_target_manager()

    # =====================
    # Semgrep scan settings
    # =====================

    @property
    def configs(self) -> List[str]:
        """Get all valid configs to run semgrep on for the current workspace"""
        configs = []
        settings_configs = self._settings["scan"].get("configuration")
        if settings_configs is not None:
            configs.extend(settings_configs)
        # Should do something with CI here
        # Configs we have autodetected from the workspace folders
        if self.autodetect_configs:
            configs.extend(self._workspace_configs)
        if len(configs) > 0:
            return configs
        else:
            return ["auto"]

    @property
    def baseline_commit(self) -> Optional[str]:
        return self._settings["scan"].get("baselineCommit")

    @property
    def severity(self) -> List[str]:
        return self._settings["scan"].get("severity", ["INFO", "WARNING", "ERROR"])

    @property
    def exclude(self) -> List[str]:
        return self._settings["scan"].get("exclude", [])

    @property
    def include(self) -> List[str]:
        return self._settings["scan"].get("include", [])

    @property
    def jobs(self) -> int:
        return self._settings["scan"].get("jobs", 1)

    @property
    def configurationSource(self) -> List[str]:
        return self._settings["scan"].get("configurationSource", ["file"])

    @property
    def disable_nosem(self) -> bool:
        return self._settings["scan"].get("disableNoSem", False)

    @property
    def max_memory(self) -> int:
        return self._settings["scan"].get("maxMemory", 0)

    @property
    def max_target_bytes(self) -> int:
        return self._settings["scan"].get("maxTargetBytes", 0)

    @property
    def timeout_threshold(self) -> int:
        return self._settings["scan"].get("timeoutThreshold", 0)

    @property
    def use_git_ignore(self) -> bool:
        return self._settings["scan"].get("useGitIgnore", True)

    @property
    def project_url(self) -> Union[str, None]:
        return get_project_url()

    @property
    def scan_url(self) -> str:
        scan_handler = ScanHandler(True)
        metadata = generate_meta_from_environment(self.baseline_commit)
        state = get_state()
        to_server = (
            ""
            if state.env.semgrep_url == "https://semgrep.dev"
            else f" to {state.env.semgrep_url}"
        )
        metadata_dict = metadata.to_dict()
        scan_handler.fetch_and_init_scan_config(metadata_dict)
        return scan_handler.rules

    # =====================
    # Semgrep LSP settings
    # =====================
    @property
    def watch_open_files(self) -> bool:
        return self._settings["lsp"].get("watchOpenFiles", True)

    @property
    def watch_workspace(self) -> bool:
        return self._settings["lsp"].get("watchWorkspace", True)

    @property
    def watch_configs(self) -> bool:
        return self._settings["lsp"].get("watchConfigs", True)

    @property
    def autodetect_configs(self) -> bool:
        return self._settings["lsp"].get("autodetectConfigs", True)

    @property
    def ci_enabled(self) -> bool:
        return self._settings["lsp"].get("ciEnabled", True)

    @property
    def metrics(self) -> MetricsState:
        choice = self._settings.get("metrics", "on")
        if choice == "on":
            return MetricsState.ON
        elif choice == "off":
            return MetricsState.OFF
        else:
            return MetricsState.AUTO

    # =====================
    # Useful properties
    # =====================

    @property
    def settings(self) -> JsonObject:
        return self._settings

    def _rules(self, configs: List[str]) -> List[Rule]:
        configs_obj, _ = get_config(None, None, configs, project_url=self.project_url)
        all_rules = configs_obj.get_rules(True)
        filtered_rules = [
            rule for rule in all_rules if rule.severity.value in self.severity
        ]
        return filtered_rules

    @property
    def workspace_rules(self) -> List[Rule]:
        """Get all local rules we're running"""
        return self._rules(self.configs)

    @property
    def ci_rules(self) -> Optional[List[Rule]]:
        if not self.logged_in:
            return None
        return self._rules([self.scan_url])

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
        return get_state().app_session.token

    @property
    def logged_in(self) -> bool:
        return self.token is not None

    @property
    def is_git_dir(self) -> bool:
        try:
            for f in self.folder_paths:
                git_check_output(["git", "-C", f, "rev-parse"])
            return True
        except Exception:
            return False

    def _scanner(
        self, configs: List[str]
    ) -> Callable[
        ...,
        Tuple[
            RuleMatchMap,
            List[SemgrepError],
            Set[Path],
            FileTargetingLog,
            List[Rule],
            ProfileManager,
            OutputExtra,
            Collection[RuleSeverity],
            Dict[str, int],
        ],
    ]:
        """Generate a scanner according to the config"""
        output_settings = OutputSettings(output_format=OutputFormat.JSON)
        output_handler = OutputHandler(output_settings)
        get_state().metrics.configure(self.metrics, None)
        return partial(
            semgrep.semgrep_main.main,
            engine=EngineType.OSS,
            configs=configs,
            severity=self.severity,
            exclude=self.exclude,
            include=self.include,
            output_handler=output_handler,
            dryrun=True,
            jobs=self.jobs,
            pattern=None,
            lang=None,
            no_git_ignore=not self.use_git_ignore,
            max_memory=self.max_memory,
            timeout_threshold=self.timeout_threshold,
            disable_nosem=self.disable_nosem,
            baseline_commit=self.baseline_commit,
        )

    # I like doing it this way because then it's all in one spot
    # but I can see an argument for this being a function that takes a config
    @property
    def scanner(
        self,
    ) -> Callable[
        ...,
        Tuple[
            RuleMatchMap,
            List[SemgrepError],
            Set[Path],
            FileTargetingLog,
            List[Rule],
            ProfileManager,
            OutputExtra,
            Collection[RuleSeverity],
            Dict[str, int],
        ],
    ]:
        return self._scanner(configs=self.configs)

    @property
    def scanner_ci(
        self,
    ) -> Callable[
        ...,
        Tuple[
            RuleMatchMap,
            List[SemgrepError],
            Set[Path],
            FileTargetingLog,
            List[Rule],
            ProfileManager,
            OutputExtra,
            Collection[RuleSeverity],
            Dict[str, int],
        ],
    ]:
        return self._scanner(configs=[self.scan_url])

    # =====================
    # Config management
    # =====================

    def _update_target_manager(self) -> None:
        """Update our target manager so we're reporting accurate files"""
        self.target_manager = TargetManager(
            includes=self.include,
            excludes=self.exclude,
            max_target_bytes=self.max_target_bytes,
            target_strings=self.folder_paths,
        )

    def update_workspace(
        self,
        added: Optional[List[JsonObject]],
        removed: Optional[List[JsonObject]],
    ) -> None:
        """Add or remove folders from our config, and update what we need to"""
        if self._workspace_folders is not None:

            if added is not None:
                self._workspace_folders.extend(added)
            if removed is not None:
                self._workspace_folders = [
                    f for f in self._workspace_folders if f not in removed
                ]
        elif added is not None:
            self._workspace_folders = added
        # update workspace configs
        configs = []
        # Autodetect configs from workspace folders
        if self.autodetect_configs:
            for f in self.folder_paths:
                # check if semgrep.yaml exists in the folder
                possibles = [
                    "semgrep.yaml",
                    "semgrep.yml",
                    ".semgrep.yaml",
                    ".semgrep.yml",
                ]
                files = []
                for p in possibles:
                    files.extend(glob.glob(f + "/**/" + p, recursive=True))
                print(files)
                for f in files:
                    configs.append(str(f))
            self._workspace_configs = configs
        self._update_target_manager()
