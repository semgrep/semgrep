import urllib
from typing import Any
from typing import List
from typing import Mapping
from typing import Optional
from typing import Union

from semgrep.app import auth
from semgrep.app.scans import ScanHandler
from semgrep.config_resolver import get_config
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
        if "lsp" not in lsp_config:
            lsp_config["lsp"] = {}
        self._settings: Mapping[str, Mapping[str, Any]] = lsp_config
        self._workspace_folders = workspace_folders
        self.update_target_manager()

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
        if self.logged_in:
            # this can fail if something isn't a git repo
            try:
                configs.append(self.scan_url)
            except Exception:
                pass
        if len(configs) > 0:
            return configs
        else:
            return ["auto"]

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
        return self._settings["scan"].get("maxTargetBytes", 0)

    @property
    def project_url(self) -> Union[str, None]:
        return get_project_url()

    @property
    def scan_url(self) -> str:
        get_state()
        scan_handler = ScanHandler(True)
        metadata = generate_meta_from_environment(None)
        metadata_dict = metadata.to_dict()
        scan_handler.fetch_and_init_scan_config(metadata_dict)
        return scan_handler.rules

    # =====================
    # Semgrep LSP settings
    # =====================
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

    @property
    def rules(self) -> List[Rule]:
        configs_obj, _ = get_config(
            None, None, self.configs, project_url=self.project_url
        )
        all_rules = configs_obj.get_rules(True)
        return all_rules

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
        return self.token is not None

    # =====================
    # Config management
    # =====================

    def update_target_manager(self) -> None:
        self.target_manager = TargetManager(
            includes=self.include,
            excludes=self.exclude,
            max_target_bytes=self.max_target_bytes,
            respect_git_ignore=True,
            file_ignore=get_file_ignore(),
            target_strings=self.folder_paths,
        )
