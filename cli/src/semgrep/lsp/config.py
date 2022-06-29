import glob
import urllib
from functools import partial
from typing import Callable
from typing import List
from typing import Optional
from typing import Union

import semgrep.semgrep_main
from semgrep.config_resolver import get_config
from semgrep.constants import OutputFormat
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.project import get_project_url
from semgrep.rule import Rule
from semgrep.semgrep_types import Language
from semgrep.target_manager import TargetManager
from semgrep.types import JsonObject
from semgrep.util import flatten


class LSPConfig:
    def __init__(
        self, lsp_config: JsonObject, workspace_folders: List[JsonObject]
    ) -> None:
        self._settings = lsp_config
        self._workspace_folders = workspace_folders
        self.update_workspace(added=None, removed=None)
        self._update_target_manager()

    @property
    def settings(self) -> JsonObject:
        return self._settings

    @property
    def configs(self) -> List[str]:
        # First check if workspace folders/semgrep.yaml exists
        configs = []
        settings_configs = self._settings.get("semgrep.scan.configuration")
        if settings_configs is not None:
            configs.extend(settings_configs)
        # Should do something with CI here
        configs.extend(self._workspace_configs)
        if len(configs) > 0:
            return configs
        else:
            return ["auto"]

    @property
    def severity(self) -> List[str]:
        return self._settings.get("semgrep.scan.severity", ["INFO", "WARNING", "ERROR"])

    @property
    def exclude(self) -> List[str]:
        return self._settings.get("semgrep.scan.exclude", [])

    @property
    def include(self) -> List[str]:
        return self._settings.get("semgrep.scan.include", [])

    @property
    def jobs(self) -> int:
        return self._settings.get("semgrep.scan.jobs", 1)

    @property
    def configurationSource(self) -> List[str]:
        return self._settings.get("semgrep.scan.configurationSource", ["file"])

    @property
    def disable_nosem(self) -> bool:
        return self._settings.get("semgrep.scan.disableNoSem", False)

    @property
    def max_memory(self) -> int:
        return self._settings.get("semgrep.scan.maxMemory", 0)

    @property
    def max_target_bytes(self) -> int:
        return self._settings.get("semgrep.scan.maxTargetBytes", 0)

    @property
    def timeout_threshold(self) -> int:
        return self._settings.get("semgrep.scan.timeoutThreshold", 0)

    @property
    def use_git_ignore(self) -> bool:
        return self._settings.get("semgrep.scan.useGitIgnore", True)

    @property
    def project_url(self) -> Union[str, None]:
        return get_project_url()

    @property
    def watch_workspace(self) -> bool:
        return self._settings.get("semgrep.scan.watchedWorkspace", True)

    @property
    def rules(self) -> List[Rule]:
        configs_obj, config_errors = get_config(
            None, None, self.configs, project_url=self.project_url
        )
        # We don't want to rewrite IDs here because things get annoying UI wise
        all_rules = configs_obj.get_rules(True)
        filtered_rules = [
            rule for rule in all_rules if rule.severity.value in self.severity
        ]
        return filtered_rules

    @property
    def languages(self) -> List[Language]:
        return flatten([rule.languages for rule in self.rules])

    @property
    def folders(self) -> List[str]:
        return [f["uri"] for f in self._workspace_folders]

    @property
    def folder_paths(self) -> List[str]:
        folder_paths = []
        for f in self.folders:
            uri = urllib.parse.urlparse(f)
            target_name = urllib.request.url2pathname(uri.path)
            folder_paths.append(target_name)
        return folder_paths

    @property
    def scanner(self) -> Callable:
        # TODO: do something smart here with CI things in the future
        baseline_commit = self._settings.get("semgrep.scan.baselineCommit")
        output_settings = OutputSettings(output_format=OutputFormat.JSON)
        output_handler = OutputHandler(output_settings)
        return partial(
            semgrep.semgrep_main.main,
            pattern=None,
            lang=None,
            output_handler=output_handler,
            deep=False,
            configs=self.configs,
            severity=self.severity,
            exclude=self.exclude,
            include=self.include,
            jobs=self.jobs,
            no_git_ignore=not self.use_git_ignore,
            max_memory=self.max_memory,
            timeout_threshold=self.timeout_threshold,
            disable_nosem=self.disable_nosem,
            baseline_commit=baseline_commit,
        )

    def _update_target_manager(self) -> None:
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
        for f in self.folder_paths:
            # check if semgrep.yaml exists in the folder
            possibles = ["semgrep.yaml", "semgrep.yml", ".semgrep.yaml", ".semgrep.yml"]
            files = []
            for p in possibles:
                files.extend(glob.glob(f + "/**/" + p, recursive=True))
            print(files)
            for f in files:
                configs.append(str(f))
        self._workspace_configs = configs
        self._update_target_manager()
