"""
Manages system settings for Semgrep

These can change the way Semgrep operates within the environment. They are distinct from config,
which describes which rules Semgrep should run.

To retrieve settings use `SETTINGS.value`, to set them use `SETTINGS.value = ...`.

If no settings have been configured on the system, DEFAULT_SETTINGS will be written.

If the process does not have permission to the settings path, a PermissionError will be raised;
callers should handle this gracefully.
"""
import os
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import Mapping

from ruamel.yaml import YAML

from semgrep.constants import SEMGREP_SETTINGS_FILE
from semgrep.constants import SETTINGS_FILE
from semgrep.constants import USER_DATA_FOLDER
from semgrep.verbose_logging import getLogger

DEFAULT_SETTINGS: Dict[str, Any] = {}


class Settings:
    def __init__(self) -> None:
        self._logger = getLogger(__name__)
        self._yaml = YAML()
        self._yaml.default_flow_style = False
        self._path = Settings.get_path_to_settings()

        # If file exists, read file. Otherwise use default
        # Must perform access check first in case we don't have permission to stat the path
        if os.access(self._path, os.R_OK) and self._path.exists():
            with self._path.open("r") as fd:
                yaml_file = self._yaml.load(fd)
            if not isinstance(yaml_file, Mapping):
                self._logger.warning(
                    f"Bad settings format; {self._path} will be overriden. Contents:\n{yaml_file}"
                )
                self._value = DEFAULT_SETTINGS
            else:
                self._value = cast(Dict[str, Any], yaml_file)
        else:
            self._value = DEFAULT_SETTINGS

    @staticmethod
    def get_path_to_settings() -> Path:
        """
        Uses ~/.semgrep/settings.yaml unless SEMGREP_SETTINGS_FILE is set
        """
        if SEMGREP_SETTINGS_FILE:
            return Path(SEMGREP_SETTINGS_FILE)

        return Path("~").expanduser() / USER_DATA_FOLDER / SETTINGS_FILE

    def add_setting(self, key: str, value: Any) -> None:
        """
        Update and save this system's settings object

        :param value: The settings object
        """
        self._value[key] = value
        try:
            if not self._path.parent.exists():
                self._path.parent.mkdir(parents=True, exist_ok=True)
            with self._path.open("w") as fd:
                self._yaml.dump(self._value, fd)
        except PermissionError:
            self._logger.verbose("Could not write settings file at %s", self._path)

    def get_setting(self, key: str, *, default: Any) -> Any:
        return self._value.get(key, default)


SETTINGS = Settings()
