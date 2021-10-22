"""
Manages system settings for Semgrep

These can change the way Semgrep operates within the environment. They are distinct from config,
which describes which rules Semgrep should run.

To retrieve settings use `SETTINGS.value`, to set them use `SETTINGS.value = ...`.

If no settings have been configured on the system, DEFAULT_SETTINGS will be written.

If the process does not have permission to the settings path, a PermissionError will be raised;
callers should handle this gracefully.
"""
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import Mapping

from ruamel.yaml import YAML

from semgrep.constants import SETTINGS_FILE
from semgrep.constants import USER_DATA_FOLDER


DEFAULT_SETTINGS: Dict[str, Any] = {}


class Settings:
    def __init__(self) -> None:
        self._yaml = YAML()
        self._yaml.default_flow_style = False
        self._path = Path("~").expanduser() / USER_DATA_FOLDER / SETTINGS_FILE

        # If file exists, read file. Otherwise use default
        if self._path.exists():
            with self._path.open("r") as fd:
                yaml_file = self._yaml.load(fd)
            if not isinstance(yaml_file, Mapping):
                raise TypeError(
                    f"Bad settings format; please check or remove {self._path}"
                )
            self._value = cast(Dict[str, Any], yaml_file)
        else:
            self._value = DEFAULT_SETTINGS

    def add_setting(self, key: str, value: Any) -> None:
        """
        Update and save this system's settings object

        :param value: The settings object
        """
        self._value[key] = value
        if not self._path.parent.exists():
            self._path.parent.mkdir(parents=True, exist_ok=True)
        with self._path.open("w") as fd:
            self._yaml.dump(self._value, fd)

    def get_setting(self, key: str, *, default: Any) -> Any:
        return self._value.get(key, default)


SETTINGS = Settings()
