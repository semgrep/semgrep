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
from typing import cast
from typing import Mapping
from typing import Optional

from ruamel.yaml import YAML

from semgrep.constants import SETTINGS_FILE
from semgrep.constants import USER_DATA_FOLDER
from semgrep.types import JsonObject


DEFAULT_SETTINGS: JsonObject = {}


class Settings:
    def __init__(self) -> None:
        self._loaded: Optional[JsonObject] = None
        self._yaml = YAML()
        self._yaml.default_flow_style = False
        self._path = Path("~").expanduser() / USER_DATA_FOLDER / SETTINGS_FILE

    @property
    def value(self) -> JsonObject:
        """
        Retrieve this system's settings object

        :raise PermissionError: if the process does not have access to the settings file
        """
        if self._loaded is None:
            if self._path.exists():
                with self._path.open("r") as fd:
                    self._loaded = self._yaml.load(fd)
                if not isinstance(self._loaded, Mapping):
                    raise TypeError(
                        f"Bad settings format; please check or remove {self._path}"
                    )
            else:
                self.value = DEFAULT_SETTINGS
        return cast(JsonObject, self._loaded)  # Above logic guarantees type

    @value.setter
    def value(self, value: JsonObject) -> None:
        """
        Update and save this system's settings object

        :param value: The settings object
        """
        if not self._path.parent.exists():
            self._path.parent.mkdir(parents=True, exist_ok=True)
        with self._path.open("w") as fd:
            self._yaml.dump(value, fd)
        # Make sure we've written the file successfully before altering the data structure
        self._loaded = value


SETTINGS = Settings()
