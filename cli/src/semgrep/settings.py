"""
Manages system settings for Semgrep

These can change the way Semgrep operates within the environment. They are distinct from config,
which describes which rules Semgrep should run.

To retrieve settings use `get_state().settings.get("key")`, to set them use `get_state().settings.set("key", "value").value = ...`.

If no settings have been configured on the system, DEFAULT_SETTINGS will be written.

If the process does not have permission to the settings path, a PermissionError will be raised;
callers should handle this gracefully.
"""
import os
import uuid
from pathlib import Path
from tempfile import mkstemp
from typing import Any
from typing import cast
from typing import Mapping
from typing import Optional

from attr import define
from attr import field
from ruamel.yaml import YAML
from typing_extensions import Literal
from typing_extensions import TypedDict

from semgrep.env import Env
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)
yaml = YAML()
yaml.default_flow_style = False


class SettingsSchema(TypedDict, total=False):
    has_shown_metrics_notification: bool
    api_token: Optional[str]
    anonymous_user_id: str


SettingsKeys = Literal[
    "has_shown_metrics_notification", "api_token", "anonymous_user_id"
]


def generate_anonymous_user_id(api_token: Optional[str]) -> str:
    return (
        str(uuid.uuid4())
        if api_token is None
        else str(uuid.uuid5(uuid.UUID("0" * 32), api_token))
    )


def generate_default_settings(api_token: Optional[str] = None) -> SettingsSchema:
    anonymous_user_id = generate_anonymous_user_id(api_token)
    logged_out_settings: SettingsSchema = {
        "has_shown_metrics_notification": False,
        "anonymous_user_id": anonymous_user_id,
    }
    return (
        {
            **logged_out_settings,
            "api_token": api_token,
        }
        if api_token is not None
        else logged_out_settings
    )


@define
class Settings:
    path: Path = field()
    _contents: SettingsSchema = field()

    @path.default
    def get_default_path(self) -> Path:
        """
        Uses {$XDG_CONFIG_HOME/semgrep || ~/.semgrep}/settings.yaml unless SEMGREP_SETTINGS_FILE is set
        """
        # state.env and state.settings get initialized together, but settings depends on env
        # so we just read the env a second time here ¯\_(ツ)_/¯
        env = Env()
        return env.user_settings_file

    @_contents.default
    def get_default_contents(self) -> SettingsSchema:
        """If file exists, read file. Otherwise use default"""
        # Must perform access check first in case we don't have permission to stat the path
        env = Env()
        default_settings = generate_default_settings(env.app_token)
        if not os.access(self.path, os.R_OK) or not self.path.is_file():
            return default_settings

        with self.path.open() as fd:
            yaml_contents = yaml.load(fd)

        if not isinstance(yaml_contents, Mapping):
            logger.warning(
                f"Bad settings format; {self.path} will be overridden. Contents:\n{yaml_contents}"
            )
            return default_settings

        return cast(SettingsSchema, {**default_settings, **yaml_contents})

    def __attrs_post_init__(self) -> None:
        self.save()  # in case we retrieved default contents

    # coupling: src/osemgrep/configuring/Semgrep_setting.ml save
    def save(self) -> None:
        try:
            if not self.path.parent.exists():
                self.path.parent.mkdir(parents=True, exist_ok=True)

            dir = str(self.path.parent)
            fd, tmp_path = mkstemp(suffix=".yml", prefix="settings", dir=dir, text=True)
            with os.fdopen(fd, "w") as f:
                yaml.dump(self._contents, f)

            # By writing to a guarenteed unique file and then renaming
            # that file.  The written contents are guarenteed to be
            # valid. If we write directly to the file, there is a
            # chance that concurrent instances of the program be
            # writing to the file at the same time an get race
            # conditions.
            os.replace(tmp_path, self.path)

        except PermissionError:
            logger.verbose("Could not write settings file at %s", self.path)

    def get(self, key: SettingsKeys, default: Any = None) -> Any:
        return self._contents.get(key, default)

    def set(self, key: SettingsKeys, value: Any) -> None:
        """
        Update and save this system's settings object

        :param value: The settings object
        """
        self._contents[key] = value
        self.save()

    def delete(self, key: SettingsKeys) -> None:
        """
        Deletes key KEY from settings file if it exists

        Noop otherwise
        """
        if key in self._contents:
            del self._contents[key]
            self.save()
