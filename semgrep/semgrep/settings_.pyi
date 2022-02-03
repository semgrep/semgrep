"""Manages system settings for Semgrep

These can change the way Semgrep operates within the environment. They
are distinct from config, which describes which rules Semgrep should
run.

To retrieve settings use `SETTINGS.value`, to set them use `SETTINGS.value = ...`.

If no settings have been configured on the system, DEFAULT_SETTINGS will be written.

If the process does not have permission to the settings path, a PermissionError will be raised;
callers should handle this gracefully.

"""

class Settings:
    def __init__(self) -> None:
       ...

#TODO: type
SETTINGS = ...
