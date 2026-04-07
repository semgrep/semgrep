#
# Copyright (c) 2025 Semgrep Inc.
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
import json
import sys
from pathlib import Path
from typing import Any

from pydantic import BaseModel
from pydantic import ConfigDict
from pydantic import Field

from semgrep.env import Env
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

SEMGREP_PLUGIN_SETTINGS_FILENAME = "plugin.json"


def get_semgrep_settings_paths(cwd: str | None) -> list[Path]:
    """
    Returns the paths to .semgrep/plugin.json files that should be loaded for a
    given workspace directory, in order from lowest to highest precedence:
      1. <workspace_dir>/.semgrep/plugin.json (project-level settings)
      2. ~/.semgrep/plugin.json (user-level settings, overrides project settings)
    """
    paths = []
    env = Env()
    project_dir = env.claude_project_dir or env.cursor_project_dir or cwd
    logger.info(f"Getting settings paths from workspace {project_dir}")
    if project_dir:
        paths.append(Path(project_dir) / ".semgrep" / SEMGREP_PLUGIN_SETTINGS_FILENAME)
    paths.append(Path.home() / ".semgrep" / SEMGREP_PLUGIN_SETTINGS_FILENAME)
    return paths


class HookSettings(BaseModel):
    model_config = ConfigDict(extra="ignore")

    disable_supply_chain_scan: bool = Field(default=False)


def _load_settings_file(path_str: str) -> dict[str, Any] | None:
    path = Path(path_str)
    if not path.exists():
        return None
    try:
        with open(path, "r", encoding="utf-8") as f:
            return dict(json.load(f))
    except Exception as e:
        print(
            f"Warning: failed to load hook settings from {path}: {e}",
            file=sys.stderr,
        )
        return None


def load_hook_settings(cwd: str | None = None) -> HookSettings:
    """
    Loads and merges hook settings from .semgrep/plugin.json files.

    Settings are loaded in increasing precedence order:
      1. <workspace_dir>/.semgrep/plugin.json (project-level)
      2. ~/.semgrep/plugin.json (user-level, overrides project)

    Returns default (no filtering) settings if no files exist or are malformed.
    """
    merged: dict[str, Any] = {}
    for path in get_semgrep_settings_paths(cwd):
        data = _load_settings_file(str(path))
        if data:
            # User-level settings (loaded last) override project-level settings
            merged.update(data)
    if merged:
        settings = HookSettings.model_validate(merged)
        logger.info(f"Loaded hook settings. cwd: {cwd}, settings: {settings}")
        return settings
    return HookSettings()
