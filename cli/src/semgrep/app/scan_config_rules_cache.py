#
# Copyright (c) 2026 Semgrep Inc.
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
import hashlib
import json
import zlib
from pathlib import Path
from time import time
from typing import Optional

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.app.project_config import ProjectConfig
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# Use SCAN_CONFIG_RULES_CACHE as the singleton service entry point. The service
# is stateless; cache behavior is controlled by explicit caller inputs and
# module-level cache layout constants.

RULES_CACHE_DIR = Path(".semgrep_rules")
RULES_CACHE_TTL_SECONDS = 60 * 60
RULES_CACHE_KEY_LENGTH = 8
RULES_CACHE_DATA_SUFFIX = ".json.zlib"


class ScanConfigRulesCache:
    def _get_cache_key(
        self,
        scan_metadata: out.ScanMetadata,
        semgrep_url: str,
        deployment_id: int,
        project_metadata: out.ProjectMetadata,
        project_config: ProjectConfig,
    ) -> str:
        scan_metadata_json = scan_metadata.to_json()
        # unique_id changes on every run, but should not prevent reusing the
        # same server-generated scan config.
        scan_metadata_json.pop("unique_id", None)

        key_input = {
            "schema_version": 1,
            "semgrep_url": semgrep_url,
            "deployment_id": deployment_id,
            "scan_metadata": scan_metadata_json,
            "project_metadata": project_metadata.to_json(),
            "project_config": project_config.to_CiConfigFromRepo().to_json(),
        }
        raw_key = json.dumps(key_input, sort_keys=True, separators=(",", ":"))
        return hashlib.sha256(raw_key.encode("utf-8")).hexdigest()[
            :RULES_CACHE_KEY_LENGTH
        ]

    def _get_cache_path(
        self,
        scan_metadata: out.ScanMetadata,
        semgrep_url: str,
        deployment_id: Optional[int],
        project_metadata: Optional[out.ProjectMetadata],
        project_config: Optional[ProjectConfig],
        *,
        cache_rules: bool = False,
    ) -> Optional[Path]:
        if not cache_rules:
            return None

        if deployment_id is None or project_metadata is None or project_config is None:
            return None

        cache_key = self._get_cache_key(
            scan_metadata, semgrep_url, deployment_id, project_metadata, project_config
        )
        return RULES_CACHE_DIR / f"{cache_key}{RULES_CACHE_DATA_SUFFIX}"

    def get_cached_rules(
        self,
        scan_metadata: out.ScanMetadata,
        semgrep_url: str,
        deployment_id: Optional[int],
        project_metadata: Optional[out.ProjectMetadata],
        project_config: Optional[ProjectConfig],
        *,
        cache_rules: bool = False,
    ) -> Optional[out.RawJson]:
        cache_path = self._get_cache_path(
            scan_metadata,
            semgrep_url,
            deployment_id,
            project_metadata,
            project_config,
            cache_rules=cache_rules,
        )
        # cache_path is also None when the rules cache flag is disabled.
        if cache_path is None or not cache_path.exists():
            return None
        if not self._is_fresh(cache_path):
            return None

        rules = self._read_cached_rules(cache_path)
        if rules is None:
            return None

        logger.verbose("Using cached scan config rules from %s", cache_path)
        return rules

    def write_cached_rules(
        self,
        rules: out.RawJson,
        scan_metadata: out.ScanMetadata,
        semgrep_url: str,
        deployment_id: Optional[int],
        project_metadata: Optional[out.ProjectMetadata],
        project_config: Optional[ProjectConfig],
        *,
        cache_rules: bool = False,
    ) -> None:
        cache_path = self._get_cache_path(
            scan_metadata,
            semgrep_url,
            deployment_id,
            project_metadata,
            project_config,
            cache_rules=cache_rules,
        )
        # cache_path is also None when the rules cache flag is disabled.
        if cache_path is None:
            return

        try:
            cache_path.parent.mkdir(parents=True, exist_ok=True)
            cache_path.write_bytes(
                zlib.compress(
                    rules.to_json_string().encode("utf-8"),
                    # Level 1 keeps cache writes cheap while still shrinking
                    # large JSON rule blobs by roughly 90%.
                    level=1,
                )
            )
            self._get_meta_path(cache_path).write_text(
                json.dumps(
                    {
                        "expires_at": time() + RULES_CACHE_TTL_SECONDS,
                    },
                    sort_keys=True,
                ),
                encoding="utf-8",
            )
        except OSError as ex:
            logger.debug("Failed to write cached rules to %s: %s", cache_path, ex)

    def _get_meta_path(self, cache_path: Path) -> Path:
        return cache_path.with_suffix("").with_suffix(".meta.json")

    def _is_fresh(self, cache_path: Path) -> bool:
        meta_path = self._get_meta_path(cache_path)
        if not meta_path.exists():
            return False

        try:
            meta = json.loads(meta_path.read_text(encoding="utf-8"))
            expires_at = meta.get("expires_at")
            if not isinstance(expires_at, (int, float)):
                return False
        except (OSError, json.JSONDecodeError) as ex:
            logger.debug(
                "Failed to read cached rules metadata from %s: %s", meta_path, ex
            )
            return False

        return time() < expires_at

    def _read_cached_rules(self, cache_path: Path) -> Optional[out.RawJson]:
        try:
            raw = json.loads(zlib.decompress(cache_path.read_bytes()))
        except (OSError, json.JSONDecodeError, zlib.error) as ex:
            logger.debug("Failed to read cached rules from %s: %s", cache_path, ex)
            return None

        if not isinstance(raw, dict):
            return None

        if not isinstance(raw.get("rules"), list):
            return None

        return out.RawJson(raw)


SCAN_CONFIG_RULES_CACHE = ScanConfigRulesCache()
