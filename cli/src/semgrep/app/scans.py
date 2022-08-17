# Handle communication of findings / errors to semgrep.app
import json
import os
from collections import Counter
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from urllib.parse import urlencode

import click
import requests
import yaml as pyyaml
from boltons.iterutils import partition
from yaml import SafeDumper

from semgrep.constants import DEFAULT_SEMGREP_APP_CONFIG_URL
from semgrep.error import SemgrepError
from semgrep.parsing_data import ParsingData
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatchMap
from semgrep.state import get_state
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


class ScanHandler:
    def __init__(self, dry_run: bool) -> None:
        self._deployment_id: Optional[int] = None
        self._deployment_name: str = ""

        self.scan_id = None
        self.ignore_patterns: List[str] = []
        self._policy_names: List[str] = []
        self._autofix = False
        self.dry_run = dry_run
        self._dry_run_rules_url: str = ""
        self._skipped_syntactic_ids: List[str] = []
        self._skipped_match_based_ids: List[str] = []
        self._scan_params: str = ""
        self._rules: str = ""

    @property
    def deployment_id(self) -> Optional[int]:
        """
        Seperate property for easy of mocking in test
        """
        return self._deployment_id

    @property
    def deployment_name(self) -> str:
        """
        Seperate property for easy of mocking in test
        """
        return self._deployment_name

    @property
    def policy_names(self) -> List[str]:
        """
        Seperate property for easy of mocking in test
        """
        return self._policy_names

    @property
    def autofix(self) -> bool:
        """
        Seperate property for easy of mocking in test
        """
        return self._autofix

    @property
    def skipped_syntactic_ids(self) -> List[str]:
        """
        Seperate property for easy of mocking in test
        """
        return self._skipped_syntactic_ids

    @property
    def skipped_match_based_ids(self) -> List[str]:
        """
        Seperate property for easy of mocking in test
        """
        return self._skipped_match_based_ids

    @property
    def scan_params(self) -> str:
        """
        Seperate property for easy of mocking in test
        """
        return self._scan_params

    @property
    def rules(self) -> str:
        """
        Seperate property for easy of mocking in test
        """
        return self._rules

    def get_scan_config_from_app(self, url: str) -> Dict[str, Any]:
        state = get_state()
        response = state.app_session.get(url)
        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(
                f"API server at {state.env.semgrep_url} returned this error: {response.text}"
            )
        body = response.json()
        if isinstance(body, dict):
            return body
        else:
            raise Exception(
                f"API server at {state.env.semgrep_url} returned type '{type(response.json())}'. Expected a dictionary."
            )

    def get_scan_config(self, meta: Dict[str, Any]) -> None:
        """
        Get configurations for scan
        """
        state = get_state()
        logger.debug("Getting scan configurations")

        self._scan_params = urlencode(
            {
                "dry_run": self.dry_run,
                "repo_name": meta.get("repository"),
                "sca": meta.get("is_sca_scan", False),
                "full_scan": meta.get("is_full_scan", False),
                "semgrep_version": meta.get("semgrep_version", "0.0.0"),
            }
        )
        app_get_config_url = f"{state.env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{self._scan_params}"
        body = self.get_scan_config_from_app(app_get_config_url)

        self._deployment_id = body["deployment_id"]
        self._deployment_name = body["deployment_name"]
        self._policy_names = body["policy_names"]
        self._autofix = body.get("autofix", False)
        self._skipped_syntactic_ids = body.get("triage_ignored_syntactic_ids", [])
        self._skipped_match_based_ids = body.get("triage_ignored_match_based_ids", [])
        self._rules = pyyaml.dump(body.get("rule_config"), Dumper=SafeDumper)
        self.ignore_patterns = body.get("ignored_files", [])

    def start_scan(self, meta: Dict[str, Any]) -> None:
        """
        Get scan id and file ignores

        returns ignored list
        """
        state = get_state()
        if self.dry_run:
            logger.info(f"Would have sent POST request to create scan")
            return

        logger.debug("Starting scan")
        response = state.app_session.post(
            f"{state.env.semgrep_url}/api/agent/deployments/scans",
            json={"meta": meta, "policy": self._policy_names},
        )

        if response.status_code == 404:
            raise Exception(
                "Failed to create a scan with given token and deployment_id."
                "Please make sure they have been set correctly."
                f"API server at {state.env.semgrep_url} returned this response: {response.text}"
            )

        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(
                f"API server at {state.env.semgrep_url} returned this error: {response.text}"
            )

        body = response.json()
        self.scan_id = body["scan"]["id"]

    def report_failure(self, exit_code: int) -> None:
        """
        Send semgrep cli non-zero exit code information to server
        and return what exit code semgrep should exit with.
        """
        state = get_state()
        if self.dry_run:
            logger.info(f"Would have reported failure to semgrep.dev: {exit_code}")
            return

        response = state.app_session.post(
            f"{state.env.semgrep_url}/api/agent/scans/{self.scan_id}/error",
            json={
                "exit_code": exit_code,
                "stderr": "",
            },
        )

        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(f"API server returned this error: {response.text}")

    def report_findings(
        self,
        matches_by_rule: RuleMatchMap,
        errors: List[SemgrepError],
        rules: List[Rule],
        targets: Set[Path],
        ignored_targets: Set[Path],
        parse_rate: ParsingData,
        total_time: float,
        commit_date: str,
    ) -> None:
        """
        commit_date here for legacy reasons. epoch time of latest commit
        """
        state = get_state()
        all_ids = [r.id for r in rules]
        cai_ids, rule_ids = partition(all_ids, lambda r_id: "r2c-internal-cai" in r_id)

        all_matches = [
            match
            for matches_of_rule in matches_by_rule.values()
            for match in matches_of_rule
        ]
        new_ignored, new_matches = partition(
            all_matches,
            lambda match: bool(match.is_ignored),
        )

        findings = {
            # send a backup token in case the app is not available
            "token": os.getenv("GITHUB_TOKEN"),
            "gitlab_token": os.getenv("GITLAB_TOKEN"),
            "findings": [
                match.to_app_finding_format(commit_date).to_json()
                for match in new_matches
            ],
            "searched_paths": [str(t) for t in targets],
            "rule_ids": rule_ids,
            "cai_ids": cai_ids,
        }
        ignores = {
            "findings": [
                match.to_app_finding_format(commit_date).to_json()
                for match in new_ignored
            ],
        }

        ignored_ext_freqs = Counter(
            [os.path.splitext(path)[1] for path in ignored_targets]
        )
        ignored_ext_freqs.pop("", None)  # don't count files with no extension

        complete = {
            "exit_code": 1
            if any(match.is_blocking and not match.is_ignored for match in all_matches)
            else 0,
            "stats": {
                "findings": len(new_matches),
                "errors": [error.to_dict() for error in errors],
                "total_time": total_time,
                "unsupported_exts": dict(ignored_ext_freqs),
                "parse_rate": {
                    lang: {
                        "targets_parsed": data.num_targets - data.targets_with_errors,
                        "num_targets": data.num_targets,
                        "bytes_parsed": data.num_bytes - data.error_bytes,
                        "num_bytes": data.num_bytes,
                    }
                    for (lang, data) in parse_rate.get_errors_by_lang().items()
                },
            },
        }

        if self.dry_run:
            logger.info(
                f"Would have sent findings blob: {json.dumps(findings, indent=4)}"
            )
            logger.info(
                f"Would have sent ignores blob: {json.dumps(ignores, indent=4)}"
            )
            logger.info(
                f"Would have sent complete blob: {json.dumps(complete, indent=4)}"
            )
            return
        else:
            logger.debug(f"Sending findings blob: {json.dumps(findings, indent=4)}")
            logger.debug(f"Sending ignores blob: {json.dumps(ignores, indent=4)}")
            logger.debug(f"Sending complete blob: {json.dumps(complete, indent=4)}")

        response = state.app_session.post(
            f"{state.env.semgrep_url}/api/agent/scans/{self.scan_id}/findings",
            json=findings,
        )
        try:
            response.raise_for_status()

            resp_errors = response.json()["errors"]
            for error in resp_errors:
                message = error["message"]
                click.echo(f"Server returned following warning: {message}", err=True)

        except requests.RequestException:
            raise Exception(f"API server returned this error: {response.text}")

        response = state.app_session.post(
            f"{state.env.semgrep_url}/api/agent/scans/{self.scan_id}/ignores",
            json=ignores,
        )
        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(f"API server returned this error: {response.text}")

        # mark as complete
        response = state.app_session.post(
            f"{state.env.semgrep_url}/api/agent/scans/{self.scan_id}/complete",
            json=complete,
        )

        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(
                f"API server at {state.env.semgrep_url} returned this error: {response.text}"
            )
