# Handle communication of findings / errors to semgrep.app
import json
import os
from collections import Counter
from pathlib import Path
from typing import Any
from typing import Dict
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Set
from urllib.parse import urlencode

import click
import requests
from boltons.iterutils import partition

from semgrep.constants import DEFAULT_SEMGREP_APP_CONFIG_URL
from semgrep.constants import RuleSeverity
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
        self._deepsemgrep = False
        self.dry_run = dry_run
        self._dry_run_rules_url: str = ""
        self._skipped_syntactic_ids: List[str] = []
        self._skipped_match_based_ids: List[str] = []
        self._scan_params: str = ""
        self._rules: str = ""

    @property
    def deployment_id(self) -> Optional[int]:
        """
        Separate property for easy of mocking in test
        """
        return self._deployment_id

    @property
    def deployment_name(self) -> str:
        """
        Separate property for easy of mocking in test
        """
        return self._deployment_name

    @property
    def policy_names(self) -> List[str]:
        """
        Separate property for easy of mocking in test
        """
        return self._policy_names

    @property
    def autofix(self) -> bool:
        """
        Separate property for easy of mocking in test
        """
        return self._autofix

    @property
    def deepsemgrep(self) -> bool:
        """
        Separate property for easy of mocking in test
        """
        return self._deepsemgrep

    @property
    def skipped_syntactic_ids(self) -> List[str]:
        """
        Separate property for easy of mocking in test
        """
        return self._skipped_syntactic_ids

    @property
    def skipped_match_based_ids(self) -> List[str]:
        """
        Separate property for easy of mocking in test
        """
        return self._skipped_match_based_ids

    @property
    def scan_params(self) -> str:
        """
        Separate property for easy of mocking in test
        """
        return self._scan_params

    @property
    def rules(self) -> str:
        """
        Separate property for easy of mocking in test
        """
        return self._rules

    def _get_scan_config_from_app(self, url: str) -> Dict[str, Any]:
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

    def fetch_config_and_start_scan_old(self, meta: Dict[str, Any]) -> None:
        """
        Get configurations for scan and create scan object
        Backwards-compatible with versions of semgrep-app from before Aug 2022
        TODO: Delete once all old, on-prem instances have been deprecated
        """
        state = get_state()

        logger.debug("Getting deployment information")
        get_deployment_url = f"{state.env.semgrep_url}/api/agent/deployments/current"
        body = self._get_scan_config_from_app(get_deployment_url)
        self._deployment_id = body["deployment"]["id"]
        self._deployment_name = body["deployment"]["name"]

        logger.debug("Creating scan")
        create_scan_url_old = (
            f"{state.env.semgrep_url}/api/agent/deployments/{self.deployment_id}/scans"
        )
        response = state.app_session.post(
            create_scan_url_old,
            json={"meta": meta},
        )
        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(
                f"API server at {state.env.semgrep_url} returned this error: {response.text}"
            )
        body = response.json()
        self.scan_id = body["scan"]["id"]
        self._policy_names = body["policy"]
        self._autofix = body.get("autofix") or False
        self._skipped_syntactic_ids = body.get("triage_ignored_syntactic_ids") or []
        self._skipped_match_based_ids = body.get("triage_ignored_match_based_ids") or []

        state.error_handler.append_request(scan_id=self.scan_id)

        logger.debug("Getting rules file")
        get_rules_url = (
            f"{state.env.semgrep_url}/api/agent/scans/{self.scan_id}/rules.yaml"
        )
        self._rules = get_rules_url

    def fetch_and_init_scan_config(self, meta: Dict[str, Any]) -> None:
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
        body = self._get_scan_config_from_app(app_get_config_url)

        self._deployment_id = body["deployment_id"]
        self._deployment_name = body["deployment_name"]
        self._policy_names = body["policy_names"]
        self._rules = body["rule_config"]
        self._autofix = body.get("autofix") or False
        self._deepsemgrep = body.get("deepsemgrep") or False
        self._skipped_syntactic_ids = body.get("triage_ignored_syntactic_ids") or []
        self._skipped_match_based_ids = body.get("triage_ignored_match_based_ids") or []
        self.ignore_patterns = body.get("ignored_files") or []

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
            json={"meta": meta, "policy_names": self._policy_names},
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
        renamed_targets: Set[Path],
        ignored_targets: FrozenSet[Path],
        parse_rate: ParsingData,
        total_time: float,
        commit_date: str,
        lockfile_scan_info: Dict[str, int],
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
        # we want date stamps assigned by the app to be assigned such that the
        # current sort by relevant_since results in findings within a given scan
        # appear in an intuitive order.  this requires reversed ordering here.
        all_matches.reverse()
        sort_order = {  # used only to order rules by severity
            "EXPERIMENT": 0,
            "INVENTORY": 1,
            "INFO": 2,
            "WARNING": 3,
            "ERROR": 4,
        }
        # NB: sorted guarantees stable sort, so within a given severity level
        # issues remain sorted as before
        all_matches = sorted(
            all_matches, key=lambda match: sort_order[match.severity.value]
        )
        new_ignored, new_matches = partition(
            all_matches, lambda match: bool(match.is_ignored)
        )
        findings = [
            match.to_app_finding_format(commit_date).to_json() for match in new_matches
        ]
        ignores = [
            match.to_app_finding_format(commit_date).to_json() for match in new_ignored
        ]
        findings_and_ignores = {
            # send a backup token in case the app is not available
            "token": os.getenv("GITHUB_TOKEN"),
            "gitlab_token": os.getenv("GITLAB_TOKEN"),
            "findings": findings,
            "searched_paths": [str(t) for t in targets],
            "renamed_paths": [str(rt) for rt in renamed_targets],
            "rule_ids": rule_ids,
            "cai_ids": cai_ids,
            "ignores": ignores,
        }

        if any(match.severity == RuleSeverity.EXPERIMENT for match in new_ignored):
            logger.info("Some experimental rules were run during execution.")

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
                "lockfile_scan_info": lockfile_scan_info,
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
                f"Would have sent findings and ignores blob: {json.dumps(findings_and_ignores, indent=4)}"
            )
            logger.info(
                f"Would have sent complete blob: {json.dumps(complete, indent=4)}"
            )
            return
        else:
            logger.debug(
                f"Sending findings and ignores blob: {json.dumps(findings_and_ignores, indent=4)}"
            )
            logger.debug(f"Sending complete blob: {json.dumps(complete, indent=4)}")

        response = state.app_session.post(
            f"{state.env.semgrep_url}/api/agent/scans/{self.scan_id}/findings_and_ignores",
            json=findings_and_ignores,
        )
        # TODO: delete this once all on-prem app instances are gone
        if (
            response.status_code == 404
            and state.env.semgrep_url != "https://semgrep.dev"
        ):
            # order matters here - findings sends back errors but ignores doesn't
            ignores_response = state.app_session.post(
                f"{state.env.semgrep_url}/api/agent/scans/{self.scan_id}/ignores",
                json={"findings": ignores},
            )
            response = state.app_session.post(
                f"{state.env.semgrep_url}/api/agent/scans/{self.scan_id}/findings",
                json=findings_and_ignores,
            )
        try:
            response.raise_for_status()

            resp_errors = response.json()["errors"]
            for error in resp_errors:
                message = error["message"]
                click.echo(f"Server returned following warning: {message}", err=True)

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
