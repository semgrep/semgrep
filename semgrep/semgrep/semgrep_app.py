# Handle communication of findings / errors to semgrep.app
import os
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Set

import click
import requests
from urllib3.util.retry import Retry

from semgrep.constants import SEMGREP_URL
from semgrep.constants import SEMGREP_USER_AGENT
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatchMap
from semgrep.util import partition


# 4, 8, 16 seconds
RETRYING_ADAPTER = requests.adapters.HTTPAdapter(
    max_retries=Retry(
        total=3,
        backoff_factor=4,
        allowed_methods=["GET", "POST"],
        status_forcelist=(413, 429, 500, 502, 503),
    ),
)


class ScanHandler:
    def __init__(self, token: str) -> None:
        session = requests.Session()
        session.mount("https://", RETRYING_ADAPTER)
        session.headers["User-Agent"] = SEMGREP_USER_AGENT
        session.headers["Authorization"] = f"Bearer {token}"
        self.session = session
        self.scan_id = None
        self.deployment_id = self._get_deployment_id()

    def fail_open_exit_code(self, repo_name: str, exit_code: int) -> int:
        response = self.session.get(
            f"{SEMGREP_URL}api/agent/deployment/{self.deployment_id}/repos/{repo_name}",
            json={},
            timeout=30,
        )
        repo_data = response.json()
        fail_open = repo_data.get("repo").get("fail_open")
        return 0 if fail_open else exit_code

    def _get_deployment_id(self) -> Optional[int]:
        """
        Returns the deployment_id attached to an api_token as int

        Returns None if api_token is invalid/doesn't have associated deployment
        """
        r = self.session.get(
            f"{SEMGREP_URL}api/agent/deployment",
            timeout=10,
        )
        if r.ok:
            data = r.json()
            return data.get("deployment", {}).get("id")  # type: ignore
        else:
            return None

    def start_scan(self, meta: Dict[str, Any]) -> None:
        """
        Get scan id and file ignores

        returns ignored list
        """
        response = self.session.post(
            f"{SEMGREP_URL}api/agent/deployment/{self.deployment_id}/scan",
            json={"meta": meta},
            timeout=30,
        )

        if response.status_code == 404:
            raise Exception(
                "Failed to create a scan with given token and deployment_id."
                "Please make sure they have been set correctly."
                f"API server at {SEMGREP_URL} returned this response: {response.text}"
            )

        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(
                f"API server at {SEMGREP_URL} returned this error: {response.text}"
            )

        body = response.json()
        self.scan_id = body["scan"]["id"]
        self.autofix = body.get("autofix", False)
        self.ignore_patterns = body["scan"]["meta"].get("ignored_files", [])

    @property
    def scan_rules_url(self) -> str:
        return f"{SEMGREP_URL}api/agent/scan/{self.scan_id}/rules.yaml"

    def report_failure(self, exit_code: int) -> int:
        """
        Send semgrep cli non-zero exit code information to server
        and return what exit code semgrep should exit with.
        """
        response = self.session.post(
            f"{SEMGREP_URL}api/agent/scan/{self.scan_id}/error",
            json={
                "exit_code": exit_code,
                "stderr": "",
            },
            timeout=30,
        )

        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(f"API server returned this error: {response.text}")

        exit_code = int(response.json()["exit_code"])
        return exit_code

    def report_findings(
        self,
        matches_by_rule: RuleMatchMap,
        errors: List[SemgrepError],
        rules: List[Rule],
        targets: Set[Path],
        total_time: float,
    ) -> None:
        """ """
        all_ids = [r.id for r in rules]
        cai_ids, rule_ids = partition(
            lambda r_id: "r2c-internal-cai" in r_id,
            all_ids,
        )

        all_matches = [
            match
            for matches_of_rule in matches_by_rule.values()
            for match in matches_of_rule
        ]
        new_ignored, new_matches = partition(
            lambda match: match.is_ignored,
            all_matches,
        )

        # if self.scan.autofix:
        #     fields_to_omit.remove("fixed_lines")
        findings = {
            # send a backup token in case the app is not available
            "token": os.getenv("GITHUB_TOKEN"),
            "gitlab_token": os.getenv("GITLAB_TOKEN"),
            "findings": [match.to_app_finding_format() for match in new_matches],
            "searched_paths": [str(t) for t in targets],
            "rule_ids": rule_ids,
            "cai_ids": cai_ids,
        }
        ignores = {
            "findings": [match.to_app_finding_format() for match in new_ignored],
        }
        complete = {
            "exit_code": 1 if any(match.is_blocking for match in all_matches) else 0,
            "stats": {
                "findings": len(new_matches),
                "errors": errors,
                "total_time": total_time,
            },
        }

        response = self.session.post(
            f"{SEMGREP_URL}/api/agent/scan/{self.scan_id}/findings",
            json=findings,
            timeout=30,
        )
        try:
            response.raise_for_status()

            resp_errors = response.json()["errors"]
            for error in resp_errors:
                message = error["message"]
                click.echo(f"Server returned following warning: {message}", err=True)

        except requests.RequestException:
            raise Exception(f"API server returned this error: {response.text}")

        response = self.session.post(
            f"{SEMGREP_URL}/api/agent/scan/{self.scan_id}/ignores",
            json=ignores,
            timeout=30,
        )
        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(f"API server returned this error: {response.text}")

        # mark as complete
        response = self.session.post(
            f"{SEMGREP_URL}/api/agent/scan/{self.scan_id}/complete",
            json=complete,
            timeout=30,
        )

        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(
                f"API server at {SEMGREP_URL} returned this error: {response.text}"
            )
