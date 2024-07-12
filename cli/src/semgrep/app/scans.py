# Handle communication of findings / errors to semgrep.app
import json
import os
import sys
from collections import Counter
from dataclasses import dataclass
from datetime import datetime
from datetime import timedelta
from pathlib import Path
from time import sleep
from typing import Dict
from typing import FrozenSet
from typing import List
from typing import Optional
from typing import Set
from typing import TYPE_CHECKING

import click
import requests
from boltons.iterutils import partition

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parsers.util import DependencyParserError
from semgrep import __VERSION__
from semgrep import tracing
from semgrep.app.project_config import ProjectConfig
from semgrep.constants import USER_FRIENDLY_PRODUCT_NAMES
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.parsing_data import ParsingData
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatchMap
from semgrep.state import get_state
from semgrep.target_manager import ALL_PRODUCTS
from semgrep.verbose_logging import getLogger

if TYPE_CHECKING:
    from semgrep.engine import EngineType
    from rich.progress import Progress

logger = getLogger(__name__)


@dataclass
class ScanCompleteResult:
    success: bool
    app_block_override: bool
    app_block_reason: str


class ScanHandler:
    def __init__(self, dry_run: bool = False) -> None:
        state = get_state()
        self.local_id = str(state.local_scan_id)
        self.scan_metadata = out.ScanMetadata(
            cli_version=out.Version(__VERSION__),
            unique_id=out.Uuid(self.local_id),
            requested_products=[],
            dry_run=dry_run,
        )
        self.scan_response: Optional[out.ScanResponse] = None
        self.dry_run = dry_run
        self._dry_run_rules_url: str = ""
        self._scan_params: str = ""
        self.ci_scan_results: Optional[out.CiScanResults] = None

    @property
    def scan_id(self) -> Optional[int]:
        if self.scan_response:
            return self.scan_response.info.id
        return None

    @property
    def deployment_id(self) -> Optional[int]:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return self.scan_response.info.deployment_id
        return None

    @property
    def deployment_name(self) -> Optional[str]:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return self.scan_response.info.deployment_name
        return None

    @property
    def autofix(self) -> bool:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return self.scan_response.engine_params.autofix
        return False

    @property
    def deepsemgrep(self) -> bool:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return self.scan_response.engine_params.deepsemgrep
        return False

    @property
    def generic_slow_rollout(self) -> bool:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return self.scan_response.engine_params.generic_slow_rollout
        return False

    @property
    def dependency_query(self) -> bool:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return self.scan_response.engine_params.dependency_query
        return False

    @property
    def skipped_syntactic_ids(self) -> List[str]:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return self.scan_response.config.triage_ignored_syntactic_ids
        return []

    @property
    def skipped_match_based_ids(self) -> List[str]:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return self.scan_response.config.triage_ignored_match_based_ids
        return []

    @property
    def ignore_patterns(self) -> out.ProductIgnoredFiles:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            if self.scan_response.engine_params.product_ignored_files:
                return self.scan_response.engine_params.product_ignored_files
            # Deprecated, but used as a fallback in case
            # product_ignored_files is not set.
            if self.scan_response.engine_params.ignored_files:
                return out.ProductIgnoredFiles(
                    value={
                        product: [
                            out.Glob(pattern)
                            for pattern in self.scan_response.engine_params.ignored_files
                        ]
                        for product in ALL_PRODUCTS
                    }
                )
        return out.ProductIgnoredFiles(value={})

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
        if self.scan_response:
            return self.scan_response.config.rules.to_json_string()

        return ""

    @property
    def enabled_products(self) -> List[str]:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return [p.to_json() for p in self.scan_response.info.enabled_products]
        return []

    @property
    def historical_config(self) -> out.HistoricalConfiguration:
        config = None
        if self.scan_response:
            config = self.scan_response.engine_params.historical_config
        if config:
            return config
        return out.HistoricalConfiguration(enabled=False)

    @tracing.trace()
    def start_scan(
        self, project_metadata: out.ProjectMetadata, project_config: ProjectConfig
    ) -> None:
        """
        Get scan id and file ignores

        returns ignored list
        """
        state = get_state()

        request = out.ScanRequest(
            meta=out.RawJson(
                {
                    **project_metadata.to_json(),
                    **project_config.to_CiConfigFromRepo().to_json(),
                }
            ),
            scan_metadata=self.scan_metadata,
            project_metadata=project_metadata,
            project_config=project_config.to_CiConfigFromRepo(),
        ).to_json()

        logger.debug(f"Starting scan: {json.dumps(request, indent=4)}")
        response = state.app_session.post(
            f"{state.env.semgrep_url}/api/cli/scans",
            json=request,
        )

        if response.status_code == 401:
            logger.info(
                "API token not valid. Try to run `semgrep logout` and `semgrep login` again. "
                "Or in CI, ensure your SEMGREP_APP_TOKEN variable is set correctly.",
            )
            sys.exit(INVALID_API_KEY_EXIT_CODE)

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

        self.scan_response = out.ScanResponse.from_json(response.json())
        logger.debug(
            f"Scan started: {json.dumps(self.scan_response.to_json(), indent=4)}"
        )

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

    @tracing.trace()
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
        lockfile_dependencies: Dict[str, List[out.FoundDependency]],
        dependency_parser_errors: List[DependencyParserError],
        contributions: out.Contributions,
        engine_requested: "EngineType",
        progress_bar: "Progress",
    ) -> ScanCompleteResult:
        """
        commit_date here for legacy reasons. epoch time of latest commit

        Returns (success, block_scan, block_reason)
        """
        state = get_state()
        rule_ids = [out.RuleId(r.id) for r in rules]
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
            out.Experiment(): 0,
            out.Inventory(): 1,
            out.Info(): 2,
            out.Low(): 2,
            out.Warning(): 3,
            out.Medium(): 3,
            out.Error(): 4,
            out.High(): 4,
            out.Critical(): 5,
        }
        # NB: sorted guarantees stable sort, so within a given severity level
        # issues remain sorted as before
        all_matches = sorted(
            all_matches, key=lambda match: sort_order[match.severity.value]
        )
        new_ignored, new_matches = partition(
            all_matches, lambda match: bool(match.is_ignored)
        )
        findings = [match.to_app_finding_format(commit_date) for match in new_matches]
        ignores = [match.to_app_finding_format(commit_date) for match in new_ignored]
        token = (
            # GitHub (cloud)
            os.getenv("GITHUB_TOKEN")
            # GitLab.com (cloud)
            or os.getenv("GITLAB_TOKEN")
            # Bitbucket Cloud
            or os.getenv("BITBUCKET_TOKEN")
        )

        self.ci_scan_results = out.CiScanResults(
            # send a backup token in case the app is not available
            token=token,
            findings=findings,
            ignores=ignores,
            searched_paths=[out.Fpath(str(t)) for t in sorted(targets)],
            renamed_paths=[out.Fpath(str(rt)) for rt in sorted(renamed_targets)],
            rule_ids=rule_ids,
            contributions=contributions,
        )
        if self.dependency_query:
            self.ci_scan_results.dependencies = out.CiScanDependencies(
                lockfile_dependencies
            )

        findings_and_ignores = self.ci_scan_results.to_json()

        if any(
            isinstance(match.severity.value, out.Experiment) for match in new_ignored
        ):
            logger.info("Some experimental rules were run during execution.")

        ignored_ext_freqs = Counter(
            [os.path.splitext(path)[1] for path in ignored_targets]
        )
        ignored_ext_freqs.pop("", None)  # don't count files with no extension

        dependency_counts = {k: len(v) for k, v in lockfile_dependencies.items()}

        # NOTE: This mirrors the logic in metrics.py to show the number of
        #  findings by product for SCP customers. See PA-3312
        #  We should consider refactoring this logic into a shared function
        #  in a future PR for metric and behavioral consistency.
        #  An open question remains on whether we should be including the number
        #  of ignored findings in this count.

        findings_by_product: Dict[str, int] = Counter()
        for r, f in matches_by_rule.items():
            # NOTE: For parity with metrics.py, we are using the human-readable product name,
            #  (i.e. code) and falling back to the internal json string (i.e. sast) if we
            #  somehow drift out of sync with the product enum.
            name = USER_FRIENDLY_PRODUCT_NAMES.get(r.product, r.product.to_json())
            findings_by_product[f"{name}"] += len(f)

        complete = out.CiScanComplete(
            exit_code=(
                1
                if any(
                    match.is_blocking and not match.is_ignored for match in all_matches
                )
                else 0
            ),
            dependency_parser_errors=dependency_parser_errors,
            stats=out.CiScanCompleteStats(
                findings=len(
                    [match for match in new_matches if not match.from_transient_scan]
                ),
                errors=[error.to_CliError() for error in errors],
                total_time=total_time,
                unsupported_exts=dict(ignored_ext_freqs),
                lockfile_scan_info=dependency_counts,
                parse_rate={
                    lang: out.ParsingStats(
                        targets_parsed=data.num_targets - data.targets_with_errors,
                        num_targets=data.num_targets,
                        bytes_parsed=data.num_bytes - data.error_bytes,
                        num_bytes=data.num_bytes,
                    )
                    for (lang, data) in parse_rate.get_errors_by_lang().items()
                },
                engine_requested=engine_requested.name,
                findings_by_product=findings_by_product,
            ),
        )

        if self.dry_run:
            logger.info(
                f"Would have sent findings and ignores blob: {json.dumps(findings_and_ignores, indent=4)}"
            )
            logger.info(
                f"Would have sent complete blob: {json.dumps(complete.to_json(), indent=4)}"
            )
            return ScanCompleteResult(True, False, "")
        else:
            logger.debug(
                f"Sending findings and ignores blob: {json.dumps(findings_and_ignores, indent=4)}"
            )

        results_task = progress_bar.add_task("Uploading scan results")
        response = state.app_session.post(
            f"{state.env.semgrep_url}/api/agent/scans/{self.scan_id}/results",
            timeout=state.env.upload_findings_timeout,
            json=findings_and_ignores,
        )

        try:
            response.raise_for_status()

            res = response.json()
            resp_errors = res["errors"]
            for error in resp_errors:
                message = error["message"]
                click.echo(f"Server returned following warning: {message}", err=True)

            if "task_id" in res:
                complete.task_id = res["task_id"]

            progress_bar.update(results_task, completed=100)

        except requests.RequestException as exc:
            raise Exception(f"API server returned this error: {response.text}") from exc

        complete_task = progress_bar.add_task("Finalizing scan")
        # The largest scans we've seen so far can take up to 30
        # minutes to wait for completion. Eventually, this wait may
        # be configurable as we see larger scans and increased backend
        # load.
        try_until = datetime.utcnow() + timedelta(minutes=30)
        slow_down_after = datetime.utcnow() + timedelta(minutes=2)

        while True:
            logger.debug(
                f"Sending /complete {json.dumps(complete.to_json(), indent=4)}"
            )

            if datetime.utcnow() > try_until:
                # let the backend know we won't be trying again
                complete.final_attempt = True

            # mark as complete
            response = state.app_session.post(
                f"{state.env.semgrep_url}/api/agent/scans/{self.scan_id}/complete",
                timeout=state.env.upload_findings_timeout,
                json=complete.to_json(),
            )

            try:
                response.raise_for_status()
            except requests.RequestException:
                raise Exception(
                    f"API server at {state.env.semgrep_url} returned this error: {response.text}"
                )

            ret = response.json()
            success = ret.get("success", False)

            if success or complete.final_attempt:
                progress_bar.update(complete_task, completed=100)
                return ScanCompleteResult(
                    success,
                    bool(ret.get("app_block_override", False)),
                    ret.get("app_block_reason", ""),
                )

            progress_bar.advance(complete_task)
            sleep(5 if datetime.utcnow() < slow_down_after else 30)
