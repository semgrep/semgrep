#
# Copyright (c) 2022-2025 Semgrep Inc.
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
# Handle communication of findings / errors to semgrep.app
import json
import os
import sys
from collections import Counter
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
from typing import Union

import click
import requests

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.parsers.util import DependencyParserError
from semgrep import __VERSION__
from semgrep import telemetry
from semgrep.app.project_config import ProjectConfig
from semgrep.constants import TOO_MUCH_DATA
from semgrep.constants import USER_FRIENDLY_PRODUCT_NAMES
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.parsing_data import ParsingData
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.state import get_state
from semgrep.subproject import (
    subproject_to_stats,
)
from semgrep.target_manager import ALL_PRODUCTS
from semgrep.telemetry import scan_info_to_attrs
from semgrep.types import FilteredMatches
from semgrep.types import Target
from semgrep.types import TargetInfo
from semgrep.verbose_logging import getLogger

if TYPE_CHECKING:
    from semgrep.engine import EngineType
    from rich.progress import Progress
logger = getLogger(__name__)


class _ConfigPollTimeout(Exception):
    """
    Raised when v2 config polling times out for one POST attempt.
    The backend may have silently dropped the scan job; the POST can be retried.

    Hard backend failures (Failure status) are NOT wrapped in this exception —
    they propagate as plain Exception and bypass the POST retry loop entirely.
    """


def prepare_matches_for_app(matches: list[RuleMatch]) -> list[RuleMatch]:
    # we want date stamps assigned by the app to be assigned such that the
    # current sort by relevant_since results in findings within a given scan
    # appear in an intuitive order.  this requires reversed ordering here.
    matches.reverse()

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
    return sorted(matches, key=lambda match: sort_order[match.severity.value])


class ScanHandler:
    def __init__(
        self,
        *,
        enable_transitive_reachability: Optional[bool],
        dry_run: bool = False,
        partial_output: Optional[Path] = None,
        dump_scan_id_path: Optional[Path] = None,
        enable_mal_deps: bool = False,
        dump_scan_config_path: Path | None = None,
        load_saved_scan_config_path: Path | None = None,
    ) -> None:
        """
        When dry_run is True, semgrep ci would get the config from the app,
        run the scans, but would not upload the results.

        When partial_output is not None, the scan results for semgrep ci
        will also be saved to disk on the path that is specified.

        :param enable_transitive_reachability: if this parameter
        is not None, override the transitive_reachability_enabled setting
        obtained from EngineConfiguration (from the Semgrep App server),
        and enable or disable transitive reachability accordingly.
        :param enable_mal_deps: Override to enable malicious dependency
        rules for this scan, even if disabled at the deployment level.
        :param dump_scan_config_path: Path to save the scan config to for later use with
            load_saved_scan_config_path.
        :param load_saved_scan_config_path: Path to a scan config previously dumped with
            dump_scan_config_path. If provided, loads scan config from
            the path without reaching out to the app.
        """
        state = get_state()
        self.local_id = str(state.local_scan_id)
        self.scan_metadata = out.ScanMetadata(
            cli_version=out.Version(__VERSION__),
            unique_id=out.Uuid(self.local_id),
            requested_products=[],
            dry_run=dry_run,
            sms_scan_id=state.env.sms_scan_id,
            enable_mal_deps=enable_mal_deps if enable_mal_deps else None,
        )
        self.scan_response: Optional[out.ScanResponse] = None
        self.dry_run = dry_run
        self._scan_params: str = ""
        self.ci_scan_results: Optional[out.CiScanResults] = None
        self.partial_output = partial_output
        self.dump_scan_id_path = dump_scan_id_path
        self.enable_transitive_reachability = enable_transitive_reachability

        self.dump_scan_config_path = dump_scan_config_path
        self.load_saved_scan_config_path = load_saved_scan_config_path

    @property
    def scan_id(self) -> Optional[int]:
        if self.scan_response:
            return self.scan_response.info.id
        return None

    @property
    def always_suppress_errors(self) -> bool:
        """
        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return self.scan_response.engine_params.always_suppress_errors
        return False

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
    def resolve_all_deps_in_diff_scan(self) -> bool:
        """
        Normally, diff scans will resolve only the dependencies that are relevant to the changes
        in the diff. If this flag is set, a diff scan will resolve all dependencies and include
        the in the response to the app.

        Separate property for easy of mocking in test
        """
        if self.scan_response:
            return self.scan_response.engine_params.scan_all_deps_in_diff_scan
        return True

    @property
    def symbol_analysis(self) -> bool:
        """
        Collect symbol analysis in scan
        """
        if self.scan_response:
            return self.scan_response.engine_params.symbol_analysis
        return False

    @property
    def fips_mode(self) -> bool:
        """
        Is this scan running in a FIPS environment?
        """
        if self.scan_response:
            return self.scan_response.config.fips_mode
        return False

    @property
    def nosemgrep_disabled(self) -> bool:
        """
        Has the org disabled 'nosemgrep' inline ignore comments for this scan?
        """
        if self.scan_response:
            return self.scan_response.config.nosemgrep_disabled
        return False

    @property
    def project_merge_base(self) -> Optional[str]:
        """
        If the app tells us a merge base let's use it.
        """
        if self.scan_response and self.scan_response.config.project_merge_base:
            return self.scan_response.config.project_merge_base.value
        return None

    @property
    def ptt_enabled(self) -> bool:
        """
        Separate property for easy of mocking in test

        If path to transitivity has been enabled for a deployment, default dependency resolution
        will be overridden and the CLI will attempt to generate a dependency graph for ecosystems
        which graph generation has been implemented for.
        """
        if self.scan_response:
            return self.scan_response.engine_params.path_to_transitivity
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
    def transitive_reachability_enabled(self) -> bool:
        """
        Separate property for easy of mocking in test

        CLI flags override user config
        """
        if self.enable_transitive_reachability is not None:
            return self.enable_transitive_reachability
        if self.scan_response:
            return self.scan_response.engine_params.transitive_reachability_enabled
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

    def _handle_scan_response(self, scan_response: out.ScanResponse) -> None:
        """
        Common logic for handling a scan response after receiving config from the server.
        Sets the scan_response, logs it, dumps scan_id if needed, and updates telemetry.
        """
        self.scan_response = scan_response

        # the rules field below can be huge so better to not log it
        x = self.scan_response
        save = x.config.rules
        x.config.rules = out.RawJson(TOO_MUCH_DATA)
        logger.debug(f"Scan started: {json.dumps(x.to_json(), indent=4)}")
        x.config.rules = save

        if self.dump_scan_id_path and self.scan_id:
            self.dump_scan_id_path.parent.mkdir(parents=True, exist_ok=True)
            self.dump_scan_id_path.write_text(str(self.scan_id))

        if self.dump_scan_config_path:
            self.dump_scan_config_path.parent.mkdir(parents=True, exist_ok=True)
            self.dump_scan_config_path.write_text(self.scan_response.to_json_string())
            logger.info(f"Scan config saved to {self.dump_scan_config_path}")

        get_state().telemetry.add_resource_attrs(
            scan_info_to_attrs(self.scan_response.info)
        )

    def _raise_if_request_failed(self, response: requests.Response) -> None:
        """
        Handle HTTP response errors from the backend.

        :param response: The HTTP response to check
        :raises SystemExit: On 401 (invalid API key)
        :raises Exception: On 404 or other HTTP errors
        """
        state = get_state()

        if response.status_code == 401:
            logger.info(
                "API token not valid. Try to run `semgrep logout` and `semgrep login` again. "
                "Or in CI, ensure your SEMGREP_APP_TOKEN variable is set correctly.",
            )
            sys.exit(INVALID_API_KEY_EXIT_CODE)

        if response.status_code == 404:
            raise Exception(
                "Failed to create a scan with given token and deployment_id. "
                "Please make sure they have been set correctly. "
                f"API server at {state.env.semgrep_url} returned this response: {response.text}"
            )

        try:
            response.raise_for_status()
        except requests.RequestException:
            raise Exception(
                f"API server at {state.env.semgrep_url} returned this error: {response.text}"
            )

    @telemetry.trace()
    def start_scan(
        self, project_metadata: out.ProjectMetadata, project_config: ProjectConfig
    ) -> None:
        """Start a scan and get configuration from the server."""
        span = telemetry.get_current_span()

        if self.load_saved_scan_config_path:
            if not self.load_saved_scan_config_path.exists():
                raise ValueError(
                    f"Saved scan config not found: {self.load_saved_scan_config_path}"
                )
            raw = self.load_saved_scan_config_path.read_text()
            scan_response = out.ScanResponse.from_json_string(raw)
            self._handle_scan_response(scan_response)
            logger.info(
                f"Scan config loaded from {self.load_saved_scan_config_path} (scan_id={self.scan_id})"
            )
            span.set_attribute("scan.loaded_saved_config", True)
            return

        response = self.start_scan_v2(project_metadata, project_config)
        self._handle_scan_response(response)

    # coupling(backend): if you change this you must change poll_scan_config_v2 in Semgrep_App.ml
    @telemetry.trace()
    def start_scan_v2(
        self, project_metadata: out.ProjectMetadata, project_config: ProjectConfig
    ) -> out.ScanResponse:
        """
        Create a scan using the v2 endpoint with async config generation.

        1. POST to /api/cli/v2/scans to create scan (returns scan info immediately)
        2. Poll GET /api/cli/v2/scans/{scan_request_id}/config for up to
           SEMGREP_V2_POLL_TIMEOUT_SECONDS per attempt
        3. If polling times out (backend likely dropped the job), retry the POST up to
           SEMGREP_V2_POST_MAX_ATTEMPTS times, subject to an overall
           SEMGREP_V2_OVERALL_TIMEOUT_MINUTES cap.
        4. Construct and return ScanResponse from the combined responses.

        Note: scan_request_id is the client-generated unique_id, not the server's scan.id.
        The same scan_request_id is reused across retries because the POST is idempotent.
        """
        state = get_state()
        poll_timeout_seconds = state.env.v2_poll_timeout_seconds
        max_attempts = state.env.v2_post_max_attempts
        overall_timeout_minutes = state.env.v2_overall_timeout_minutes
        span = telemetry.get_current_span()

        # scan_request_id is the client-generated unique ID; stable across retries
        scan_request_id = self.scan_metadata.unique_id.value
        span.set_attribute("scan.v2.scan_request_id", scan_request_id)

        request = out.CreateScanRequestV2(
            scan_metadata=self.scan_metadata,
            project_metadata=project_metadata,
            project_config=project_config.to_CiConfigFromRepo(),
        )

        overall_deadline = datetime.now().replace(tzinfo=None) + timedelta(
            minutes=overall_timeout_minutes
        )

        # saved so we can log the last exception after the final attempt times out
        last_timeout_exc: Optional[_ConfigPollTimeout] = None
        for post_attempt in range(1, max_attempts + 1):
            span.set_attribute("scan.v2.post_attempt", post_attempt)

            logger.debug(
                f"Starting scan (v2) attempt {post_attempt}/{max_attempts} "
                f"with request_id={scan_request_id}: {json.dumps(request.to_json(), indent=4)}"
            )

            create_response = state.app_session.post(
                f"{state.env.semgrep_url}/api/cli/v2/scans",
                json=request.to_json(),
            )
            self._raise_if_request_failed(create_response)

            create_scan_response = out.CreateScanResponseV2.from_json(
                create_response.json()
            )
            scan_info = create_scan_response.info

            # Note: scan_info.id can be null for dry runs
            if scan_info.id:
                logger.debug(f"Scan created with ID: {scan_info.id}")
                span.set_attribute("scan.v2.scan_id", scan_info.id)

            remaining_seconds = (
                overall_deadline - datetime.now().replace(tzinfo=None)
            ).total_seconds()
            if remaining_seconds <= 0:
                break

            poll_timeout = min(poll_timeout_seconds, remaining_seconds)
            try:
                return self._poll_for_config_v2(
                    scan_request_id, scan_info, poll_timeout
                )
            except _ConfigPollTimeout as e:
                last_timeout_exc = e
                remaining_seconds = (
                    overall_deadline - datetime.now().replace(tzinfo=None)
                ).total_seconds()
                if post_attempt < max_attempts and remaining_seconds > 0:
                    logger.warning(
                        f"Config not ready after {poll_timeout:.0f}s "
                        f"(attempt {post_attempt}/{max_attempts}), retrying POST"
                    )
                else:
                    break  # deadline exceeded or final attempt — don't make another POST

        cause = (
            f": {last_timeout_exc}"
            if last_timeout_exc
            else " (deadline reached before polling started)"
        )
        raise Exception(
            f"Config generation timed out after {post_attempt} POST attempts "
            f"(scan_request_id={scan_request_id}){cause}"
        )

    def _poll_for_config_v2(
        self,
        scan_request_id: str,
        scan_info: out.ScanInfo,
        timeout_seconds: float,
    ) -> out.ScanResponse:
        """
        Poll GET /api/cli/v2/scans/{scan_request_id}/config until Success, Failure,
        or timeout_seconds elapses.

        Raises:
            _ConfigPollTimeout: config is still Pending after timeout (retryable via POST)
            Exception: backend explicitly reported Failure (not retryable)
        """
        state = get_state()
        span = telemetry.get_current_span()

        start_time = datetime.now().replace(tzinfo=None)
        deadline = start_time + timedelta(seconds=timeout_seconds)

        # Poll interval bounds; server can recommend a value within these
        server_poll_interval_seconds = 5
        minimum_poll_interval_seconds = 1
        maximum_poll_interval_seconds = 60

        poll_attempt = 0
        while datetime.now().replace(tzinfo=None) < deadline:
            poll_attempt += 1
            span.set_attribute("scan.v2.poll_attempts", poll_attempt)

            logger.debug("Polling for scan config")

            config_response = state.app_session.get(
                f"{state.env.semgrep_url}/api/cli/v2/scans/{scan_request_id}/config",
                timeout=state.env.upload_findings_timeout,
            )

            self._raise_if_request_failed(config_response)

            get_config_response = out.GetConfigResponseV2.from_json(
                config_response.json()
            )
            status = get_config_response.status

            # Use server's recommended poll interval if provided
            if polling_info := get_config_response.polling:
                server_poll_interval_seconds = polling_info.recommended_wait_seconds

            if isinstance(status.value, out.Success):
                # Config is ready
                span.set_attribute("scan.v2.config_ready", True)

                if (
                    not get_config_response.config
                    or not get_config_response.engine_params
                ):
                    raise Exception(
                        f"Config status is Success but config or engine_params is missing"
                    )

                return out.ScanResponse(
                    info=scan_info,
                    config=get_config_response.config,
                    engine_params=get_config_response.engine_params,
                )

            elif isinstance(status.value, out.Failure):
                # Config generation failed
                span.set_attribute("scan.v2.config_status", "failure")
                raise Exception(
                    f"Config generation failed for scan_request_id={scan_request_id}"
                )

            elif isinstance(status.value, out.Pending):
                # Still pending - continue polling
                span.set_attribute("scan.v2.config_status", "pending")

            # Never wait less than minimum poll interval to avoid hammering the server
            sleep(
                min(
                    max(server_poll_interval_seconds, minimum_poll_interval_seconds),
                    maximum_poll_interval_seconds,
                )
            )

        # Timeout - config never became ready
        elapsed = (datetime.now().replace(tzinfo=None) - start_time).seconds
        raise _ConfigPollTimeout(
            f"Config still pending after {elapsed}s "
            f"(scan_request_id={scan_request_id}, {poll_attempt} poll attempts)"
        )

    @telemetry.trace()
    def report_failure(self, exit_code: int) -> None:
        """
        Send semgrep cli non-zero exit code information to server
        and return what exit code semgrep should exit with.
        """
        state = get_state()

        if self.partial_output:
            self.partial_output.write_text(
                out.PartialScanResult(
                    out.PartialScanError(
                        out.CiScanFailure(exit_code=exit_code, stderr="")
                    )
                ).to_json_string()
            )

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

    @telemetry.trace()
    def report_findings(
        self,
        *,
        matches_by_rule: FilteredMatches,
        rules: List[Rule],
        targets: Set[TargetInfo],
        skipped_paths: Set[Path],
        renamed_targets: Set[Path],
        ignored_targets: FrozenSet[Target],
        cli_suggested_exit_code: int,
        parse_rate: ParsingData,
        total_time: float,
        commit_date: str,
        lockfile_dependencies: Dict[str, List[out.FoundDependency]],
        dependency_parser_errors: List[DependencyParserError],
        all_subprojects: List[Union[out.UnresolvedSubproject, out.ResolvedSubproject]],
        contributions: out.Contributions,
        engine_requested: "EngineType",
        progress_bar: "Progress",
        disable_nosem: bool,
    ) -> out.CiScanCompleteResponse:
        """
        commit_date here for legacy reasons. epoch time of latest commit

        Returns (success, block_scan, block_reason)
        """
        state = get_state()
        rule_ids = [out.RuleId(r.id) for r in rules]
        # Re-partition by is_ignored: the .kept/.removed split isn't
        # authoritative — under --sarif (or --disable-nosem) suppressed
        # matches land in .kept.
        all_matches_in_scan = [
            match
            for ms_by_rule in (matches_by_rule.kept, matches_by_rule.removed)
            for matches_of_rule in ms_by_rule.values()
            for match in matches_of_rule
        ]
        if disable_nosem:
            findings_matches = all_matches_in_scan
            ignored_matches: List[RuleMatch] = []
        else:
            findings_matches = [
                m for m in all_matches_in_scan if not m.match.extra.is_ignored
            ]
            ignored_matches = [
                m for m in all_matches_in_scan if m.match.extra.is_ignored
            ]
        all_matches = prepare_matches_for_app(findings_matches)
        all_ignored_matches = prepare_matches_for_app(ignored_matches)

        # Autofix is currently the only toggle in the App that
        # indicates we are going to store your code. Until we
        # have a dedicated toggle that allows users to opt-in
        # to us storing their code we ommit code unless autofix
        # is set.

        findings = [
            match.to_app_finding_format(
                commit_date,
                remove_dataflow_content=not self.autofix,
            )
            for match in all_matches
        ]
        ignores = [
            match.to_app_finding_format(
                commit_date,
                remove_dataflow_content=not self.autofix,
            )
            for match in all_ignored_matches
        ]
        self.ci_scan_results = out.CiScanResults(
            # We used to send SCM tokens (e.g GITHUB_TOKEN) to the app as fallback
            # but we now no longer depend on this fallback as much.
            # see ENGINE-2729.
            token=None,
            findings=findings,
            ignores=ignores,
            searched_paths=[
                out.Fpath(str(t.fpath)) for t in sorted(targets, key=lambda x: x.fpath)
            ],
            renamed_paths=[out.Fpath(str(rt)) for rt in sorted(renamed_targets)],
            skipped_paths=[out.Fpath(str(sp)) for sp in sorted(skipped_paths)],
            rule_ids=rule_ids,
            contributions=contributions,
        )
        if self.dependency_query:
            self.ci_scan_results.dependencies = out.CiScanDependencies(
                lockfile_dependencies
            )

        findings_and_ignores = self.ci_scan_results.to_json()

        if any(
            isinstance(match.severity.value, out.Experiment)
            for match in all_ignored_matches
        ):
            logger.info("Some experimental rules were run during execution.")

        ignored_ext_freqs = Counter(
            [os.path.splitext(target.fpath)[1] for target in ignored_targets]
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
        for r, f in matches_by_rule.kept.items():
            # NOTE: For parity with metrics.py, we are using the human-readable product name,
            #  (i.e. code) and falling back to the internal json string (i.e. sast) if we
            #  somehow drift out of sync with the product enum.
            name = USER_FRIENDLY_PRODUCT_NAMES.get(r.product, r.product.to_json())
            findings_by_product[f"{name}"] += len(f)

        subproject_stats = [
            subproject_to_stats(subproject) for subproject in all_subprojects
        ]

        complete = out.CiScanComplete(
            dependencies=out.CiScanDependencies(value=lockfile_dependencies),
            exit_code=cli_suggested_exit_code,
            dependency_parser_errors=dependency_parser_errors,
            stats=out.CiScanCompleteStats(
                findings=len(
                    [match for match in all_matches if not match.from_transient_scan]
                ),
                # We do not report errors anymore since they are large and have
                # caused issues in the past with overloading api endpoints
                #
                # Also, we now use opentelemetry to report these, so they're not
                # useful to us as it stands
                # TODO: Remove this from the interface file?
                errors=[],
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
                supply_chain_stats=out.SupplyChainStats(subproject_stats),
            ),
        )

        if self.partial_output:
            self.partial_output.write_text(
                out.PartialScanResult(
                    out.PartialScanOk((self.ci_scan_results, complete)),
                ).to_json_string()
            )

        if self.dry_run:
            logger.info(
                f"Would have sent findings and ignores blob: {json.dumps(findings_and_ignores, indent=4)}"
            )
            logger.info(
                f"Would have sent complete blob: {json.dumps(complete.to_json(), indent=4)}"
            )
            return out.CiScanCompleteResponse(success=True)

        # old: was also logging {json.dumps(findings_and_ignores, indent=4)}
        # alt: save it in ~/.semgrep/logs/findings_and_ignores.json?
        logger.debug(f"Sending findings and ignores blob")

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
        now = datetime.now().replace(tzinfo=None)
        try_until = now + timedelta(minutes=30)
        slow_down_after = now + timedelta(minutes=2)

        while True:
            # old: was also logging {json.dumps(complete.to_json(), indent=4)}
            # alt: save it in ~/.semgrep/logs/complete.json?
            logger.debug(f"Sending /complete")

            if datetime.now().replace(tzinfo=None) > try_until:
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

            ret = out.CiScanCompleteResponse.from_json(response.json())
            success = ret.success

            if success or complete.final_attempt:
                progress_bar.update(complete_task, completed=100)
                return ret
            progress_bar.advance(complete_task)
            sleep(5 if datetime.now().replace(tzinfo=None) < slow_down_after else 30)
