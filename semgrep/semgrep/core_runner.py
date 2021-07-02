import collections
import json
import subprocess
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import List
from typing import Set
from typing import Tuple

from ruamel.yaml import YAML

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.core_exception import CoreException
from semgrep.error import InvalidPatternErrorNoSpan
from semgrep.error import MatchTimeoutError
from semgrep.error import SemgrepError
from semgrep.evaluation import create_output
from semgrep.output import OutputSettings
from semgrep.pattern_match import PatternMatch
from semgrep.profile_manager import ProfileManager
from semgrep.profiling import ProfilingData
from semgrep.profiling import Times
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.target_manager import TargetManager
from semgrep.util import debug_tqdm_write
from semgrep.util import is_debug
from semgrep.util import SEMGREP_PATH
from semgrep.util import sub_run
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


class CoreRunner:
    """
    Handles interactions between semgrep and semgrep-core

    This includes properly invoking semgrep-core and parsing the output
    """

    def __init__(
        self,
        output_settings: OutputSettings,
        allow_exec: bool,
        jobs: int,
        timeout: int,
        max_memory: int,
        timeout_threshold: int,
        optimizations: str,
    ):
        self._output_settings = output_settings
        self._allow_exec = allow_exec
        self._jobs = jobs
        self._timeout = timeout
        self._max_memory = max_memory
        self._timeout_threshold = timeout_threshold
        self._optimizations = optimizations

    def _raise_semgrep_error_from_json(
        self,
        error_json: Dict[str, Any],
    ) -> None:
        """
        See format_output_exception in semgrep O'Caml for details on schema
        """
        error_type = error_json["error"]
        if error_type == "invalid language":
            raise SemgrepError(
                f'{error_json["language"]} was accepted by semgrep but rejected by semgrep-core. {PLEASE_FILE_ISSUE_TEXT}'
            )
        elif error_type == "invalid regexp in rule":
            raise SemgrepError(f'Invalid regexp in rule: {error_json["message"]}')
        elif error_type == "invalid pattern":
            raise InvalidPatternErrorNoSpan(
                rule_id=error_json.get("pattern_id", "<no rule_id>"),
                pattern=error_json.get("pattern", "<no pattern>"),
                language=error_json.get("language", "<no language>"),
            )
        # no special formatting ought to be required for the other types; the semgrep python should be performing
        # validation for them. So if any other type of error occurs, ask the user to file an issue
        else:
            raise SemgrepError(
                f"an internal error occured while invoking semgrep-core while. \n\t{error_type}: {error_json.get('message', 'no message')}\n{PLEASE_FILE_ISSUE_TEXT}"
            )

    def _extract_core_output(
        self, core_run: subprocess.CompletedProcess
    ) -> Dict[str, Any]:
        semgrep_output = core_run.stdout.decode("utf-8", errors="replace")
        semgrep_error_output = core_run.stderr.decode("utf-8", errors="replace")

        # By default, we print semgrep-core's error output, which includes
        # semgrep-core's logging if it was requested via --debug.
        #
        # If semgrep-core prints anything on stderr when running with default
        # flags, it's a bug that should be fixed in semgrep-core.
        #
        logger.debug(
            f"--- semgrep-core stderr ---\n"
            f"{semgrep_error_output}"
            f"--- end semgrep-core stderr ---"
        )

        returncode = core_run.returncode
        output_json = self._parse_core_output(
            core_run, semgrep_output, semgrep_error_output, returncode
        )

        if returncode != 0:
            if "error" in output_json:
                self._raise_semgrep_error_from_json(output_json)
            else:
                self._fail(
                    'non-zero exit status with missing "error" field in json response',
                    core_run,
                    returncode,
                    semgrep_output,
                    semgrep_error_output,
                )

        return output_json

    def _parse_core_output(
        self,
        core_run: subprocess.CompletedProcess,
        semgrep_output: str,
        semgrep_error_output: str,
        returncode: int,
    ) -> Dict[str, Any]:
        # See if semgrep output contains a JSON error that we can decode.
        try:
            return cast(Dict[str, Any], json.loads(semgrep_output))
        except ValueError:
            self._fail(
                "unparseable json output",
                core_run,
                returncode,
                semgrep_output,
                semgrep_error_output,
            )
            return {}  # never reached

    def _fail(
        self,
        reason: str,
        core_run: subprocess.CompletedProcess,
        returncode: int,
        semgrep_output: str,
        semgrep_error_output: str,
    ) -> None:
        # Once we require python >= 3.8, switch to using shlex.join instead
        # for proper quoting of the command line.
        shell_command = " ".join(core_run.args)
        raise SemgrepError(
            f"semgrep-core failed: {reason}\n"
            f"semgrep-core exit code: {returncode}\n"
            f"semgrep-core command: {shell_command}\n"
            f"unexpected non-json output while invoking semgrep-core:\n"
            "--- semgrep-core stdout ---\n"
            f"{semgrep_output}"
            "--- end semgrep-core stdout ---\n"
            "--- semgrep-core stderr ---\n"
            f"{semgrep_error_output}"
            "--- end semgrep-core stderr ---\n"
            f"{PLEASE_FILE_ISSUE_TEXT}"
        )

    def _add_match_times(
        self,
        rule: Rule,
        profiling_data: ProfilingData,
        output_time_json: Dict[str, Any],
    ) -> None:
        """Collect the match times reported by semgrep-core (or spacegrep)."""
        if "targets" in output_time_json:
            for target in output_time_json["targets"]:
                if "match_time" in target and "path" in target:
                    profiling_data.set_run_times(
                        rule,
                        Path(target["path"]),
                        Times(
                            parse_time=target["parse_time"],
                            match_time=target["match_time"],
                            run_time=target["run_time"],
                        ),
                    )
        if "rule_parse_time" in output_time_json:
            profiling_data.set_rule_parse_time(
                rule, output_time_json["rule_parse_time"]
            )

    def _semgrep_core_output_to_rule_matches(
        self, rules: List[Rule], core_match_json: List[Dict[str, Any]]
    ) -> Dict[Rule, List[RuleMatch]]:
        """
        Converts semgrep-core output to RuleMatch objects

        Returns Dict[Rule -> RuleMatch]

        To reuse logic iin evaluation.create_output needs to group
        findings by rule and convert semgrep-core output to PatternMatch objects
        first. When create_output no longer needs to support PatternMatch objects
        can rewrite this.
        """
        rule_id_to_rule: Dict[str, Rule] = {}
        for rule in rules:
            rule_id_to_rule[rule._raw["id"]] = rule

        rule_to_pattern_matches: Dict[
            Rule, List[PatternMatch]
        ] = collections.defaultdict(list)
        for match in core_match_json:
            rule_id = match["check_id"]
            rule = rule_id_to_rule[rule_id]

            pattern_match = PatternMatch(match)
            rule_to_pattern_matches[rule].append(pattern_match)

        rule_to_rule_matches: Dict[Rule, List[RuleMatch]] = {}
        for rule, pattern_matches in rule_to_pattern_matches.items():
            rule_to_rule_matches[rule] = create_output(rule, pattern_matches)

        return rule_to_rule_matches

    def _run_rules_direct_to_semgrep_core(
        self,
        all_rules: List[Rule],
        target_manager: TargetManager,
        profiler: ProfileManager,
    ) -> Tuple[
        Dict[Rule, List[RuleMatch]],
        Dict[Rule, List[Any]],
        List[SemgrepError],
        Set[Path],
        ProfilingData,
    ]:
        logger.debug(f"Passing whole rules directly to semgrep_core")

        outputs: Dict[Rule, List[RuleMatch]] = collections.defaultdict(list)
        errors: List[SemgrepError] = []
        all_targets: Set[Path] = set()
        file_timeouts: Dict[Path, int] = collections.defaultdict(lambda: 0)
        max_timeout_files: Set[Path] = set()

        profiling_data: ProfilingData = ProfilingData()

        from semgrep.core_orchestrator import CoreOrchestrator
        import tqdm

        # cf. for bar_format: https://tqdm.github.io/docs/tqdm/
        with tempfile.TemporaryDirectory() as semgrep_core_ast_cache_dir:
            with tqdm.tqdm(
                total=len(all_rules), bar_format="{l_bar}{bar}|{n_fmt}/{total_fmt}"
            ) as progress_bar:
                for language, rules, targets in CoreOrchestrator().orchestrate(
                    all_rules, target_manager
                ):
                    debug_tqdm_write(f"Running rules {[rule.id for rule in rules]}...")
                    with tempfile.NamedTemporaryFile(
                        "w", suffix=".yaml"
                    ) as rule_file, tempfile.NamedTemporaryFile("w") as target_file:

                        targets = frozenset(
                            target
                            for target in targets
                            if target not in max_timeout_files
                        )

                        # opti: no need to call semgrep-core if no target files
                        if not targets:
                            continue

                        all_targets = all_targets.union(targets)

                        target_file.write("\n".join(map(lambda p: str(p), targets)))
                        target_file.flush()
                        yaml = YAML()
                        yaml.dump({"rules": [rule._raw for rule in rules]}, rule_file)
                        rule_file.flush()

                        cmd = [SEMGREP_PATH] + [
                            "-lang",
                            language.value,
                            "-fast",
                            "-json",
                            "-config",
                            rule_file.name,
                            "-j",
                            str(self._jobs),
                            "-target_file",
                            target_file.name,
                            "-use_parsing_cache",
                            semgrep_core_ast_cache_dir,
                            "-timeout",
                            str(self._timeout),
                            "-max_memory",
                            str(self._max_memory),
                            "-json_time",
                        ]

                        if is_debug():
                            cmd += ["-debug"]

                        core_run = sub_run(
                            cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE
                        )
                        output_json = self._extract_core_output(core_run)
                        # import json
                        # print(json.dumps(output_json["errors"], indent=4))
                        # if "time" in output_json:
                        #     self._add_match_times(
                        #         rule, profiling_data, output_json["time"]
                        #     )

                    # end with tempfile.NamedTemporaryFile(...) ...

                    rule_to_rule_matches = self._semgrep_core_output_to_rule_matches(
                        rules, output_json["matches"]
                    )

                    # Do not just override outputs so will work correctly even if
                    # orchestrator runs rule multiple times (over diff targets presumably)
                    for rule, rule_matches in rule_to_rule_matches.items():
                        outputs[rule].extend(rule_matches)

                    # No need for findings = dedup_output(findings)
                    # This is handled already semgrep-core side

                    # When sending multiple rules to semgrep-core at once
                    # currently do not know which rule caused an error
                    if len(rules) > 1:
                        rule_id = "<multiple rules>"
                    else:
                        rule_id = rules[0].id

                    parsed_errors = [
                        CoreException.from_json(
                            e, language.value, rule_id
                        ).into_semgrep_error()
                        for e in output_json["errors"]
                    ]
                    for err in parsed_errors:
                        if isinstance(err, MatchTimeoutError):
                            file_timeouts[err.path] += 1
                            if (
                                self._timeout_threshold != 0
                                and file_timeouts[err.path] >= self._timeout_threshold
                            ):
                                max_timeout_files.add(err.path)
                    errors.extend(parsed_errors)
                    progress_bar.update(len(rules))
            # end for language ...
        # end for rule ...
        return outputs, {}, errors, all_targets, profiling_data

    # end _run_rules_direct_to_semgrep_core

    def invoke_semgrep(
        self,
        target_manager: TargetManager,
        profiler: ProfileManager,
        rules: List[Rule],
    ) -> Tuple[
        Dict[Rule, List[RuleMatch]],
        Dict[Rule, List[Dict[str, Any]]],
        List[SemgrepError],
        Set[Path],
        ProfilingData,
    ]:
        """
        Takes in rules and targets and retuns object with findings
        """
        start = datetime.now()

        (
            findings_by_rule,
            debug_steps_by_rule,
            errors,
            all_targets,
            profiling_data,
        ) = self._run_rules_direct_to_semgrep_core(rules, target_manager, profiler)

        logger.debug(
            f"semgrep ran in {datetime.now() - start} on {len(all_targets)} files"
        )
        by_severity = collections.defaultdict(list)
        for rule, findings in findings_by_rule.items():
            by_severity[rule.severity.lower()].extend(findings)

        by_sev_strings = [
            f"{len(findings)} {sev}" for sev, findings in by_severity.items()
        ]
        logger.debug(f'findings summary: {", ".join(by_sev_strings)}')

        return (
            findings_by_rule,
            debug_steps_by_rule,
            errors,
            all_targets,
            profiling_data,
        )
