import json
import os
import tempfile
from dataclasses import dataclass
from logging import getLogger
from pathlib import Path
from typing import Sequence

from semgrep.constants import OutputFormat
from semgrep.error import SemgrepError
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.semgrep_main import main as semgrep_main
from semgrep.types import JsonObject

log = getLogger(__name__)


@dataclass
class RunContext:
    language: str
    targets: Sequence[JsonObject]
    options: JsonObject
    output: JsonObject


def run_pattern(pattern: str, context: RunContext) -> JsonObject:
    context.options = {**context.options, "pattern": pattern, "configs": []}
    return _run_semgrep_with_options(context)


def run_rule(rule: str, context: RunContext) -> JsonObject:
    with tempfile.NamedTemporaryFile(
        mode="w", encoding="utf-8", prefix="semgrep-rule-"
    ) as rule_fd:
        rule_fd.write(rule)
        rule_fd.flush()

        context.options = {
            **context.options,
            "configs": [rule_fd.name],
            "pattern": None,
        }

        return _run_semgrep_with_options(context)


def _run_semgrep_with_options(context: RunContext) -> JsonObject:
    with tempfile.TemporaryDirectory(prefix="semgrep-target-") as target_dir:
        if not _write_targets(context, Path(target_dir)):
            return {}
        return json.loads(_exec_semgrep(context, target_dir))  # type: ignore


def _exec_semgrep(context: RunContext, target_dir: str) -> str:
    # TODO: Create a thin library interface here
    # In the meantime, this is a copy-past of semgrep.semgrep_main.invoke_semgrep

    pwd = os.getcwd()

    try:
        os.chdir(target_dir)  # things like git-awareness are relative to working dir

        output_settings = OutputSettings(
            **{"output_format": OutputFormat.JSON, **context.output}  # type: ignore
        )
        output_handler = OutputHandler(output_settings)

        try:
            (
                filtered_matches_by_rule,
                _semgrep_errors,
                all_targets,
                ignore_log,
                filtered_rules,
                profiler,
                profiling_data,
                shown_severities,
            ) = semgrep_main(
                output_handler=output_handler,
                target=["."],
                lang=context.language,
                no_git_ignore=True,
                **context.options,
            )

            output_handler.has_output = True
            output_handler.all_targets = all_targets
            output_handler.filtered_rules = filtered_rules
            output_handler.rules = output_handler.rules.union(
                filtered_matches_by_rule.keys()
            )
            output_handler.rule_matches = [
                m for ms in filtered_matches_by_rule.values() for m in ms
            ]
            output_handler.profiler = profiler
            output_handler.profiling_data = profiling_data
            output_handler.severities = shown_severities
            output = output_handler._build_output()
        except SemgrepError as e:
            output_handler.all_targets = set()
            output_handler.filtered_rules = []
            output_handler.handle_semgrep_errors([e])
            output = output_handler._build_output()
    finally:
        os.chdir(pwd)

    return output


def _write_targets(context: RunContext, target_dir_path: Path) -> bool:
    for t in context.targets:
        t_name = t.get("name")
        if not isinstance(t_name, str):
            log.error(f"Target must include string 'name' field")
            return False
        t_content = t.get("content")
        if not isinstance(t_content, str):
            log.error(f"Target must include string 'content' field")
            return False
        t_path = Path(t_name)
        # Prevent path resolution vulnerabilities
        if t_path.name != str(t_path):
            log.error(f"Target names must be pure filenames")
            return False
        with (target_dir_path / t_path).open(mode="w", encoding="utf-8") as fd:
            fd.write(t_content)
        log.debug(f"Semgrep target written to '{t_path}'")
    return True
