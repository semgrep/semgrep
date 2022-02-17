import os
from typing import Sequence
from typing import Tuple

import click

import semgrep.semgrep_main
from semgrep.constants import OutputFormat
from semgrep.constants import RuleSeverity
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.formatter.text import TextFormatter
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.rule_match import RuleMatch
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


@click.command()
@click.option("--lang", "-l", help="Parse pattern and all files in specified language.")
@click.argument("target", nargs=-1, type=click.Path(allow_dash=True))
def parse(lang: str, target: Tuple[str, ...]) -> None:
    """
    Tries to parse all files. Shows which files have problems parsing in language
    """
    semgrep.util.set_flags(verbose=False, debug=False, quiet=False, force_color=False)
    target_sequence: Sequence[str] = list(target) if target else [os.curdir]
    pattern = "$X = 12345"

    output_format = OutputFormat.JSON
    output_settings = OutputSettings(
        output_format=output_format,
    )
    output_handler = OutputHandler(output_settings)

    try:
        (
            filtered_matches_by_rule,
            all_targets,
            ignore_log,
            filtered_rules,
            profiler,
            profiling_data,
            shown_severities,
        ) = semgrep.semgrep_main.main(
            output_handler=output_handler,
            target=target_sequence,
            pattern=pattern,
            lang=lang,
            configs=[],
            jobs=1,  # __get_cpu_count(),
            no_git_ignore=True,  # pass as arg
            optimizations="none",
        )
    except SemgrepError as e:
        output_handler.handle_semgrep_errors([e])
        output_handler.output({}, all_targets=set(), filtered_rules=[])
        raise e

    click.echo(
        f"There were issues parsing the following files as {lang}. Each file followed by lines that caused parsing issue:"
    )
    for core_error in output_handler.error_set:
        if (
            isinstance(core_error, SemgrepCoreError)
            # and core_error.error_type == "Syntax error" # TODO sometimes errors with Fatal Error
        ):
            rm = RuleMatch(
                id="-",
                path=core_error.path,
                start=core_error.start,
                end=core_error.end,
                message=core_error.message,
                severity=RuleSeverity.INFO,
                fix=None,
                fix_regex=None,
                extra={},
                metadata={},
            )
            click.echo(
                "\n".join(TextFormatter._build_text_output([rm], True, None, None))
            )
