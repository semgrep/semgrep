import sys
import time
from pathlib import Path
from typing import Any
from typing import Iterable
from typing import Sequence

import click

from semgrep.commands.login import Authentication
from semgrep.commands.scan import CONTEXT_SETTINGS
from semgrep.commands.scan import scan
from semgrep.commands.scan import scan_options
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.ignores import IGNORE_FILE_NAME
from semgrep.semgrep_app import ScanHandler
from semgrep.util import set_flags
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)

# These patterns are excluded via --exclude regardless of other ignore configuration
ALWAYS_EXCLUDE_PATTERNS = [".semgrep/", ".semgrep_logs/"]

# These patterns are excluded via --exclude unless the user provides their own .semgrepignore
DEFAULT_EXCLUDE_PATTERNS = ["test/", "tests/", "*_test.go"]


def yield_valid_patterns(patterns: Iterable[str]) -> Iterable[str]:
    """
    Parses patterns from semgrep.dev and returns the lines that
    are non-empty and do not start with #
    """
    for pattern in patterns:
        pattern = pattern.strip()

        if not pattern:
            continue
        if pattern.startswith("#"):
            continue

        yield pattern


def yield_exclude_paths(requested_patterns: Sequence[str]) -> Iterable[str]:
    patterns = [*yield_valid_patterns(requested_patterns), *ALWAYS_EXCLUDE_PATTERNS]
    if Path(IGNORE_FILE_NAME).is_file() and not requested_patterns:
        patterns.extend(DEFAULT_EXCLUDE_PATTERNS)

    yield from patterns


@click.command(context_settings=CONTEXT_SETTINGS)
@click.pass_context
@scan_options
def ci(
    ctx: click.Context,
    verbose: bool,
    debug: bool,
    quiet: bool,
    force_color: bool,
    *args: Any,
    **kwargs: Any,
) -> None:
    set_flags(verbose=verbose, debug=debug, quiet=quiet, force_color=force_color)

    token = Authentication.get_token()
    if not token:
        logger.info("run `semgrep login` before using `semgrep ci`")
        sys.exit(1)
    if not Authentication.is_valid_token(token):
        logger.info(
            "API token not valid. Try to run `semgrep logout` and `semgrep login` again.",
        )
        sys.exit(1)

    scan_handler = ScanHandler(token)
    metadata = {"repository": "returntocorp/semgrep"}

    try:
        scan_handler.start_scan(metadata)
    except Exception as e:
        logger.info(f"Failed to start scan so exiting...")
        exit_code = scan_handler.fail_open_exit_code(metadata["repository"], 1)
        if exit_code == 0:
            logger.info(
                "Fail Open is configured for this repository on semgrep.dev so exiting with code 0",
            )
        sys.exit(exit_code)

    # Set config to rules if default of (,) or is --config policy
    if not kwargs.get("config") or kwargs.get("config") == ("policy",):
        kwargs["config"] = (scan_handler.scan_rules_url,)
    else:
        logger.info(
            "`semgrep ci` can only run with policies configured in semgrep.dev you may be looking for `semgrep scan`"
        )
        sys.exit(2)

    # Append ignores configured on semgrep.dev
    kwargs["exclude"] = (
        *kwargs["exclude"],
        *yield_exclude_paths(scan_handler.ignore_patterns),
    )

    try:
        start = time.time()
        ret = ctx.invoke(
            scan,
            debug=debug,
            verbose=verbose,
            quiet=quiet,
            force_color=force_color,
            **kwargs,
        )
        total_time = time.time() - start
    except Exception as e:
        logger.info(f"Encountered error when running rules: {e}")
        if isinstance(e, SemgrepError):
            exit_code = e.code
        else:
            exit_code = FATAL_EXIT_CODE
        exit_code = scan_handler.report_failure(exit_code)
        if exit_code == 0:
            logger.info(
                "Fail Open is configured for this repository on semgrep.dev so exiting with code 0",
            )
        sys.exit(exit_code)

    if ret is not None:
        logger.info("Reporting findings to semgrep.dev ...")
        matches_by_rule, errors, rules, targets = ret
        # if report, finish scan, check fail_open
        scan_handler.report_findings(
            matches_by_rule, errors, rules, targets, total_time
        )
        logger.info(f"Success.")

        # if metadata.event_name in audit_on:
        #     logger.info(
        #         f"Audit mode is on for {metadata.event_name}, so exiting with code 0 even if matches found",
        #         err=True,
        #     )
        #     sys.exit(0)
        # else:
        #     logger.info(f"Blocking matches found so exiting with code 1")
        #     sys.exit(1)
    else:
        logger.info(f"Semgrep encountered unexpected issue while running ci scan")
        sys.exit(1)
