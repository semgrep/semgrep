import time
from typing import Any
from typing import Iterable
from typing import Sequence

import click

from semgrep.commands.login import Authentication
from semgrep.commands.scan import CONTEXT_SETTINGS
from semgrep.commands.scan import scan
from semgrep.commands.scan import scan_options
from semgrep.semgrep_app import ScanHandler

# These patterns are excluded via --exclude regardless of other ignore configuration
ALWAYS_EXCLUDE_PATTERNS = [".semgrep/", ".semgrep_logs/"]

# These patterns are excluded via --exclude unless the user provides their own .semgrepignore
DEFAULT_EXCLUDE_PATTERNS = ["test/", "tests/", "*_test.go"]


def yield_valid_patterns(patterns: Iterable[str]) -> Iterable[str]:
    for pattern in patterns:
        pattern = pattern.strip()

        if not pattern:
            continue
        if pattern.startswith("#"):
            continue

        yield pattern


def yield_exclude_args(requested_patterns: Sequence[str]) -> Iterable[str]:
    patterns = [*yield_valid_patterns(requested_patterns), *ALWAYS_EXCLUDE_PATTERNS]
    if SEMGREPIGNORE_PATH.is_file():
        patterns.extend(DEFAULT_EXCLUDE_PATTERNS)

    for pattern in patterns:
        yield from ["--exclude", pattern]


@click.command(context_settings=CONTEXT_SETTINGS)
@click.pass_context
@scan_options
def app_scan(ctx: click.Context, *args: Any, **kwargs: Any) -> None:
    token = Authentication.get_token()
    assert token
    scan_handler = ScanHandler(token)

    metadata = {"repository": "returntocorp/semgrep"}
    scan_handler.start_scan(metadata)

    kwargs["exclude"] = (*kwargs["exclude"], *scan_handler.ignore_patterns)

    try:
        start = time.time()
        ret = ctx.invoke(scan, **kwargs)
        total_time = time.time() - start
    except Exception as e:
        exit_code = scan_handler.report_failure(exit_code)
        sys.exit(exit_code)

    if ret is not None:
        matches_by_rule, rules, targets = ret
        # if report, finish scan, check fail_open
        scan_handler.report_findings(matches_by_rule, rules, targets, total_time)
