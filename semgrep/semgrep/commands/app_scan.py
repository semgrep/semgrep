from typing import Any

import click

from semgrep.commands.scan import CONTEXT_SETTINGS
from semgrep.commands.scan import scan
from semgrep.commands.scan import scan_options


@click.command(context_settings=CONTEXT_SETTINGS)
@click.pass_context
@scan_options
def app_scan(ctx: click.Context, *args: Any, **kwargs: Any) -> None:
    ctx.forward(scan)
