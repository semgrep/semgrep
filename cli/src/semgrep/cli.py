#!/usr/bin/env python3
from typing import Dict

import click

from semgrep.commands.ci import ci
from semgrep.commands.install import install_semgrep_pro
from semgrep.commands.login import login
from semgrep.commands.login import logout
from semgrep.commands.lsp import lsp
from semgrep.commands.publish import publish
from semgrep.commands.scan import scan
from semgrep.default_group import DefaultGroup
from semgrep.state import get_state
from semgrep.verbose_logging import getLogger
from semgrep.constants import DEFAULT_EPILOGUE

logger = getLogger(__name__)


@click.group(cls=DefaultGroup, default_command="scan", name="semgrep", epilog=DEFAULT_EPILOGUE)
@click.help_option("--help", "-h")
@click.pass_context
def cli(ctx: click.Context) -> None:
    """
    Semgrep CLI scans your code for bugs, security and dependency vulnerabilities.

    For more information about Semgrep, visit https://semgrep.dev
    """
    state = get_state()
    state.terminal.init_for_cli()

    commands: Dict[str, click.Command] = ctx.command.commands  # type: ignore

    subcommand: str = (
        ctx.invoked_subcommand if ctx.invoked_subcommand in commands else "unset"
    )

    state.app_session.authenticate()
    state.app_session.user_agent.tags.add(f"command/{subcommand}")
    state.metrics.add_feature("subcommand", subcommand)


cli.add_command(ci)
cli.add_command(login)
cli.add_command(logout)
cli.add_command(publish)
cli.add_command(scan)
cli.add_command(install_semgrep_pro)
cli.add_command(lsp)
