#!/usr/bin/env python3
import os
import subprocess

import click

from semgrep.app import app_session
from semgrep.commands.ci import ci
from semgrep.commands.install import install_deep_semgrep
from semgrep.commands.login import login
from semgrep.commands.login import logout
from semgrep.commands.publish import publish
from semgrep.commands.scan import scan
from semgrep.default_group import DefaultGroup
from semgrep.git import GIT_SH_TIMEOUT
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


@click.group(cls=DefaultGroup, default_command="scan", name="semgrep")
@click.help_option("--help", "-h")
@click.pass_context
def cli(ctx: click.Context) -> None:
    """
    To get started quickly, run `semgrep scan --config auto`
    """
    app_session.authenticate()
    app_session.user_agent.tags.add(
        f"command/{ctx.invoked_subcommand}"
        if ctx.invoked_subcommand in ctx.command.commands  # type: ignore
        else "command/unset"
    )

    workspace = os.getenv("GITHUB_WORKSPACE")
    if workspace:
        try:
            output = subprocess.run(
                ["git", "config", "--global", "--add", "safe.directory", workspace],
                check=True,
                encoding="utf-8",
                capture_output=True,
                timeout=GIT_SH_TIMEOUT,
            )
        except Exception as e:
            logger.info(
                f"Git safe directory workaround failed. Git commands might fail: {e}"
            )


cli.add_command(ci)
cli.add_command(login)
cli.add_command(logout)
cli.add_command(publish)
cli.add_command(scan)
cli.add_command(install_deep_semgrep)
