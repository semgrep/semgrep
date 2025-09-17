#
# Copyright (c) 2020-2025 Semgrep Inc.
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
##############################################################################
# Prelude
##############################################################################
# pysemgrep command dispatch (semgrep scan vs semgrep ci vs ...)
#
from typing import Dict

import click

from semgrep.commands.ci import ci
from semgrep.commands.install import install_semgrep_pro
from semgrep.commands.login import login
from semgrep.commands.mcp import semgrep_mcp
from semgrep.commands.publish import publish
from semgrep.commands.scan import scan
from semgrep.default_group import DefaultGroup
from semgrep.git import git_check_output
from semgrep.state import get_state
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


##############################################################################
# Helpers
##############################################################################


def maybe_set_git_safe_directories() -> None:
    """
    Configure Git to be willing to run in any directory when we're in Docker.

    In docker, every path is trusted:
    - the user explicitly mounts their trusted code directory
    - r2c provides every other path

    More info:
    - https://github.blog/2022-04-12-git-security-vulnerability-announced/
    - https://github.com/actions/checkout/issues/766
    """
    env = get_state().env
    if not env.in_docker:
        return

    try:
        # "*" is used over Path.cwd() in case the user targets an absolute path instead of setting --workdir
        git_check_output(["git", "config", "--global", "--add", "safe.directory", "*"])
    except Exception as e:
        logger.info(
            f"Semgrep failed to set the safe.directory Git config option. Git commands might fail: {e}"
        )


##############################################################################
# Entry point
##############################################################################


@click.group(cls=DefaultGroup, default_command="scan", name="semgrep")
@click.help_option("--help", "-h")
@click.pass_context
def cli(ctx: click.Context) -> None:
    state = get_state()
    state.terminal.init_for_cli()

    commands: Dict[str, click.Command] = ctx.command.commands  # type: ignore
    subcommand: str = (
        ctx.invoked_subcommand if ctx.invoked_subcommand in commands else "unset"
    )

    state.app_session.authenticate()
    state.app_session.user_agent.tags.add(f"command/{subcommand}")
    state.metrics.add_feature("subcommand", subcommand)

    maybe_set_git_safe_directories()


cli.add_command(cmd=ci)
cli.add_command(cmd=login)
cli.add_command(cmd=publish)
cli.add_command(cmd=scan, name="scan")
cli.commands["scan"].help = "Scan code using Semgrep rules (default)"
cli.add_command(cmd=semgrep_mcp, name="mcp")
cli.add_command(cmd=install_semgrep_pro)
