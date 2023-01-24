#!/usr/bin/env python3
import platform
import sys
from typing import Dict

import click

from semgrep.commands.ci import ci
from semgrep.commands.install import install_deep_semgrep
from semgrep.commands.install import install_semgrep_pro
from semgrep.commands.login import login
from semgrep.commands.login import logout
from semgrep.commands.lsp import lsp
from semgrep.commands.publish import publish
from semgrep.commands.scan import scan
from semgrep.commands.shouldafound import shouldafound
from semgrep.default_group import DefaultGroup
from semgrep.error import FATAL_EXIT_CODE
from semgrep.state import get_state
from semgrep.util import git_check_output
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


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


def abort_if_linux_arm64() -> None:
    """
    Exit with FATAL_EXIT_CODE if the user is running on Linux ARM64.
    Print helpful error message.
    """
    if platform.machine() in {"arm64", "aarch64"} and platform.system() == "Linux":
        logger.error("Semgrep does not support Linux ARM64")
        sys.exit(FATAL_EXIT_CODE)


@click.group(cls=DefaultGroup, default_command="scan", name="semgrep")
@click.help_option("--help", "-h")
@click.pass_context
def cli(ctx: click.Context) -> None:
    """
    To get started quickly, run `semgrep scan --config auto`

    Run `semgrep SUBCOMMAND --help` for more information on each subcommand

    If no subcommand is passed, will run `scan` subcommand by default
    """
    state = get_state()
    state.terminal.init_for_cli()

    abort_if_linux_arm64()

    commands: Dict[str, click.Command] = ctx.command.commands  # type: ignore

    subcommand: str = (
        ctx.invoked_subcommand if ctx.invoked_subcommand in commands else "unset"
    )

    state.app_session.authenticate()
    state.app_session.user_agent.tags.add(f"command/{subcommand}")
    state.metrics.add_feature("subcommand", subcommand)

    maybe_set_git_safe_directories()


cli.add_command(ci)
cli.add_command(login)
cli.add_command(logout)
cli.add_command(publish)
cli.add_command(scan)
cli.add_command(install_semgrep_pro)
# Just for backwards compat
cli.add_command(install_deep_semgrep)
cli.add_command(shouldafound)
cli.add_command(lsp)
