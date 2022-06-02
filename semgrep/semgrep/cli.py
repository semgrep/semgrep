#!/usr/bin/env python3
import subprocess

import click

from semgrep.commands.ci import ci
from semgrep.commands.install import install_deep_semgrep
from semgrep.commands.login import login
from semgrep.commands.login import logout
from semgrep.commands.publish import publish
from semgrep.commands.scan import scan
from semgrep.commands.shouldafound import shouldafound
from semgrep.default_group import DefaultGroup
from semgrep.git import GIT_SH_TIMEOUT
from semgrep.state import get_state
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
        subprocess.run(
            # "*" is used over Path.cwd() in case the user targets an absolute path instead of setting --workdir
            ["git", "config", "--global", "--add", "safe.directory", "*"],
            check=True,
            encoding="utf-8",
            timeout=GIT_SH_TIMEOUT,
        )
    except Exception as e:
        logger.info(
            f"Semgrep failed to set the safe.directory Git config option. Git commands might fail: {e}"
        )


@click.group(cls=DefaultGroup, default_command="scan", name="semgrep")
@click.help_option("--help", "-h")
@click.pass_context
def cli(ctx: click.Context) -> None:
    """
    To get started quickly, run `semgrep scan --config auto`
    """
    state = get_state()

    state.app_session.authenticate()
    state.app_session.user_agent.tags.add(
        f"command/{ctx.invoked_subcommand}"
        if ctx.invoked_subcommand in ctx.command.commands  # type: ignore
        else "command/unset"
    )

    maybe_set_git_safe_directories()


cli.add_command(ci)
cli.add_command(login)
cli.add_command(logout)
cli.add_command(publish)
cli.add_command(scan)
cli.add_command(install_deep_semgrep)
cli.add_command(shouldafound)
