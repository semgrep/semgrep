#!/usr/bin/env python3
import click

from semgrep.commands.login import login
from semgrep.commands.login import logout
from semgrep.commands.scan import scan
from semgrep.default_group import DefaultGroup


@click.group(cls=DefaultGroup, default_command="scan")
def cli() -> None:
    pass


cli.add_command(login)
cli.add_command(logout)
cli.add_command(scan)
