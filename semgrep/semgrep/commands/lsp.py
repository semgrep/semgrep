import logging
from typing import Optional

import click

from semgrep.commands.wrapper import handle_command_errors
from semgrep.constants import LSP_DEFAULT_CONFIG
from semgrep.lsp.server import run_server
from semgrep.lsp.utils.log import init_log


@click.command()
@click.option("--verbose", is_flag=True, help="Enable verbose logging")
@click.option("--debug", is_flag=True, help="Enable debug logging")
@click.option(
    "--logfile",
    "-l",
    type=click.Path(writable=True, exists=False, file_okay=True, dir_okay=False),
    required=False,
    help="Write logs to this file",
)
@click.option(
    "--config",
    type=str,
    default=LSP_DEFAULT_CONFIG,
    help="A Semgrep config for LSP mode",
)
@handle_command_errors
def lsp(
    verbose: Optional[bool], debug: Optional[bool], logfile: Optional[str], config: str
) -> None:
    """
    Start the Semgrep LSP server.
    """

    # Setup logging
    level = logging.WARNING
    if verbose:
        level = logging.INFO
    elif debug:
        level = logging.DEBUG
    # The LSP server uses stdin/stdout for json-rpc communication, so we
    # want to configure the root logger to use either stderr or a file.
    init_log("semgrep.lsp", level, logfile)

    print(f"Starting Semgrep LSP server ...")
    run_server(config)
